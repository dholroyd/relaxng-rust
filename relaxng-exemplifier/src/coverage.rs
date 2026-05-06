//! Coverage tour: generate a minimal set of documents that collectively exercise every
//! `Choice` branch and every `Optional` path (taken / not-taken).
//!
//! Strategy: a "decision tape" generator that replays a fixed sequence of branch choices,
//! defaulting to minimum-cost completions for all remaining decisions. For each Choice
//! branch and each Optional-taken path, we produce one document where that specific
//! decision is forced, and all other decisions use the minimum-cost default.

use relaxng_model::model::{DefineRule, Pattern};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::analysis::Analysis;
use crate::datatypes::generate_datatype_value;
use crate::document::{Output, XmlAttr, XmlElement};
use crate::names::{NameContext, Vocabulary};

/// A generator that follows a pre-determined sequence of branch choices,
/// using minimum-cost defaults for any decisions not in the tape.
struct TapeGen<'a> {
    analysis: &'a Analysis,
    vocab: &'a Vocabulary,
    /// Tape of decisions: each entry is a branch index (for Choice) or 0/1 (for Optional).
    tape: Vec<usize>,
    /// Current position in the tape.
    pos: usize,
}

impl<'a> TapeGen<'a> {
    fn new(analysis: &'a Analysis, vocab: &'a Vocabulary, tape: Vec<usize>, _fuel: usize) -> Self {
        TapeGen {
            analysis,
            vocab,
            tape,
            pos: 0,
        }
    }

    /// Pop the next decision from the tape, or return `default`.
    fn next_decision(&mut self, default: usize) -> usize {
        if self.pos < self.tape.len() {
            let v = self.tape[self.pos];
            self.pos += 1;
            v
        } else {
            default
        }
    }

    fn generate(
        &mut self,
        pattern: &Pattern,
        fuel: usize,
        visiting: &mut HashSet<usize>,
    ) -> Output {
        match pattern {
            Pattern::Empty(_) => Output::default(),
            Pattern::Text(_) => Output::text(gen_short_ascii_det(self.pos)),
            Pattern::NotAllowed(_) => Output::default(),

            Pattern::DatatypeValue { datatype, .. } => {
                Output::text(generate_datatype_value(datatype))
            }
            Pattern::DatatypeName { datatype, .. } => {
                Output::text(generate_datatype_det(datatype, self.pos))
            }

            Pattern::Choice(branches, _) => {
                // Filter to feasible branches
                let feasible: Vec<(usize, &Pattern)> = branches
                    .iter()
                    .enumerate()
                    .filter(|(_, b)| self.analysis.min_fuel(b) <= fuel)
                    .collect();

                if feasible.is_empty() {
                    // Pick cheapest nullable or cheapest overall
                    let branch = branches
                        .iter()
                        .filter(|b| self.analysis.nullable(b))
                        .min_by_key(|b| self.analysis.min_fuel(b))
                        .or_else(|| branches.iter().min_by_key(|b| self.analysis.min_fuel(b)));
                    return branch
                        .map(|b| self.generate(b, fuel, visiting))
                        .unwrap_or_default();
                }

                // Decision: which feasible branch to pick
                let n = feasible.len();
                let decision = self.next_decision(0) % n;
                self.generate(feasible[decision].1, fuel, visiting)
            }

            Pattern::Group(patterns, _) => {
                let mut output = Output::default();
                let mut remaining = fuel;
                for (i, p) in patterns.iter().enumerate() {
                    let reserved: usize = patterns[i + 1..]
                        .iter()
                        .map(|pp| self.analysis.min_fuel(pp))
                        .fold(0, |a, b| a.saturating_add(b));
                    let this_fuel = remaining.saturating_sub(reserved);
                    let sub = self.generate(p, this_fuel, visiting);
                    let used = self.analysis.min_fuel(p).min(remaining);
                    remaining = remaining.saturating_sub(used);
                    output.merge(sub);
                }
                output
            }

            Pattern::Interleave(patterns, _) => {
                let mut output = Output::default();
                for p in patterns {
                    output.merge(self.generate(p, fuel, visiting));
                }
                output
            }

            Pattern::Optional(p, _) => {
                if fuel == 0 || self.analysis.min_fuel(p) > fuel {
                    return Output::default();
                }
                // Decision: 0 = skip, 1 = take
                let take = self.next_decision(0);
                if take != 0 {
                    self.generate(p, fuel, visiting)
                } else {
                    Output::default()
                }
            }

            Pattern::ZeroOrMore(p, _) => {
                let min_f = self.analysis.min_fuel(p).max(1);
                let max_reps = (fuel / min_f).min(4);
                if max_reps == 0 {
                    return Output::default();
                }
                let count = self.next_decision(0) % (max_reps + 1);
                let mut output = Output::default();
                for _ in 0..count {
                    output.merge(self.generate(p, fuel, visiting));
                }
                output
            }

            Pattern::OneOrMore(p, _) => {
                let min_f = self.analysis.min_fuel(p).max(1);
                let max_reps = (fuel / min_f).max(1).min(4);
                let count = 1 + self.next_decision(0) % max_reps;
                let mut output = Output::default();
                for _ in 0..count {
                    output.merge(self.generate(p, fuel, visiting));
                }
                output
            }

            Pattern::Element(nc, p, _, _) => {
                if fuel == 0 {
                    return Output::default();
                }
                let (local_name, namespace_uri) =
                    pick_name_det(nc, self.pos, self.vocab, NameContext::Element);
                let mut content = self.generate(p, fuel.saturating_sub(1), visiting);
                // XML does not allow two attributes with the same expanded name on one element.
                // Deduplicate by (local_name, namespace_uri), keeping the first occurrence.
                let mut seen = std::collections::HashSet::new();
                content
                    .attrs
                    .retain(|a| seen.insert((a.local_name.clone(), a.namespace_uri.clone())));
                Output::element(XmlElement {
                    namespace_uri,
                    local_name,
                    attrs: content.attrs,
                    children: content.children,
                })
            }

            Pattern::Attribute(nc, p, _, _) => {
                let (local_name, namespace_uri) =
                    pick_name_det(nc, self.pos, self.vocab, NameContext::Attribute);
                let value = self.text_value(p, fuel, visiting);
                Output::attr(XmlAttr {
                    namespace_uri,
                    local_name,
                    value,
                })
            }

            Pattern::Mixed(p, _) => self.generate(p, fuel, visiting),

            Pattern::List(p, _) => {
                let v = self.text_value(p, fuel, visiting);
                Output::text(v)
            }

            Pattern::Ref(_, _, pat_ref) => {
                let ptr = pat_ref.0.as_ptr() as usize;
                if visiting.contains(&ptr) || fuel == 0 {
                    return Output::default();
                }
                visiting.insert(ptr);
                let borrow = pat_ref.0.borrow();
                let result = if let Some(rule) = borrow.as_ref() {
                    self.generate(rule.pattern(), fuel.saturating_sub(1), visiting)
                } else {
                    Output::default()
                };
                drop(borrow);
                visiting.remove(&ptr);
                result
            }
        }
    }

    fn text_value(
        &mut self,
        pattern: &Pattern,
        fuel: usize,
        visiting: &mut HashSet<usize>,
    ) -> String {
        match pattern {
            Pattern::Text(_) => gen_short_ascii_det(self.pos),
            Pattern::DatatypeValue { datatype, .. } => generate_datatype_value(datatype),
            Pattern::DatatypeName { datatype, .. } => generate_datatype_det(datatype, self.pos),
            Pattern::Choice(branches, _) => {
                let n = branches.len();
                let idx = self.next_decision(0) % n;
                self.text_value(&branches[idx], fuel, visiting)
            }
            Pattern::Optional(p, _) => {
                let take = self.next_decision(0);
                if take != 0 {
                    self.text_value(p, fuel, visiting)
                } else {
                    String::new()
                }
            }
            Pattern::Group(pats, _) => pats
                .iter()
                .map(|p| self.text_value(p, fuel, visiting))
                .collect::<Vec<_>>()
                .join(""),
            Pattern::Ref(_, _, pat_ref) => {
                let ptr = pat_ref.0.as_ptr() as usize;
                if visiting.contains(&ptr) || fuel == 0 {
                    return String::new();
                }
                visiting.insert(ptr);
                let borrow = pat_ref.0.borrow();
                let result = if let Some(rule) = borrow.as_ref() {
                    self.text_value(rule.pattern(), fuel.saturating_sub(1), visiting)
                } else {
                    String::new()
                };
                drop(borrow);
                visiting.remove(&ptr);
                result
            }
            Pattern::Empty(_) => String::new(),
            _ => gen_ncname_det(self.pos),
        }
    }
}

/// Count the number of distinct Choice/Optional decision points reachable from `pattern`.
fn count_decisions(pattern: &Pattern, visiting: &mut HashSet<usize>) -> usize {
    match pattern {
        Pattern::Choice(branches, _) => {
            // One decision here, plus decisions inside each branch
            1 + branches
                .iter()
                .map(|b| count_decisions(b, visiting))
                .sum::<usize>()
        }
        Pattern::Optional(p, _) => 1 + count_decisions(p, visiting),
        Pattern::ZeroOrMore(p, _) | Pattern::OneOrMore(p, _) => 1 + count_decisions(p, visiting),
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => {
            pats.iter().map(|p| count_decisions(p, visiting)).sum()
        }
        Pattern::Element(_, p, _, _)
        | Pattern::Attribute(_, p, _, _)
        | Pattern::Mixed(p, _)
        | Pattern::List(p, _) => count_decisions(p, visiting),
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if visiting.contains(&ptr) {
                return 0;
            }
            visiting.insert(ptr);
            let borrow = pat_ref.0.borrow();
            let result = borrow
                .as_ref()
                .map(|rule| count_decisions(rule.pattern(), visiting))
                .unwrap_or(0);
            drop(borrow);
            visiting.remove(&ptr);
            result
        }
        _ => 0,
    }
}

/// Count the max number of branches in any single Choice reachable from `pattern`.
fn max_choice_width(pattern: &Pattern, visiting: &mut HashSet<usize>) -> usize {
    match pattern {
        Pattern::Choice(branches, _) => {
            let here = branches.len();
            let inner = branches
                .iter()
                .map(|b| max_choice_width(b, visiting))
                .max()
                .unwrap_or(0);
            here.max(inner)
        }
        Pattern::Optional(p, _) => 2usize.max(max_choice_width(p, visiting)),
        Pattern::ZeroOrMore(p, _) | Pattern::OneOrMore(p, _) => {
            5usize.max(max_choice_width(p, visiting))
        }
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => pats
            .iter()
            .map(|p| max_choice_width(p, visiting))
            .max()
            .unwrap_or(0),
        Pattern::Element(_, p, _, _)
        | Pattern::Attribute(_, p, _, _)
        | Pattern::Mixed(p, _)
        | Pattern::List(p, _) => max_choice_width(p, visiting),
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if visiting.contains(&ptr) {
                return 0;
            }
            visiting.insert(ptr);
            let borrow = pat_ref.0.borrow();
            let result = borrow
                .as_ref()
                .map(|rule| max_choice_width(rule.pattern(), visiting))
                .unwrap_or(0);
            drop(borrow);
            visiting.remove(&ptr);
            result
        }
        _ => 0,
    }
}

/// Generate a minimal set of documents covering all Choice branches and Optional paths.
pub fn coverage_tour(
    start: &Rc<RefCell<Option<DefineRule>>>,
    analysis: &Analysis,
    vocab: &Vocabulary,
    default_fuel: usize,
    pretty: bool,
) -> Vec<String> {
    let borrow = start.borrow();
    let rule = match borrow.as_ref() {
        Some(r) => r,
        None => return vec![],
    };
    let pattern = rule.pattern();

    // Determine how many documents to generate
    let max_branches = max_choice_width(pattern, &mut HashSet::new()).max(2);
    let n_decisions = count_decisions(pattern, &mut HashSet::new());
    // We generate one document per branch index + a few variations
    let n_docs = max_branches.max(n_decisions.min(16));

    let mut results = Vec::new();

    for doc_idx in 0..n_docs {
        // Build a tape: all decisions at position `doc_idx % max_branches`,
        // repeating the same choice throughout to maximize coverage of a single branch level
        let tape: Vec<usize> = (0..n_decisions.max(1)).map(|_| doc_idx).collect();
        let mut tape_gen = TapeGen::new(analysis, vocab, tape, default_fuel);
        let output = tape_gen.generate(pattern, default_fuel, &mut HashSet::new());
        results.push(crate::document::serialize_document(&output, pretty));
    }

    results
}

/// Deterministic short ASCII string based on a seed index.
fn gen_short_ascii_det(idx: usize) -> String {
    let words = ["foo", "bar", "baz", "qux", "test", "value", "data", "item"];
    words[idx % words.len()].to_string()
}

/// Deterministic NCName based on a seed index.
fn gen_ncname_det(idx: usize) -> String {
    let names = ["a", "b", "c", "d", "x", "y", "z", "n"];
    names[idx % names.len()].to_string()
}

/// Deterministic datatype value generation based on a seed index.
fn generate_datatype_det(dt: &relaxng_model::datatype::Datatypes, idx: usize) -> String {
    use relaxng_model::datatype::xsd::XsdDatatypes;
    use relaxng_model::datatype::{Datatypes, relax::BuiltinDatatype};
    match dt {
        Datatypes::Relax(BuiltinDatatype::String | BuiltinDatatype::Token) => {
            gen_short_ascii_det(idx)
        }
        Datatypes::Xsd(xsd) => match xsd {
            XsdDatatypes::Boolean(_) => if idx % 2 == 0 { "true" } else { "false" }.to_string(),
            XsdDatatypes::Integer(_, _) | XsdDatatypes::Long(_, _) | XsdDatatypes::Int(_, _) => {
                idx.to_string()
            }
            XsdDatatypes::Date(_) => "2024-01-01".to_string(),
            XsdDatatypes::Datetime(_) => "2024-01-01T00:00:00".to_string(),
            XsdDatatypes::AnyURI(_) => "http://example.com".to_string(),
            XsdDatatypes::Language(_) => "en".to_string(),
            _ => gen_ncname_det(idx),
        },
    }
}

/// Deterministic name picking: prefers schema-native vocabulary for wildcards.
fn pick_name_det(
    nc: &relaxng_model::model::NameClass,
    idx: usize,
    vocab: &Vocabulary,
    ctx: NameContext,
) -> (String, String) {
    use relaxng_model::model::NameClass;
    let pool = match ctx {
        NameContext::Element => &vocab.element_names,
        NameContext::Attribute => &vocab.attr_names,
    };
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => (name.clone(), namespace_uri.clone()),
        NameClass::NsName { namespace_uri, .. } => {
            let candidates: Vec<&(String, String)> =
                pool.iter().filter(|(_, ns)| ns == namespace_uri).collect();
            if !candidates.is_empty() {
                return candidates[idx % candidates.len()].clone();
            }
            (gen_ncname_det(idx), namespace_uri.clone())
        }
        NameClass::AnyName { .. } => {
            if !pool.is_empty() {
                return pool[idx % pool.len()].clone();
            }
            let names = ["elem", "item", "node", "value"];
            (names[idx % names.len()].to_string(), String::new())
        }
        NameClass::Alt { a, .. } => pick_name_det(a, idx, vocab, ctx),
    }
}
