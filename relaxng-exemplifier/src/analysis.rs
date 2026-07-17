use relaxng_model::model::Pattern;
use std::collections::{HashMap, HashSet};

/// Pre-computed per-node metadata for schema patterns.
/// Stored for `Ref` nodes (keyed by `Rc` pointer); structural patterns are computed inline.
#[derive(Default)]
pub struct Analysis {
    ref_nullables: HashMap<usize, bool>,
    ref_min_fuels: HashMap<usize, usize>,
}

impl Analysis {
    pub fn new(start: &Pattern) -> Self {
        let mut analysis = Analysis::default();
        let mut visiting = HashSet::new();
        build_cache(start, &mut analysis, &mut visiting);
        analysis
    }

    /// Whether `pattern` can match the empty sequence.
    pub fn nullable(&self, pattern: &Pattern) -> bool {
        compute_nullable(pattern, &self.ref_nullables)
    }

    /// Minimum fuel units required to generate a valid instance of `pattern`.
    /// Returns 0 for nullable patterns. Uses `usize::MAX` for `NotAllowed`.
    pub fn min_fuel(&self, pattern: &Pattern) -> usize {
        compute_min_fuel(pattern, &self.ref_nullables, &self.ref_min_fuels)
    }
}

/// Walk the pattern tree, caching nullable and min_fuel for all reachable `Ref` nodes.
fn build_cache(pattern: &Pattern, analysis: &mut Analysis, visiting: &mut HashSet<usize>) {
    match pattern {
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if analysis.ref_nullables.contains_key(&ptr) || visiting.contains(&ptr) {
                return;
            }
            visiting.insert(ptr);
            let (nullable, min_fuel) = {
                let borrow = pat_ref.0.borrow();
                if let Some(rule) = borrow.as_ref() {
                    // Recursively cache sub-patterns first
                    build_cache(rule.pattern(), analysis, visiting);
                    let n = compute_nullable(rule.pattern(), &analysis.ref_nullables);
                    let mf = compute_min_fuel(
                        rule.pattern(),
                        &analysis.ref_nullables,
                        &analysis.ref_min_fuels,
                    );
                    (n, mf)
                } else {
                    (false, usize::MAX)
                }
            };
            visiting.remove(&ptr);
            analysis.ref_nullables.insert(ptr, nullable);
            analysis.ref_min_fuels.insert(ptr, min_fuel);
        }
        Pattern::Choice(branches, _) => {
            for b in branches {
                build_cache(b, analysis, visiting);
            }
        }
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => {
            for p in pats {
                build_cache(p, analysis, visiting);
            }
        }
        Pattern::OneOrMore(p, _)
        | Pattern::ZeroOrMore(p, _)
        | Pattern::Optional(p, _)
        | Pattern::Mixed(p, _)
        | Pattern::Element(_, p, _, _)
        | Pattern::Attribute(_, p, _, _)
        | Pattern::List(p, _) => build_cache(p, analysis, visiting),
        Pattern::DatatypeName {
            except: Some(e), ..
        } => build_cache(e, analysis, visiting),
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. }
        | Pattern::DatatypeName { except: None, .. } => {}
    }
}

pub(crate) fn compute_nullable(pattern: &Pattern, ref_nullables: &HashMap<usize, bool>) -> bool {
    match pattern {
        Pattern::Empty(_) | Pattern::Text(_) | Pattern::ZeroOrMore(..) | Pattern::Optional(..) => {
            true
        }
        Pattern::NotAllowed(_) => false,
        Pattern::Choice(branches, _) => branches.iter().any(|b| compute_nullable(b, ref_nullables)),
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => {
            pats.iter().all(|p| compute_nullable(p, ref_nullables))
        }
        Pattern::OneOrMore(p, _) | Pattern::Mixed(p, _) | Pattern::List(p, _) => {
            compute_nullable(p, ref_nullables)
        }
        Pattern::Element(..) | Pattern::Attribute(..) => false,
        Pattern::DatatypeValue { .. } | Pattern::DatatypeName { .. } => false,
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            *ref_nullables.get(&ptr).unwrap_or(&false)
        }
    }
}

pub(crate) fn compute_min_fuel(
    pattern: &Pattern,
    ref_nullables: &HashMap<usize, bool>,
    ref_min_fuels: &HashMap<usize, usize>,
) -> usize {
    match pattern {
        Pattern::Empty(_) | Pattern::Text(_) | Pattern::NotAllowed(_) => 0,
        Pattern::DatatypeValue { .. } | Pattern::DatatypeName { .. } => 0,
        Pattern::Optional(..) | Pattern::ZeroOrMore(..) => 0,
        Pattern::OneOrMore(p, _) | Pattern::Mixed(p, _) | Pattern::List(p, _) => {
            compute_min_fuel(p, ref_nullables, ref_min_fuels)
        }
        Pattern::Choice(branches, _) => branches
            .iter()
            .map(|b| compute_min_fuel(b, ref_nullables, ref_min_fuels))
            .min()
            .unwrap_or(0),
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => pats
            .iter()
            .map(|p| compute_min_fuel(p, ref_nullables, ref_min_fuels))
            .fold(0usize, |a, b| a.saturating_add(b)),
        Pattern::Element(_, p, _, _) => {
            1usize.saturating_add(compute_min_fuel(p, ref_nullables, ref_min_fuels))
        }
        Pattern::Attribute(_, p, _, _) => {
            1usize.saturating_add(compute_min_fuel(p, ref_nullables, ref_min_fuels))
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            // Cyclic refs not in cache get cost 1 to guarantee termination
            *ref_min_fuels.get(&ptr).unwrap_or(&1)
        }
    }
}
