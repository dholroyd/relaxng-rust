//! Explaining why text content or an attribute value is not valid, for
//! [`ValidatorError::InvalidValue`](super::ValidatorError::InvalidValue).

use super::Explainer;
use super::infoset::NamespaceScope;
use super::schema::{DatatypeId, DatatypeValueId, Pat, PatId};
use crate::model::datatype::reason::{Phrase, Reason, join_or, quote, render};
use crate::xml_whitespace::split_xml_whitespace;
use std::collections::HashSet;

/// The patterns that text derived from some position could have matched: those that look at
/// the text's value.
#[derive(Default)]
struct Candidates {
    datatypes: Vec<DatatypeId>,
    values: Vec<DatatypeValueId>,
    excepts: Vec<(DatatypeId, PatId)>,
    lists: Vec<PatId>,
}

impl Explainer<'_> {
    /// Why `value` matches none of the datatypes the schema allows at `expected`, as phrases
    /// such as `must be greater than or equal to 0`: one for each kind of reason, covering
    /// every alternative it failed for that kind of reason.  Empty if no reason can be found.
    pub(super) fn value_reasons(&self, expected: PatId, value: &str) -> Vec<Phrase> {
        render(&self.value_reasons_from(&[expected], value))
    }

    /// As [`Self::value_reasons`], for text that could have matched at any of `positions`,
    /// before they are combined
    fn value_reasons_from(&self, positions: &[PatId], value: &str) -> Vec<Reason> {
        let mut c = Candidates::default();
        let mut seen = HashSet::new();
        for &pid in positions {
            self.candidates(pid, &mut seen, &mut c);
        }
        let mut reasons = vec![];
        let default_ns = self.ns.and_then(|ns| ns.default_namespace()).unwrap_or("");
        for &v in &c.values {
            let v = self.schema.datatype_value(v);
            let mut reason = Reason::new("must be equal to", quote(&v.describe()));
            // A QName can read the same as the schema's, but resolve to another name
            if let Some(actual) = v.describe_actual(value, default_ns, |p| self.lookup_prefix(p)) {
                reason = reason.noting(format!("(it is {})", quote(&actual)));
            }
            reasons.push(reason);
        }
        for &dt in &c.datatypes {
            reasons.extend(self.datatype_reason(dt, value));
        }
        for &(dt, except) in &c.excepts {
            match self.datatype_reason(dt, value) {
                Some(r) => reasons.push(r),
                None => reasons.push(self.except_reason(except, value)),
            }
        }
        if !c.lists.is_empty() {
            reasons.push(self.list_reason(&c.lists, value));
        }
        reasons
    }

    /// Collects the patterns that text derived from `pid` could match, following the same
    /// structure as `Schema::text_deriv`.
    fn candidates(&self, pid: PatId, seen: &mut HashSet<PatId>, c: &mut Candidates) {
        if !seen.insert(pid) {
            return;
        }
        match self.schema.patt(pid) {
            Pat::Choice(p1, p2, _) | Pat::Interleave(p1, p2, _) => {
                self.candidates(p1, seen, c);
                self.candidates(p2, seen, c);
            }
            Pat::Group(p1, p2, _) => {
                self.candidates(p1, seen, c);
                if self.schema.patt(p1).is_nullable() {
                    self.candidates(p2, seen, c);
                }
            }
            Pat::After(p, _, _) | Pat::OneOrMore(p, _) => self.candidates(p, seen, c),
            Pat::Datatype(dt) => c.datatypes.push(dt),
            Pat::DatatypeValue(v) => c.values.push(v),
            Pat::DatatypeExcept(dt, except) => c.excepts.push((dt, except)),
            Pat::List(p) => c.lists.push(p),
            Pat::Empty | Pat::Text | Pat::NotAllowed | Pat::Attribute(..) | Pat::Element(..) => {}
        }
    }

    /// Why `value` is not valid for the datatype, or `None` if it is
    fn datatype_reason(&self, dt: DatatypeId, value: &str) -> Option<Reason> {
        self.schema
            .datatype(dt)
            .explain_invalid(value, |p| self.lookup_prefix(p))
    }

    fn value_matches(&self, v: DatatypeValueId, value: &str) -> bool {
        let default_ns = self.ns.and_then(|ns| ns.default_namespace()).unwrap_or("");
        self.schema
            .datatype_value(v)
            .is_valid_with_ns(value, default_ns, |p| self.lookup_prefix(p))
    }

    fn lookup_prefix(&self, prefix: &str) -> Option<String> {
        self.ns
            .and_then(|ns| ns.lookup_prefix(prefix))
            .map(str::to_owned)
    }

    /// Why `value`, which is a valid value of the datatype, is excluded by `except`
    fn except_reason(&self, except: PatId, value: &str) -> Reason {
        let mut c = Candidates::default();
        self.candidates(except, &mut HashSet::new(), &mut c);
        match c.values.iter().find(|&&v| self.value_matches(v, value)) {
            Some(&v) => Reason::new(
                "must not be equal to",
                quote(&self.schema.datatype_value(v).describe()),
            ),
            None => Reason::phrase("is a value the schema excludes".to_string()),
        }
    }

    /// Why `value` matches none of the list patterns `lists` (the content of each).  Each list
    /// is derived by the value's items in turn, as validation does, and the reason comes from
    /// those that matched the most items before failing: the others went wrong earlier, so are
    /// less likely to be what the document meant.
    fn list_reason(&self, lists: &[PatId], value: &str) -> Reason {
        let fallback =
            || Reason::phrase("must have a different number or combination of list items".into());
        let Some(ns) = self.ns else {
            return fallback();
        };
        let items: Vec<&str> = split_xml_whitespace(value).collect();
        let mut furthest = 0;
        let mut positions = vec![];
        for &list in lists {
            let Some((matched, at)) = self.list_failure(list, &items, ns) else {
                continue;
            };
            if matched > furthest {
                furthest = matched;
                positions.clear();
            }
            if matched == furthest {
                positions.push(at);
            }
        }
        if positions.is_empty() {
            return fallback();
        }
        let Some(item) = items.get(furthest) else {
            return Reason::phrase(if items.is_empty() {
                "must have at least one list item".into()
            } else {
                "must have more list items".into()
            });
        };
        let reasons = self.value_reasons_from(&positions, item);
        if !reasons.is_empty() {
            Reason::prefixed(
                format!("list item {}", quote(item)),
                join_or(&render(&reasons)),
            )
        } else if let Some(prev) = furthest.checked_sub(1).map(|i| items[i]) {
            // Nothing may follow the items before this one
            Reason::phrase(format!("must end after list item {}", quote(prev)))
        } else {
            Reason::phrase("must have no list items".into())
        }
    }

    /// Derives the list content `list` by `items` in turn.  Returns how many items matched,
    /// and the pattern they reached, if an item then fails or the list is incomplete once all
    /// have matched; `None` if the items match the list.
    fn list_failure(
        &self,
        list: PatId,
        items: &[&str],
        ns: &NamespaceScope<'_>,
    ) -> Option<(usize, PatId)> {
        let mut at = list;
        for (i, item) in items.iter().enumerate() {
            let d = self.schema.text_deriv(at, item, ns).ok()?;
            if let Pat::NotAllowed = self.schema.patt(d) {
                return Some((i, at));
            }
            at = d;
        }
        (!self.schema.patt(at).is_nullable()).then_some((items.len(), at))
    }
}
