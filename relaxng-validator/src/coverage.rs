use crate::nameclass::{describe_datatype, describe_datatype_value, describe_nameclass};
use crate::schema::{Pat, Schema};

/// Description of a trackable pattern in the schema.
pub struct TrackablePattern {
    /// Index of this pattern in the schema's pattern arena.
    pub pat_id: u16,
    /// Kind of pattern.
    pub kind: &'static str,
    /// Human-readable name or description.
    pub name: String,
    /// Source spans where this pattern was defined.
    pub spans: Vec<codemap::Span>,
}

/// Coverage report tracking which schema patterns were exercised during validation.
pub struct CoverageReport {
    covered: Box<[u64]>,
    patterns: Vec<TrackablePattern>,
}

impl CoverageReport {
    /// Check whether a specific pattern (by arena index) was covered.
    pub fn is_covered(&self, pat_id: u16) -> bool {
        let idx = pat_id as usize;
        idx < self.covered.len() * 64 && (self.covered[idx / 64] >> (idx % 64)) & 1 != 0
    }

    /// Merge another report into this one (bitwise OR). Use this to aggregate
    /// coverage across multiple document validations against the same schema.
    pub fn merge(&mut self, other: &CoverageReport) {
        for (a, b) in self.covered.iter_mut().zip(other.covered.iter()) {
            *a |= *b;
        }
    }

    /// Number of trackable patterns that were covered.
    pub fn covered_count(&self) -> usize {
        self.patterns
            .iter()
            .filter(|p| self.is_covered(p.pat_id))
            .count()
    }

    /// Total number of trackable patterns in the schema.
    pub fn total_trackable(&self) -> usize {
        self.patterns.len()
    }

    /// All trackable patterns.
    pub fn patterns(&self) -> &[TrackablePattern] {
        &self.patterns
    }

    /// Iterator over patterns that were NOT covered.
    pub fn uncovered_patterns(&self) -> impl Iterator<Item = &TrackablePattern> {
        self.patterns.iter().filter(|p| !self.is_covered(p.pat_id))
    }
}

impl Schema {
    /// Build a coverage report from the current schema state.
    /// Returns `None` if coverage tracking was not enabled.
    pub(crate) fn build_coverage_report(&self) -> Option<CoverageReport> {
        let covered = self.coverage.as_ref()?.clone();
        let inner = self.inner.borrow();
        let count = self.compile_time_count as usize;
        let mut patterns = Vec::new();
        for i in 0..count {
            let pat = &inner.patterns[i];
            // Skip resolved-Ref duplicates: if the memo maps this pattern to
            // a different (canonical) PatId, this slot is a placeholder copy.
            if inner.memo.get(pat).is_some_and(|c| c.0 as usize != i) {
                continue;
            }
            let (kind, name) = match pat {
                Pat::Element(nc, _) => {
                    let mut desc = String::new();
                    describe_nameclass(nc, &mut desc);
                    ("Element", desc)
                }
                Pat::Attribute(nc, _) => {
                    let mut desc = String::new();
                    describe_nameclass(nc, &mut desc);
                    ("Attribute", desc)
                }
                Pat::Datatype(dt) => ("Datatype", describe_datatype(dt)),
                Pat::DatatypeValue(dt) => ("DatatypeValue", describe_datatype_value(dt)),
                Pat::DatatypeExcept(dt, _) => ("DatatypeExcept", describe_datatype(dt)),
                Pat::Text => {
                    // The Text sentinel is always pre-populated; only report it
                    // if the schema actually references it (has source spans).
                    if !inner.span_map.contains_key(&(i as u16)) {
                        continue;
                    }
                    ("Text", "text".to_string())
                }
                _ => continue,
            };
            let spans = inner.span_map.get(&(i as u16)).cloned().unwrap_or_default();
            patterns.push(TrackablePattern {
                pat_id: i as u16,
                kind,
                name,
                spans,
            });
        }
        Some(CoverageReport { covered, patterns })
    }
}
