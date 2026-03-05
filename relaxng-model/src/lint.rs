use crate::model::Pattern;
use std::collections::HashSet;

/// Lint warnings for patterns that are technically valid but likely unintended.
#[derive(Debug)]
pub enum LintWarning {
    /// A Choice branch is NotAllowed and can never match.
    DeadChoiceBranch { span: Option<codemap::Span> },
    /// A Group or Interleave contains NotAllowed, making the whole pattern dead.
    DeadComposite {
        kind: &'static str,
        span: Option<codemap::Span>,
    },
    /// Redundant nesting, e.g. Optional(Optional(x)).
    RedundantWrapping {
        outer: &'static str,
        inner: &'static str,
        span: Option<codemap::Span>,
    },
}

/// Walk a compiled pattern tree and collect lint warnings.
pub fn lint_pattern(pat: &Pattern) -> Vec<LintWarning> {
    let mut warnings = Vec::new();
    let mut seen = HashSet::new();
    lint_walk(pat, &mut warnings, &mut seen);
    warnings
}

fn lint_walk(pat: &Pattern, warnings: &mut Vec<LintWarning>, seen: &mut HashSet<usize>) {
    match pat {
        Pattern::Choice(children, _span) => {
            for child in children {
                if matches!(child, Pattern::NotAllowed(_)) {
                    let child_span = match child {
                        Pattern::NotAllowed(s) => *s,
                        _ => None,
                    };
                    warnings.push(LintWarning::DeadChoiceBranch { span: child_span });
                }
                lint_walk(child, warnings, seen);
            }
        }
        Pattern::Group(children, span) => {
            if children.iter().any(|c| matches!(c, Pattern::NotAllowed(_))) {
                warnings.push(LintWarning::DeadComposite {
                    kind: "group",
                    span: *span,
                });
            }
            for child in children {
                lint_walk(child, warnings, seen);
            }
        }
        Pattern::Interleave(children, span) => {
            if children.iter().any(|c| matches!(c, Pattern::NotAllowed(_))) {
                warnings.push(LintWarning::DeadComposite {
                    kind: "interleave",
                    span: *span,
                });
            }
            for child in children {
                lint_walk(child, warnings, seen);
            }
        }
        Pattern::Optional(inner, span) => {
            check_redundant_wrapping("optional", inner, *span, warnings);
            lint_walk(inner, warnings, seen);
        }
        Pattern::ZeroOrMore(inner, span) => {
            check_redundant_wrapping("zeroOrMore", inner, *span, warnings);
            lint_walk(inner, warnings, seen);
        }
        Pattern::OneOrMore(inner, span) => {
            check_redundant_wrapping("oneOrMore", inner, *span, warnings);
            lint_walk(inner, warnings, seen);
        }
        Pattern::Mixed(inner, _) | Pattern::List(inner, _) => {
            lint_walk(inner, warnings, seen);
        }
        Pattern::Element(_, inner, _, _) | Pattern::Attribute(_, inner, _, _) => {
            lint_walk(inner, warnings, seen);
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    lint_walk(rule.pattern(), warnings, seen);
                }
            }
        }
        Pattern::DatatypeName { except, .. } => {
            if let Some(e) = except {
                lint_walk(e, warnings, seen);
            }
        }
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. } => {}
    }
}

/// Check if a lint warning is a DeadChoiceBranch.
impl LintWarning {
    pub fn is_dead_choice_branch(&self) -> bool {
        matches!(self, LintWarning::DeadChoiceBranch { .. })
    }
    pub fn is_dead_composite(&self) -> bool {
        matches!(self, LintWarning::DeadComposite { .. })
    }
    pub fn is_redundant_wrapping(&self) -> bool {
        matches!(self, LintWarning::RedundantWrapping { .. })
    }
}

fn check_redundant_wrapping(
    outer: &'static str,
    inner: &Pattern,
    span: Option<codemap::Span>,
    warnings: &mut Vec<LintWarning>,
) {
    let inner_name = match inner {
        Pattern::Optional(_, _) => Some("optional"),
        Pattern::ZeroOrMore(_, _) => Some("zeroOrMore"),
        Pattern::OneOrMore(_, _) => Some("oneOrMore"),
        _ => None,
    };
    let redundant = match (outer, inner_name) {
        // optional(optional(x)) = optional(x)
        ("optional", Some("optional")) => true,
        // optional(zeroOrMore(x)) = zeroOrMore(x)
        ("optional", Some("zeroOrMore")) => true,
        // zeroOrMore(zeroOrMore(x)) = zeroOrMore(x)
        ("zeroOrMore", Some("zeroOrMore")) => true,
        // zeroOrMore(oneOrMore(x)) = zeroOrMore(x)
        ("zeroOrMore", Some("oneOrMore")) => true,
        // zeroOrMore(optional(x)) = zeroOrMore(x)
        ("zeroOrMore", Some("optional")) => true,
        // oneOrMore(zeroOrMore(x)) = zeroOrMore(x)
        ("oneOrMore", Some("zeroOrMore")) => true,
        _ => false,
    };
    if redundant {
        warnings.push(LintWarning::RedundantWrapping {
            outer,
            inner: inner_name.unwrap(),
            span,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{DefineRule, NameClass, PatRef};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn dummy_span() -> codemap::Span {
        let mut codemap = codemap::CodeMap::new();
        let file = codemap.add_file("dummy".to_string(), "x".to_string());
        file.span
    }

    fn empty() -> Pattern {
        Pattern::Empty(None)
    }

    fn not_allowed() -> Pattern {
        Pattern::NotAllowed(None)
    }

    fn text() -> Pattern {
        Pattern::Text(None)
    }

    #[test]
    fn no_warnings_for_clean_pattern() {
        let pat = Pattern::Choice(vec![empty(), text()], None);
        let warnings = lint_pattern(&pat);
        assert!(warnings.is_empty());
    }

    #[test]
    fn dead_choice_branch_detected() {
        let pat = Pattern::Choice(vec![text(), not_allowed()], None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_dead_choice_branch());
    }

    #[test]
    fn multiple_dead_choice_branches() {
        let pat = Pattern::Choice(vec![not_allowed(), text(), not_allowed()], None);
        let warnings = lint_pattern(&pat);
        let dead_count = warnings
            .iter()
            .filter(|w| w.is_dead_choice_branch())
            .count();
        assert_eq!(dead_count, 2);
    }

    #[test]
    fn dead_group_detected() {
        let pat = Pattern::Group(vec![text(), not_allowed()], None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_dead_composite());
        if let LintWarning::DeadComposite { kind, .. } = &warnings[0] {
            assert_eq!(*kind, "group");
        }
    }

    #[test]
    fn dead_interleave_detected() {
        let pat = Pattern::Interleave(vec![text(), not_allowed()], None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_dead_composite());
        if let LintWarning::DeadComposite { kind, .. } = &warnings[0] {
            assert_eq!(*kind, "interleave");
        }
    }

    #[test]
    fn redundant_optional_optional() {
        let inner = Pattern::Optional(Box::new(empty()), None);
        let pat = Pattern::Optional(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_redundant_wrapping());
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "optional");
            assert_eq!(*inner, "optional");
        }
    }

    #[test]
    fn redundant_optional_zero_or_more() {
        let inner = Pattern::ZeroOrMore(Box::new(text()), None);
        let pat = Pattern::Optional(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_redundant_wrapping());
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "optional");
            assert_eq!(*inner, "zeroOrMore");
        }
    }

    #[test]
    fn redundant_zero_or_more_zero_or_more() {
        let inner = Pattern::ZeroOrMore(Box::new(text()), None);
        let pat = Pattern::ZeroOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "zeroOrMore");
            assert_eq!(*inner, "zeroOrMore");
        }
    }

    #[test]
    fn redundant_zero_or_more_one_or_more() {
        let inner = Pattern::OneOrMore(Box::new(text()), None);
        let pat = Pattern::ZeroOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "zeroOrMore");
            assert_eq!(*inner, "oneOrMore");
        }
    }

    #[test]
    fn redundant_zero_or_more_optional() {
        let inner = Pattern::Optional(Box::new(text()), None);
        let pat = Pattern::ZeroOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "zeroOrMore");
            assert_eq!(*inner, "optional");
        }
    }

    #[test]
    fn redundant_one_or_more_zero_or_more() {
        let inner = Pattern::ZeroOrMore(Box::new(text()), None);
        let pat = Pattern::OneOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        if let LintWarning::RedundantWrapping { outer, inner, .. } = &warnings[0] {
            assert_eq!(*outer, "oneOrMore");
            assert_eq!(*inner, "zeroOrMore");
        }
    }

    #[test]
    fn no_redundant_one_or_more_optional() {
        // oneOrMore(optional(x)) is NOT redundant — it means "1+ of (x or nothing)"
        let inner = Pattern::Optional(Box::new(text()), None);
        let pat = Pattern::OneOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert!(warnings.is_empty());
    }

    #[test]
    fn no_redundant_one_or_more_one_or_more() {
        // oneOrMore(oneOrMore(x)) is actually equivalent to oneOrMore(x), but the simplification
        // rules don't flag it since it's arguable. If needed, add later.
        let inner = Pattern::OneOrMore(Box::new(text()), None);
        let pat = Pattern::OneOrMore(Box::new(inner), None);
        let warnings = lint_pattern(&pat);
        assert!(warnings.is_empty());
    }

    #[test]
    fn nested_warnings_through_element() {
        // Warnings inside elements should be found
        let choice = Pattern::Choice(vec![text(), not_allowed()], None);
        let pat = Pattern::Element(
            NameClass::AnyName { except: None },
            Box::new(choice),
            None,
            None,
        );
        let warnings = lint_pattern(&pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_dead_choice_branch());
    }

    #[test]
    fn follows_refs_once() {
        // Build a Ref that points to a pattern with a warning
        let span = dummy_span();
        let inner_pat = Pattern::Choice(vec![text(), not_allowed()], None);
        let define = DefineRule::AssignCombine(span, None, inner_pat);
        let pat_ref = Rc::new(RefCell::new(Some(define)));
        let ref_pat = Pattern::Ref(dummy_span(), "test".to_string(), PatRef(pat_ref));
        let warnings = lint_pattern(&ref_pat);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].is_dead_choice_branch());
    }

    #[test]
    fn handles_recursive_refs() {
        // Build a self-referencing Ref — lint should not infinite loop
        let pat_ref: Rc<RefCell<Option<DefineRule>>> = Rc::new(RefCell::new(None));
        let ref_pat = Pattern::Ref(dummy_span(), "rec".to_string(), PatRef(Rc::clone(&pat_ref)));
        let choice = Pattern::Choice(vec![text(), ref_pat], None);
        let define = DefineRule::AssignCombine(dummy_span(), None, choice);
        *pat_ref.borrow_mut() = Some(define);

        let top = Pattern::Ref(dummy_span(), "rec".to_string(), PatRef(pat_ref));
        // Should terminate without warnings (no NotAllowed in the choice)
        let warnings = lint_pattern(&top);
        assert!(warnings.is_empty());
    }
}
