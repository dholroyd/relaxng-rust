//! Reasons that a value is not valid, and combining several of them into phrases for
//! diagnostics.
//!
//! A value that fails every alternative of a choice fails each for its own reason, and those
//! reasons are often alike: `must be a valid date`, `must be a valid gYear`, and so on.  So each
//! [`Reason`] is kept in parts, a head that alike reasons share and an item that is particular to
//! each, and [`render`] writes each group of reasons with the same head only once, as in `must be
//! a valid date or gYear`.  Every head comes from a fixed set of templates, never from the schema
//! or document, so however many alternatives a schema has, the number of groups stays small, and
//! only the items in a group need limiting.

/// The most items to give in one group of reasons
const MAX_ITEMS: usize = 4;

/// The most characters to show of text from the schema or the document, which may be any
/// length
const MAX_SHOWN_CHARS: usize = 40;

/// Why a value is not valid: `head item note`, such as `must have a character count of`
/// `at least 3` `(it has 2)`
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Reason {
    head: String,
    item: String,
    /// Said after the item, such as the value's own length, `(it has 2)`
    note: Option<String>,
    /// Whether the reason is itself a list of alternatives
    alternatives: bool,
}

/// Heads for the digits of a decimal value.  As for the length units in
/// [`Violation::Length`](crate::model::datatype::xsd::Violation::Length), the unit is a noun
/// before the number.
pub(crate) const DIGIT_COUNT: &str = "must have a digit count of";
pub(crate) const FRACTION_DIGIT_COUNT: &str = "must have a fraction digit count of";

impl Reason {
    /// A reason that can be combined with others of the same `head`, which must be a fixed
    /// phrase, not text from the schema or document
    pub(crate) fn new(head: &'static str, item: impl Into<String>) -> Reason {
        Reason {
            head: head.to_string(),
            item: item.into(),
            note: None,
            alternatives: false,
        }
    }

    /// A reason that is complete in itself.  Only identical phrases are combined, so `phrase`
    /// must not vary with anything unbounded, other than through [`quote`].
    pub(crate) fn phrase(phrase: String) -> Reason {
        Reason {
            head: phrase,
            item: String::new(),
            note: None,
            alternatives: false,
        }
    }

    /// As [`Self::phrase`], for `prefix` followed by `phrase`
    pub(crate) fn prefixed(prefix: String, phrase: Phrase) -> Reason {
        Reason {
            alternatives: phrase.alternatives,
            ..Reason::phrase(format!("{prefix} {}", phrase.text))
        }
    }

    /// Follows the item with `note`
    pub(crate) fn noting(mut self, note: String) -> Reason {
        self.note = Some(note);
        self
    }
}

/// Text for a diagnostic, and whether it is a list of alternatives joined with `or`, which
/// decides how it is joined with others
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Phrase {
    pub(crate) text: String,
    alternatives: bool,
}

/// Writes `reasons` as phrases, one for each group of reasons with the same head, in the order
/// the groups first appear.  Within a group, items with the same note share it, as in `must
/// have a character count of at least 5 or at most 2 (it has 3)`.
pub(crate) fn render(reasons: &[Reason]) -> Vec<Phrase> {
    let mut groups: Vec<(&str, Vec<&Reason>)> = vec![];
    for r in reasons {
        match groups.iter_mut().find(|(head, _)| *head == r.head) {
            Some((_, members)) if members.contains(&r) => {}
            Some((_, members)) => members.push(r),
            None => groups.push((&r.head, vec![r])),
        }
    }
    groups
        .into_iter()
        .map(|(head, members)| render_group(head, &members))
        .collect()
}

fn render_group(head: &str, members: &[&Reason]) -> Phrase {
    let shown = &members[..members.len().min(MAX_ITEMS)];
    let mut clusters: Vec<Vec<&Reason>> = vec![];
    for &r in shown {
        // Only items with a note have anything to share
        let same = |c: &&mut Vec<&Reason>| r.note.is_some() && c[0].note == r.note;
        match clusters.iter_mut().find(same) {
            Some(c) => c.push(r),
            None => clusters.push(vec![r]),
        }
    }
    let parts: Vec<Phrase> = clusters
        .iter()
        .map(|c| {
            let items: Vec<Phrase> = c
                .iter()
                .filter(|r| !r.item.is_empty())
                .map(|r| Phrase {
                    text: r.item.clone(),
                    alternatives: false,
                })
                .collect();
            let mut part = join_or_more(&items, 0);
            if let Some(note) = &c[0].note {
                part.text.push(' ');
                part.text.push_str(note);
            }
            part
        })
        .filter(|p| !p.text.is_empty())
        .collect();
    let body = join_or_more(&parts, members.len() - shown.len());
    let alternatives = body.alternatives || members.iter().any(|r| r.alternatives);
    let text = if body.text.is_empty() {
        head.to_string()
    } else {
        format!("{head} {}", body.text)
    };
    Phrase { text, alternatives }
}

/// `a`, `a or b`, or `a, b or c`
pub(crate) fn join_or(parts: &[Phrase]) -> Phrase {
    join_or_more(parts, 0)
}

/// As [`join_or`], adding that there are `more` parts not given.  Where a part has alternatives
/// of its own, the last is set off with a comma, so that it is clear which `or` is which.
fn join_or_more(parts: &[Phrase], more: usize) -> Phrase {
    let nested = parts.iter().any(|p| p.alternatives);
    let last_sep = if nested { ", or " } else { " or " };
    let mut text = String::new();
    for (i, part) in parts.iter().enumerate() {
        if i > 0 {
            text.push_str(if i + 1 == parts.len() && more == 0 {
                last_sep
            } else {
                ", "
            });
        }
        text.push_str(&part.text);
    }
    if more > 0 {
        text.push_str(last_sep);
        text.push_str(&format!("one of {more} more"));
    }
    Phrase {
        text,
        alternatives: nested || parts.len() > 1 || more > 0,
    }
}

/// `text` in quotes, as [`shorten`] shows it
pub(crate) fn quote(text: &str) -> String {
    format!("\"{}\"", shorten(text))
}

/// `text`, cut short after [`MAX_SHOWN_CHARS`] characters, and with characters that would
/// disturb a one-line message on a terminal (control characters, and those that reorder
/// bidirectional text) escaped
pub(crate) fn shorten(text: &str) -> String {
    let mut s = String::new();
    for (i, c) in text.chars().enumerate() {
        if i == MAX_SHOWN_CHARS {
            s.push('…');
            break;
        }
        match c {
            '\n' => s.push_str("\\n"),
            '\r' => s.push_str("\\r"),
            '\t' => s.push_str("\\t"),
            c if c.is_control()
                || matches!(c, '\u{200E}' | '\u{200F}' | '\u{202A}'..='\u{202E}' | '\u{2066}'..='\u{2069}') =>
            {
                s.push_str(&format!("\\u{{{:x}}}", c as u32));
            }
            c => s.push(c),
        }
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::datatype::xsd::{BYTE_COUNT, CHARACTER_COUNT};

    fn render_all(reasons: &[Reason]) -> String {
        join_or(&render(reasons)).text
    }

    #[test]
    fn groups_reasons_by_head() {
        let r = [
            Reason::new("must be a valid", "date"),
            Reason::new("must match the pattern", "\"x\""),
            Reason::new("must be a valid", "gYear"),
            Reason::new("must be a valid", "date"),
        ];
        assert_eq!(
            render_all(&r),
            r#"must be a valid date or gYear, or must match the pattern "x""#
        );
    }

    #[test]
    fn shares_notes() {
        let has = |n: usize| format!("(it has {n})");
        let r = [
            Reason::new(CHARACTER_COUNT, "at least 5").noting(has(3)),
            Reason::new(CHARACTER_COUNT, "at most 1").noting(has(3)),
            Reason::new(CHARACTER_COUNT, "at most 2").noting(has(2)),
            Reason::new(BYTE_COUNT, "exactly 1").noting(has(2)),
        ];
        assert_eq!(
            render_all(&r),
            "must have a character count of at least 5 or at most 1 (it has 3), \
             or at most 2 (it has 2), or must have a byte count of exactly 1 (it has 2)"
        );
    }

    #[test]
    fn limits_items_in_a_group() {
        let r: Vec<_> = (0..7)
            .map(|i| Reason::new("must be equal to", format!("\"{i}\"")))
            .collect();
        assert_eq!(
            render_all(&r),
            r#"must be equal to "0", "1", "2", "3" or one of 3 more"#
        );
    }

    #[test]
    fn phrases_combine_only_when_identical() {
        let r = [
            Reason::phrase("is not valid".to_string()),
            Reason::phrase("is not valid".to_string()),
            Reason::phrase("must have more list items".to_string()),
        ];
        assert_eq!(render_all(&r), "is not valid or must have more list items");
    }

    #[test]
    fn shortens_long_and_unprintable_text() {
        let long = "a".repeat(50);
        assert_eq!(quote(&long), format!("\"{}…\"", "a".repeat(40)));
        assert_eq!(quote("a\nb\u{202E}c\\d"), r#""a\nb\u{202e}c\d""#);
    }
}
