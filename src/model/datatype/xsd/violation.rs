//! Describing why a value is not valid for an XSD datatype.

use super::{LengthFacet, PatternFacet};
#[cfg(feature = "validator")]
use super::{XsdDatatypes, parse_qname};
#[cfg(feature = "validator")]
use crate::model::datatype::reason::{DIGIT_COUNT, FRACTION_DIGIT_COUNT, Reason, quote, shorten};

/// Heads for the length of a value, in each unit, as given in [`Violation::Length`].  The unit
/// is a noun before the number, so that nothing need agree with the number in being singular or
/// plural.
pub(crate) const CHARACTER_COUNT: &str = "must have a character count of";
pub(crate) const ITEM_COUNT: &str = "must have an item count of";
pub(crate) const BYTE_COUNT: &str = "must have a byte count of";

/// The first check that a value failed, of those [`XsdDatatypes::parse_value`](super::XsdDatatypes::parse_value) makes, with the
/// facet it failed, borrowed from the datatype.
///
/// Values are rejected routinely while validating a valid document, by every alternative of a
/// choice but the one that matches, so this type needs to be cheap.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Violation<'s> {
    Lexical,
    /// Outside the range of the datatype itself, such as 300 for byte
    TypeRange,
    Pattern(&'s PatternFacet),
    Length {
        facet: LengthFacet,
        /// The head of a reason about the length, which says what it counts
        unit: &'static str,
        actual: usize,
    },
    /// Outside the lower bound if `min`, otherwise the upper bound, `bound`, as written in the
    /// schema
    Range {
        min: bool,
        bound: &'s str,
        inclusive: bool,
    },
    TotalDigits {
        max: u16,
        actual: usize,
    },
    FractionDigits {
        max: u16,
        actual: u64,
    },
}

#[cfg(feature = "validator")]
impl XsdDatatypes {
    /// Why `text` is not a valid value of this datatype, as a phrase such as `must be greater
    /// than or equal to 0`, or `None` if it is valid. `lookup_ns` resolves the prefix of a
    /// `QName`, as for [`Self::is_valid_with_ns`].
    pub(crate) fn explain_invalid(
        &self,
        text: &str,
        lookup_ns: impl Fn(&str) -> Option<String>,
    ) -> Option<Reason> {
        match self.parse_value(text) {
            Err(v) => Some(self.describe_violation(v)),
            // As in `is_valid_with_ns`
            Ok(_) => match (self, parse_qname(text)) {
                (XsdDatatypes::QName(_), Some((Some(prefix), _)))
                    if lookup_ns(prefix).is_none() =>
                {
                    Some(Reason::phrase(format!(
                        "uses the prefix {}, which is not declared",
                        quote(prefix)
                    )))
                }
                _ => None,
            },
        }
    }

    /// Describes `v`, which a value of this datatype failed.
    pub(crate) fn describe_violation(&self, v: Violation<'_>) -> Reason {
        let has = |actual: &dyn std::fmt::Display| format!("(it has {actual})");
        match v {
            Violation::Lexical => Reason::new("must be a valid", self.name()),
            Violation::TypeRange => Reason::new("must be within the range of", self.name()),
            Violation::Pattern(p) => Reason::new("must match the pattern", quote(&p.0)),
            Violation::Length {
                facet,
                unit,
                actual,
            } => describe_length(facet, unit, actual).noting(has(&actual)),
            Violation::Range {
                min,
                bound,
                inclusive,
            } => describe_range(min, bound, inclusive, self.is_temporal()),
            Violation::TotalDigits { max, actual } => {
                Reason::new(DIGIT_COUNT, format!("at most {max}")).noting(has(&actual))
            }
            Violation::FractionDigits { max: 0, actual } => {
                Reason::new(FRACTION_DIGIT_COUNT, "0").noting(has(&actual))
            }
            Violation::FractionDigits { max, actual } => {
                Reason::new(FRACTION_DIGIT_COUNT, format!("at most {max}")).noting(has(&actual))
            }
        }
    }

    /// Whether values are points in time, so are earlier or later, rather than less or greater
    fn is_temporal(&self) -> bool {
        matches!(
            self,
            XsdDatatypes::Datetime(..)
                | XsdDatatypes::Date(..)
                | XsdDatatypes::Time(..)
                | XsdDatatypes::GYearMonth(..)
                | XsdDatatypes::GYear(..)
                | XsdDatatypes::GMonthDay(..)
                | XsdDatatypes::GDay(..)
                | XsdDatatypes::GMonth(..)
        )
    }
}

/// Describes the lower bound if `min`, otherwise the upper bound, `bound`
#[cfg(feature = "validator")]
fn describe_range(min: bool, bound: &str, inclusive: bool, temporal: bool) -> Reason {
    // A bound can be any length, such as an integer with a thousand digits
    let b = shorten(bound);
    let item = match (temporal, min, inclusive) {
        (false, true, true) => format!("greater than or equal to {b}"),
        (false, true, false) => format!("greater than {b}"),
        (false, false, true) => format!("less than or equal to {b}"),
        (false, false, false) => format!("less than {b}"),
        (true, true, true) => format!("{b} or later"),
        (true, true, false) => format!("later than {b}"),
        (true, false, true) => format!("{b} or earlier"),
        (true, false, false) => format!("earlier than {b}"),
    };
    Reason::new("must be", item)
}

/// Describes the bound of `len` that a value of length `actual` violates.  Of minLength and
/// maxLength together, only the one violated is given, as for minInclusive and maxInclusive.
#[cfg(feature = "validator")]
fn describe_length(len: LengthFacet, head: &'static str, actual: usize) -> Reason {
    let item = match len {
        LengthFacet::Unbounded => unreachable!("an unbounded length is never violated"),
        LengthFacet::MinLength(min) => format!("at least {min}"),
        LengthFacet::MaxLength(max) => format!("at most {max}"),
        LengthFacet::MinMaxLength(min, _) if actual < min => format!("at least {min}"),
        LengthFacet::MinMaxLength(_, max) => format!("at most {max}"),
        LengthFacet::Length(len) => format!("exactly {len}"),
    };
    Reason::new(head, item)
}
