use crate::Context;
use crate::datatype::relax::normalize_whitespace;
use lazy_static::lazy_static;
use relaxng_syntax::types;
use relaxng_syntax::types::DatatypeName;
use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

pub const NAMESPACE_URI: &str = "http://www.w3.org/2001/XMLSchema-datatypes";

/// Wrapper for finite floats that implements Eq and Hash.
/// Only constructed from validated finite values.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct FiniteF64(f64);
impl Eq for FiniteF64 {}
impl std::hash::Hash for FiniteF64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct FiniteF32(f32);
impl Eq for FiniteF32 {}
impl std::hash::Hash for FiniteF32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum XsdDatatypeValues {
    String(String),
    Token(String),
    Name(String),
    QName(QNameVal),
}

impl XsdDatatypeValues {
    /// Validate a value with namespace context for QName resolution.
    /// `default_ns` is the default namespace for unprefixed names in the document.
    /// `lookup_ns` resolves prefixes to namespace URIs in the document context.
    pub fn is_valid_with_ns(
        &self,
        value: &str,
        default_ns: &str,
        lookup_ns: impl Fn(&str) -> Option<String>,
    ) -> bool {
        match self {
            XsdDatatypeValues::String(s) => s == value,
            XsdDatatypeValues::Token(s) => s == &normalize_whitespace(value),
            XsdDatatypeValues::Name(s) => {
                let normalized = normalize_whitespace(value);
                is_valid_name(&normalized) && s == &normalized
            }
            XsdDatatypeValues::QName(v) => QNameVal::resolve(value, default_ns, lookup_ns)
                .is_some_and(|resolved| &resolved == v),
        }
    }
}

impl super::Datatype for XsdDatatypeValues {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            XsdDatatypeValues::String(s) => s == value,
            XsdDatatypeValues::Token(s) => s == &normalize_whitespace(value),
            XsdDatatypeValues::Name(s) => {
                let normalized = normalize_whitespace(value);
                is_valid_name(&normalized) && s == &normalized
            }
            XsdDatatypeValues::QName(_) => {
                // QName comparison requires namespace context; fall back to false
                // when called without context. Use is_valid_with_ns instead.
                false
            }
        }
    }
}

lazy_static! {
    static ref LANG_RE: regex::Regex = regex::Regex::new(r"^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$").unwrap();
    static ref DATETIME_RE: regex::Regex = regex::Regex::new(r"^-?\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:Z(?:(?:\+|-)\d{2}:\d{2})?)?$").unwrap();
    static ref DURATION_RE: regex::Regex = regex::Regex::new(r"^P(([0-9]+([.,][0-9]*)?Y)?([0-9]+([.,][0-9]*)?M)?([0-9]+([.,][0-9]*)?D)?T?([0-9]+([.,][0-9]*)?H)?([0-9]+([.,][0-9]*)?M)?([0-9]+([.,][0-9]*)?S)?)|\d{4}-?(0[1-9]|11|12)-?(?:[0-2]\d|30|31)T((?:[0-1][0-9]|[2][0-3]):?(?:[0-5][0-9]):?(?:[0-5][0-9]|60)|2400|24:00)$").unwrap();
    static ref TIME_RE: regex::Regex = regex::Regex::new(r"^\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref GYEARMONTH_RE: regex::Regex = regex::Regex::new(r"^-?\d{4,}-\d{2}(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref GYEAR_RE: regex::Regex = regex::Regex::new(r"^-?\d{4,}(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref GMONTHDAY_RE: regex::Regex = regex::Regex::new(r"^--\d{2}-\d{2}(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref GDAY_RE: regex::Regex = regex::Regex::new(r"^---\d{2}(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref GMONTH_RE: regex::Regex = regex::Regex::new(r"^--\d{2}(Z|[+-]\d{2}:\d{2})?$").unwrap();
    static ref HEX_RE: regex::Regex = regex::Regex::new(r"^([0-9a-fA-F]{2})*$").unwrap();
    static ref BASE64_RE: regex::Regex = regex::Regex::new(r"^[A-Za-z0-9+/\s]*={0,2}$").unwrap();
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum XsdDatatypes {
    NormalizedString(StringFacets),
    String(StringFacets),
    Byte(MinMaxFacet<i8>, Option<PatternFacet>),
    Short(MinMaxFacet<i16>, Option<PatternFacet>),
    UnsignedByte(MinMaxFacet<u8>, Option<PatternFacet>),
    UnsignedShort(MinMaxFacet<u16>, Option<PatternFacet>),
    Long(MinMaxFacet<i64>, Option<PatternFacet>),
    Int(MinMaxFacet<i32>, Option<PatternFacet>),
    Integer(MinMaxFacet<num_bigint::BigInt>, Option<PatternFacet>),
    PositiveInteger(MinMaxFacet<num_bigint::BigUint>, Option<PatternFacet>),
    NonNegativeInteger(MinMaxFacet<num_bigint::BigUint>, Option<PatternFacet>),
    NonPositiveInteger(MinMaxFacet<num_bigint::BigInt>, Option<PatternFacet>),
    NegativeInteger(MinMaxFacet<num_bigint::BigInt>, Option<PatternFacet>),
    UnsignedInt(MinMaxFacet<u32>, Option<PatternFacet>),
    UnsignedLong(MinMaxFacet<u64>, Option<PatternFacet>),
    Decimal {
        min_max: MinMaxFacet<bigdecimal::BigDecimal>,
        pattern: Option<PatternFacet>,
        fraction_digits: Option<u16>,
        total_digits: Option<u16>,
    },
    Float(MinMaxFacet<FiniteF32>, Option<PatternFacet>),
    Double(MinMaxFacet<FiniteF64>, Option<PatternFacet>),
    NmTokens(LengthFacet, Option<PatternFacet>),
    NmToken(StringFacets),
    NcName(StringFacets),
    Name(StringFacets),
    Token(StringFacets),
    Duration(Option<PatternFacet>),
    Date(Option<PatternFacet>),
    Datetime(Option<PatternFacet>),
    Time(Option<PatternFacet>),
    GYearMonth(Option<PatternFacet>),
    GYear(Option<PatternFacet>),
    GMonthDay(Option<PatternFacet>),
    GDay(Option<PatternFacet>),
    GMonth(Option<PatternFacet>),
    HexBinary(LengthFacet, Option<PatternFacet>),
    Base64Binary(LengthFacet, Option<PatternFacet>),
    AnyURI(StringFacets),
    Language(StringFacets),
    Boolean(Option<PatternFacet>),
    Id(StringFacets),
    IdRef(StringFacets),
    IdRefs(LengthFacet, Option<PatternFacet>),
    Entity(StringFacets),
    Entities(LengthFacet, Option<PatternFacet>),
}
impl super::Datatype for XsdDatatypes {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            XsdDatatypes::NormalizedString(str_facets) => {
                let normal_val = super::relax::normalize_whitespace(value);
                str_facets.is_valid(&normal_val)
            }
            XsdDatatypes::String(str_facets) => str_facets.is_valid(value),
            XsdDatatypes::Byte(min_max, patt) => {
                i8::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Short(min_max, patt) => {
                i16::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedByte(min_max, patt) => {
                u8::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedShort(min_max, patt) => {
                u16::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Long(min_max, patt) => {
                i64::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Int(min_max, patt) => {
                i32::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Integer(min_max, patt) => {
                num_bigint::BigInt::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::PositiveInteger(min_max, patt) => {
                num_bigint::BigUint::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NonNegativeInteger(min_max, patt) => {
                num_bigint::BigUint::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NonPositiveInteger(min_max, patt) => {
                num_bigint::BigInt::from_str(value)
                    .ok()
                    .is_some_and(|v| v <= num_bigint::BigInt::from(0) && min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NegativeInteger(min_max, patt) => {
                num_bigint::BigInt::from_str(value)
                    .ok()
                    .is_some_and(|v| v < num_bigint::BigInt::from(0) && min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Decimal {
                min_max,
                pattern: pat,
                fraction_digits,
                total_digits,
            } => {
                bigdecimal::BigDecimal::from_str(value)
                    .ok()
                    .is_some_and(|v| {
                        if !min_max.is_valid(&v) {
                            return false;
                        }
                        if let Some(td) = total_digits {
                            // totalDigits counts significant digits (ignoring sign, decimal point, leading zeros)
                            let stripped = value.trim_start_matches('-').trim_start_matches('0');
                            let sig_digits: usize =
                                stripped.chars().filter(|c| c.is_ascii_digit()).count();
                            let sig_digits = if sig_digits == 0 { 1 } else { sig_digits };
                            if sig_digits > *td as usize {
                                return false;
                            }
                        }
                        if let Some(fd) = fraction_digits {
                            let (_digits, scale) = v.as_bigint_and_exponent();
                            let actual_frac = if scale > 0 { scale as usize } else { 0 };
                            if actual_frac > *fd as usize {
                                return false;
                            }
                        }
                        true
                    })
                    && pat.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NmTokens(len, patt) => {
                let tokens: Vec<&str> = value.split_whitespace().collect();
                !tokens.is_empty()
                    && tokens.iter().all(|t| is_valid_nmtoken(t))
                    && len.is_valid_count(tokens.len())
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NmToken(str_facets) => {
                is_valid_nmtoken(value) && str_facets.is_valid(value)
            }
            XsdDatatypes::NcName(str_facets) => {
                is_valid_ncname(value) && str_facets.is_valid(value)
            }
            XsdDatatypes::Name(str_facets) => is_valid_name(value) && str_facets.is_valid(value),
            XsdDatatypes::Token(str_facets) => {
                let normalized = super::relax::normalize_whitespace(value);
                str_facets.is_valid(&normalized)
            }
            XsdDatatypes::Duration(patt) => {
                DURATION_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Date(patt) => {
                chrono::NaiveDate::parse_from_str(value, "%Y-%m-%d").is_ok()
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Datetime(patt) => {
                DATETIME_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Time(patt) => {
                TIME_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::GYearMonth(patt) => {
                GYEARMONTH_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::GYear(patt) => {
                GYEAR_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::GMonthDay(patt) => {
                GMONTHDAY_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::GDay(patt) => {
                GDAY_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::GMonth(patt) => {
                GMONTH_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::HexBinary(len, patt) => {
                HEX_RE.is_match(value)
                    && len.is_valid_count(value.len() / 2)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Base64Binary(len, patt) => {
                let stripped: std::string::String =
                    value.chars().filter(|c| !c.is_whitespace()).collect();
                if !BASE64_RE.is_match(&stripped) {
                    return false;
                }
                // Count decoded bytes: each 4 base64 chars = 3 bytes, minus padding
                let padding = stripped.chars().rev().take_while(|&c| c == '=').count();
                let data_chars = stripped.len() - padding;
                let decoded_len = if stripped.is_empty() {
                    0
                } else {
                    (data_chars * 3) / 4
                };
                len.is_valid_count(decoded_len)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Float(min_max, patt) => {
                value
                    .parse::<f32>()
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&FiniteF32(v)))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Double(min_max, patt) => {
                value
                    .parse::<f64>()
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&FiniteF64(v)))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::AnyURI(str_facets) => {
                uriparse::URIReference::try_from(value).is_ok() && str_facets.is_valid(value)
            }
            XsdDatatypes::Language(str_facets) => {
                LANG_RE.is_match(value) && str_facets.is_valid(value)
            }
            XsdDatatypes::Boolean(patt) => {
                (value == "true" || value == "false" || value == "1" || value == "0")
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedInt(min_max, patt) => {
                u32::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedLong(min_max, patt) => {
                u64::from_str(value)
                    .ok()
                    .is_some_and(|v| min_max.is_valid(&v))
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Id(str_facets) => is_valid_ncname(value) && str_facets.is_valid(value),
            XsdDatatypes::IdRef(str_facets) => is_valid_ncname(value) && str_facets.is_valid(value),
            XsdDatatypes::IdRefs(len, patt) => {
                let tokens: Vec<&str> = value.split_whitespace().collect();
                !tokens.is_empty()
                    && tokens.iter().all(|t| is_valid_ncname(t))
                    && len.is_valid_count(tokens.len())
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Entity(str_facets) => {
                is_valid_ncname(value) && str_facets.is_valid(value)
            }
            XsdDatatypes::Entities(len, patt) => {
                let tokens: Vec<&str> = value.split_whitespace().collect();
                !tokens.is_empty()
                    && tokens.iter().all(|t| is_valid_ncname(t))
                    && len.is_valid_count(tokens.len())
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
        }
    }
}

fn is_name_start_char(c: char) -> bool {
    matches!(c,
        ':' | 'A'..='Z' | '_' | 'a'..='z'
        | '\u{C0}'..='\u{D6}' | '\u{D8}'..='\u{F6}' | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}' | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}' | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}' | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}' | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}'
    )
}

fn is_name_char(c: char) -> bool {
    is_name_start_char(c)
        || matches!(c, '-' | '.' | '0'..='9' | '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
}

fn is_valid_nmtoken(text: &str) -> bool {
    !text.is_empty() && text.chars().all(is_name_char)
}

fn is_valid_name(text: &str) -> bool {
    let mut chars = text.chars();
    match chars.next() {
        Some(c) if is_name_start_char(c) => chars.all(is_name_char),
        _ => false,
    }
}

fn is_valid_ncname(text: &str) -> bool {
    match relaxng_syntax::compact::nc_name(relaxng_syntax::compact::Span::new(text)) {
        Ok((rest, _name)) => rest.fragment().is_empty(),
        Err(_) => false,
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct StringFacets {
    len: LengthFacet,
    pattern: Option<PatternFacet>,
}
impl StringFacets {
    fn is_valid(&self, value: &str) -> bool {
        self.len.is_valid(value)
            && if let Some(ref pat) = self.pattern {
                pat.is_valid(value)
            } else {
                true
            }
    }

    pub fn bounded(&self) -> bool {
        !matches!(self.len, LengthFacet::Unbounded)
    }

    pub fn min_len(&self) -> Option<usize> {
        match self.len {
            LengthFacet::Unbounded => None,
            LengthFacet::MinLength(min) => Some(min),
            LengthFacet::MaxLength(_) => None,
            LengthFacet::MinMaxLength(min, _) => Some(min),
            LengthFacet::Length(len) => Some(len),
        }
    }

    pub fn max_len(&self) -> Option<usize> {
        match self.len {
            LengthFacet::Unbounded => None,
            LengthFacet::MinLength(_) => None,
            LengthFacet::MaxLength(max) => Some(max),
            LengthFacet::MinMaxLength(_, max) => Some(max),
            LengthFacet::Length(len) => Some(len),
        }
    }

    pub fn regex(&self) -> Option<&regex::Regex> {
        self.pattern.as_ref().map(|pat| &pat.1)
    }
}

#[derive(Debug)]
pub enum XsdDatatypeError {
    Facet {
        type_name: &'static str,
        facet: FacetError,
    },
    UnsupportedDatatype {
        span: codemap::Span,
        name: String,
    },
    InvalidValueOfType {
        span: codemap::Span,
        type_name: &'static str,
    },
}
#[derive(Debug)]
pub enum FacetError {
    ConflictingFacet(&'static str),
    InvalidInt(codemap::Span, String),
    InvalidFloat(codemap::Span, String),
    InvalidPattern(codemap::Span, regex::Error),
    InvalidFacet(codemap::Span, String),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum LengthFacet {
    Unbounded,
    MinLength(usize),
    MaxLength(usize),
    MinMaxLength(usize, usize),
    Length(usize),
}
impl LengthFacet {
    fn is_valid(&self, value: &str) -> bool {
        self.is_valid_count(value.chars().count())
    }

    fn is_valid_count(&self, actual: usize) -> bool {
        match self {
            LengthFacet::Unbounded => true,
            LengthFacet::MinLength(min) => *min <= actual,
            LengthFacet::MaxLength(max) => actual <= *max,
            LengthFacet::MinMaxLength(min, max) => *min <= actual && actual <= *max,
            LengthFacet::Length(len) => actual == *len,
        }
    }

    fn merge(&mut self, other: LengthFacet) -> Result<(), FacetError> {
        *self = match self {
            LengthFacet::Unbounded => other,
            LengthFacet::MinLength(min) => match other {
                LengthFacet::Unbounded | LengthFacet::MinMaxLength(_, _) => unreachable!(),
                LengthFacet::MinLength(_min) => {
                    return Err(FacetError::ConflictingFacet("minLength"));
                }
                LengthFacet::MaxLength(max) => {
                    if *min > max {
                        return Err(FacetError::ConflictingFacet(
                            "minLength greater than maxLength",
                        ));
                    }
                    LengthFacet::MinMaxLength(*min, max)
                }
                LengthFacet::Length(_) => return Err(FacetError::ConflictingFacet("length")),
            },
            LengthFacet::MaxLength(max) => match other {
                LengthFacet::Unbounded | LengthFacet::MinMaxLength(_, _) => unreachable!(),
                LengthFacet::MaxLength(_max) => {
                    return Err(FacetError::ConflictingFacet("maxLength"));
                }
                LengthFacet::MinLength(min) => {
                    if min > *max {
                        return Err(FacetError::ConflictingFacet(
                            "minLength greater than maxLength",
                        ));
                    }
                    LengthFacet::MinMaxLength(min, *max)
                }
                LengthFacet::Length(_) => return Err(FacetError::ConflictingFacet("length")),
            },
            LengthFacet::MinMaxLength(_, _) => {
                return Err(FacetError::ConflictingFacet("conflicting length facet"));
            }
            LengthFacet::Length(_) => {
                return Err(FacetError::ConflictingFacet("conflicting length facet"));
            }
        };
        Ok(())
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Min<T: PartialOrd> {
    Unbounded,
    Inclusive(T),
    Exclusive(T),
}
impl<T: PartialOrd> Min<T> {
    fn is_valid(&self, v: &T) -> bool {
        match self {
            Min::Unbounded => true,
            Min::Inclusive(min) => min <= v,
            Min::Exclusive(min) => min < v,
        }
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Max<T: PartialOrd> {
    Unbounded,
    Inclusive(T),
    Exclusive(T),
}
impl<T: PartialOrd> Max<T> {
    fn is_valid(&self, v: &T) -> bool {
        match self {
            Max::Unbounded => true,
            Max::Inclusive(max) => v <= max,
            Max::Exclusive(max) => v < max,
        }
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct MinMaxFacet<T: PartialOrd> {
    min: Min<T>,
    max: Max<T>,
}
impl<T: PartialOrd> Default for MinMaxFacet<T> {
    fn default() -> Self {
        MinMaxFacet {
            min: Min::Unbounded,
            max: Max::Unbounded,
        }
    }
}

impl<T> MinMaxFacet<T>
where
    T: PartialOrd + Copy + std::ops::Add<Output = T> + From<u8>,
{
    // return the min inclusive value
    pub fn min(&self) -> Option<T> {
        match &self.min {
            Min::Unbounded => None,
            Min::Inclusive(min) => Some(*min),
            Min::Exclusive(min) => Some(*min + T::from(1)),
        }
    }
}

impl<T> MinMaxFacet<T>
where
    T: PartialOrd + Copy + std::ops::Sub<Output = T> + From<u8>,
{
    // return the max inclusive value
    pub fn max(&self) -> Option<T> {
        match &self.max {
            Max::Unbounded => None,
            Max::Inclusive(max) => Some(*max),
            Max::Exclusive(max) => Some(*max - T::from(1)),
        }
    }
}

impl<T> MinMaxFacet<T>
where
    T: PartialOrd + Clone + std::ops::Add<Output = T> + From<u8>,
{
    /// Return the min inclusive value (for types that implement Clone but not Copy)
    pub fn min_cloned(&self) -> Option<T> {
        match &self.min {
            Min::Unbounded => None,
            Min::Inclusive(min) => Some(min.clone()),
            Min::Exclusive(min) => Some(min.clone() + T::from(1)),
        }
    }
}

impl<T> MinMaxFacet<T>
where
    T: PartialOrd + Clone + std::ops::Sub<Output = T> + From<u8>,
{
    /// Return the max inclusive value (for types that implement Clone but not Copy)
    pub fn max_cloned(&self) -> Option<T> {
        match &self.max {
            Max::Unbounded => None,
            Max::Inclusive(max) => Some(max.clone()),
            Max::Exclusive(max) => Some(max.clone() - T::from(1)),
        }
    }
}

impl<T> MinMaxFacet<T>
where
    T: PartialOrd,
{
    pub fn bounded(&self) -> bool {
        !matches!((&self.min, &self.max), (Min::Unbounded, Max::Unbounded))
    }

    fn min_inclusive(&mut self, val: T) -> Result<(), FacetError> {
        match &self.max {
            Max::Unbounded => {}
            Max::Inclusive(max) => {
                if val > *max {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxInclusive",
                    ));
                }
            }
            Max::Exclusive(max) => {
                if val >= *max {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxExclusive",
                    ));
                }
            }
        }
        self.min = match self.min {
            Min::Unbounded => Min::Inclusive(val),
            Min::Inclusive(_) => unreachable!(),
            Min::Exclusive(_) => {
                return Err(FacetError::ConflictingFacet(
                    "minInclusive conflicts with minExclusive",
                ));
            }
        };
        Ok(())
    }
    fn min_exclusive(&mut self, val: T) -> Result<(), FacetError> {
        match &self.max {
            Max::Unbounded => {}
            Max::Inclusive(max) => {
                if val > *max {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxInclusive",
                    ));
                }
            }
            Max::Exclusive(max) => {
                if val >= *max {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxExclusive",
                    ));
                }
            }
        }
        self.min = match self.min {
            Min::Unbounded => Min::Exclusive(val),
            Min::Inclusive(_) => {
                return Err(FacetError::ConflictingFacet(
                    "minExclusive conflicts with minInclusive",
                ));
            }
            Min::Exclusive(_) => unreachable!(),
        };
        Ok(())
    }
    fn max_inclusive(&mut self, val: T) -> Result<(), FacetError> {
        match &self.min {
            Min::Unbounded => {}
            Min::Inclusive(min) => {
                if *min > val {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxInclusive",
                    ));
                }
            }
            Min::Exclusive(min) => {
                if *min >= val {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxExclusive",
                    ));
                }
            }
        }
        self.max = match self.max {
            Max::Unbounded => Max::Inclusive(val),
            Max::Inclusive(_) => unreachable!(),
            Max::Exclusive(_) => {
                return Err(FacetError::ConflictingFacet(
                    "maxInclusive conflicts with maxExclusive",
                ));
            }
        };
        Ok(())
    }
    fn max_exclusive(&mut self, val: T) -> Result<(), FacetError> {
        match &self.min {
            Min::Unbounded => {}
            Min::Inclusive(min) => {
                if *min > val {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxInclusive",
                    ));
                }
            }
            Min::Exclusive(min) => {
                if *min >= val {
                    return Err(FacetError::ConflictingFacet(
                        "minInclusive conflicts with maxExclusive",
                    ));
                }
            }
        }
        self.max = match self.max {
            Max::Unbounded => Max::Exclusive(val),
            Max::Inclusive(_) => {
                return Err(FacetError::ConflictingFacet(
                    "maxExclusive conflicts with maxInclusive",
                ));
            }
            Max::Exclusive(_) => unreachable!(),
        };
        Ok(())
    }

    fn is_valid(&self, v: &T) -> bool {
        self.min.is_valid(v) && self.max.is_valid(v)
    }
}

#[derive(Clone)]
pub struct PatternFacet(String, regex::Regex);
impl PartialEq for PatternFacet {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for PatternFacet {}
impl std::hash::Hash for PatternFacet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl fmt::Debug for PatternFacet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_tuple("PatternFacet").field(&self.0).finish()
    }
}
impl PatternFacet {
    fn is_valid(&self, value: &str) -> bool {
        self.1.is_match(value)
    }
}

#[derive(Default)]
pub struct Compiler;
impl super::DatatypeCompiler for Compiler {
    type DT = XsdDatatypes;
    type DTValue = XsdDatatypeValues;
    type Error = XsdDatatypeError;

    fn datatype_value(
        &self,
        ctx: &Context,
        datatype_name: &types::DatatypeName,
        value: &str,
        ns: Option<&str>,
        ns_bindings: &[(String, String)],
    ) -> Result<Self::DTValue, Self::Error> {
        match datatype_name {
            DatatypeName::CName(types::QName(_namespace_uri, name)) => {
                self.compile_value(ctx, &name.0, &name.1, value, ns, ns_bindings)
            }
            DatatypeName::NamespacedName(_) => {
                unimplemented!()
            }
            _ => panic!("Unexpected {datatype_name:?}"),
        }
    }

    fn datatype_name(
        &self,
        ctx: &Context,
        datatype_name: &types::DatatypeName,
        params: &[types::Param],
    ) -> Result<Self::DT, Self::Error> {
        match datatype_name {
            types::DatatypeName::CName(types::QName(_namespace_uri, name)) => {
                self.compile(ctx, &name.0, &name.1, params)
            }
            _ => panic!("Unexpected {datatype_name:?}"),
        }
    }
}

impl Compiler {
    fn compile(
        &self,
        ctx: &Context,
        span: &types::Span,
        name: &str,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, XsdDatatypeError> {
        match name {
            "normalizedString" => {
                self.normalized_string(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "normalizedString",
                        facet,
                    })
            }
            "string" => self
                .string(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "string",
                    facet,
                }),
            "byte" => self
                .byte(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "byte",
                    facet,
                }),
            "short" => self
                .short(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "short",
                    facet,
                }),
            "unsignedByte" => {
                self.unsigned_byte(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "unsignedByte",
                        facet,
                    })
            }
            "unsignedShort" => {
                self.unsigned_short(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "unsignedShort",
                        facet,
                    })
            }
            "long" => self
                .long(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "long",
                    facet,
                }),
            "int" => self
                .int(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "int",
                    facet,
                }),
            "integer" => self
                .integer(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "integer",
                    facet,
                }),
            "positiveInteger" => {
                self.positive_integer(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "positiveInteger",
                        facet,
                    })
            }
            "nonNegativeInteger" => {
                self.non_negative_integer(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "nonNegativeInteger",
                        facet,
                    })
            }
            "nonPositiveInteger" => {
                self.non_positive_integer(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "nonPositiveInteger",
                        facet,
                    })
            }
            "negativeInteger" => {
                self.negative_integer(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "negativeInteger",
                        facet,
                    })
            }
            "decimal" => self
                .decimal(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "decimal",
                    facet,
                }),
            "float" => self
                .float(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "float",
                    facet,
                }),
            "double" => self
                .double(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "double",
                    facet,
                }),
            "NMTOKENS" => self
                .nmtokens(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "NMTOKENS",
                    facet,
                }),
            "NMTOKEN" => self
                .nmtoken(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "NMTOKEN",
                    facet,
                }),
            "NCName" => self
                .ncname(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "NCName",
                    facet,
                }),
            "Name" => self
                .name(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "Name",
                    facet,
                }),
            "token" => self
                .token(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "token",
                    facet,
                }),
            "duration" => self
                .duration(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "duration",
                    facet,
                }),
            "date" => self
                .date(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "date",
                    facet,
                }),
            "dateTime" => self
                .datetime(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "dateTime",
                    facet,
                }),
            "anyURI" => self
                .any_uri(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "anyURI",
                    facet,
                }),
            "language" => self
                .language(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "language",
                    facet,
                }),
            "boolean" => self
                .boolean(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "boolean",
                    facet,
                }),
            "unsignedInt" => {
                self.unsigned_int(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "unsignedInt",
                        facet,
                    })
            }
            "ID" => self
                .id(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "ID",
                    facet,
                }),
            "IDREF" => self
                .idref(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "IDREF",
                    facet,
                }),
            "unsignedLong" => {
                self.unsigned_long(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "unsignedLong",
                        facet,
                    })
            }
            "time" => self
                .time(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "time",
                    facet,
                }),
            "gYearMonth" => self
                .gyearmonth(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "gYearMonth",
                    facet,
                }),
            "gYear" => self
                .gyear(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "gYear",
                    facet,
                }),
            "gMonthDay" => self
                .gmonthday(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "gMonthDay",
                    facet,
                }),
            "gDay" => self
                .gday(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "gDay",
                    facet,
                }),
            "gMonth" => self
                .gmonth(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "gMonth",
                    facet,
                }),
            "hexBinary" => self
                .hex_binary(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "hexBinary",
                    facet,
                }),
            "base64Binary" => {
                self.base64_binary(ctx, params)
                    .map_err(|facet| XsdDatatypeError::Facet {
                        type_name: "base64Binary",
                        facet,
                    })
            }
            "IDREFS" => self
                .idrefs(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "IDREFS",
                    facet,
                }),
            "ENTITY" => self
                .entity(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "ENTITY",
                    facet,
                }),
            "ENTITIES" => self
                .entities(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "ENTITIES",
                    facet,
                }),
            _ => Err(XsdDatatypeError::UnsupportedDatatype {
                span: ctx.convert_span(span),
                name: name.to_string(),
            }),
        }
    }

    fn compile_value(
        &self,
        ctx: &Context,
        span: &types::Span,
        name: &str,
        value: &str,
        ns: Option<&str>,
        ns_bindings: &[(String, String)],
    ) -> Result<XsdDatatypeValues, XsdDatatypeError> {
        match name {
            "string" => Ok(XsdDatatypeValues::String(value.to_string())),
            "token" => Ok(XsdDatatypeValues::Token(normalize_whitespace(value))),
            "Name" => {
                let normalized = normalize_whitespace(value);
                if !is_valid_name(&normalized) {
                    return Err(XsdDatatypeError::InvalidValueOfType {
                        span: ctx.convert_span(span),
                        type_name: "Name",
                    });
                }
                Ok(XsdDatatypeValues::Name(normalized))
            }
            "QName" => {
                // Use the ns attribute from <value> if present, otherwise fall back to context
                let default_ns = ns.unwrap_or(ctx.default_namespace_uri()).to_string();
                let qname = QNameVal::resolve(value, &default_ns, |prefix| {
                    // First check ns_bindings from the schema element (XML syntax),
                    // then fall back to context namespace declarations (compact syntax)
                    ns_bindings
                        .iter()
                        .find(|(p, _)| p == prefix)
                        .map(|(_, uri)| uri.clone())
                        .or_else(|| {
                            ctx.namespace_uri_for_prefix_str(prefix)
                                .map(|s| s.to_string())
                        })
                })
                .ok_or(XsdDatatypeError::InvalidValueOfType {
                    span: ctx.convert_span(span),
                    type_name: "QName",
                })?;
                Ok(XsdDatatypeValues::QName(qname))
            }
            _ => Err(XsdDatatypeError::UnsupportedDatatype {
                span: ctx.convert_span(span),
                name: name.to_string(),
            }),
        }
    }

    fn normalized_string(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NormalizedString(StringFacets {
            len,
            pattern,
        }))
    }

    fn string(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::String(StringFacets { len, pattern }))
    }

    fn short(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::i16(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::i16(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::i16(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::i16(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Short(min_max, pattern))
    }

    fn unsigned_short(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::u16(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::u16(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::u16(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::u16(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::UnsignedShort(min_max, pattern))
    }

    fn long(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::i64(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::i64(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::i64(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::i64(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Long(min_max, pattern))
    }

    fn int(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::i32(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::i32(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::i32(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::i32(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Int(min_max, pattern))
    }
    fn integer(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Integer(min_max, pattern))
    }

    fn positive_integer(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::biguint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::biguint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::biguint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::biguint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::PositiveInteger(min_max, pattern))
    }

    fn decimal(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;
        let mut fraction_digits = None;
        let mut total_digits = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigdecimal(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigdecimal(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigdecimal(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigdecimal(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                "fractionDigits" => fraction_digits = Some(Self::u16(ctx, param)?),
                "totalDigits" => total_digits = Some(Self::u16(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Decimal {
            min_max,
            pattern,
            fraction_digits,
            total_digits,
        })
    }
    fn double(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::f64(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::f64(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::f64(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::f64(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Double(min_max, pattern))
    }

    fn nmtokens(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NmTokens(len, pattern))
    }

    fn nmtoken(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NmToken(StringFacets { len, pattern }))
    }

    fn ncname(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NcName(StringFacets { len, pattern }))
    }

    fn token(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Token(StringFacets { len, pattern }))
    }

    fn duration(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Duration(pattern))
    }

    fn date(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Date(pattern))
    }

    fn datetime(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Datetime(pattern))
    }

    fn any_uri(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::AnyURI(StringFacets { len, pattern }))
    }

    fn language(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Language(StringFacets { len, pattern }))
    }

    fn boolean(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Boolean(pattern))
    }

    fn unsigned_int(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::u32(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::u32(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::u32(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::u32(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::UnsignedInt(min_max, pattern))
    }

    fn unsigned_long(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::u64(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::u64(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::u64(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::u64(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::UnsignedLong(min_max, pattern))
    }

    fn id(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Id(StringFacets { len, pattern }))
    }

    fn idref(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::IdRef(StringFacets { len, pattern }))
    }

    fn byte(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::i8(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::i8(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::i8(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::i8(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Byte(min_max, pattern))
    }

    fn unsigned_byte(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::u8(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::u8(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::u8(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::u8(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::UnsignedByte(min_max, pattern))
    }

    fn float(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::f32(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::f32(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::f32(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::f32(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Float(min_max, pattern))
    }

    fn non_negative_integer(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::biguint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::biguint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::biguint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::biguint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NonNegativeInteger(min_max, pattern))
    }

    fn non_positive_integer(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NonPositiveInteger(min_max, pattern))
    }

    fn negative_integer(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::NegativeInteger(min_max, pattern))
    }

    fn time(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Time(pattern))
    }

    fn gyearmonth(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::GYearMonth(pattern))
    }

    fn gyear(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::GYear(pattern))
    }

    fn gmonthday(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::GMonthDay(pattern))
    }

    fn gday(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::GDay(pattern))
    }

    fn gmonth(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::GMonth(pattern))
    }

    fn hex_binary(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::HexBinary(len, pattern))
    }

    fn base64_binary(
        &self,
        ctx: &Context,
        params: &[types::Param],
    ) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Base64Binary(len, pattern))
    }

    fn name(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Name(StringFacets { len, pattern }))
    }

    fn idrefs(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::IdRefs(len, pattern))
    }

    fn entity(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Entity(StringFacets { len, pattern }))
    }

    fn entities(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;
        let mut pattern = None;

        for param in params {
            match &param.name.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.span),
                        param.name.to_string(),
                    ));
                }
            }
        }

        Ok(XsdDatatypes::Entities(len, pattern))
    }

    fn i8(ctx: &Context, param: &types::Param) -> Result<i8, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn u8(ctx: &Context, param: &types::Param) -> Result<u8, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn i16(ctx: &Context, param: &types::Param) -> Result<i16, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn i32(ctx: &Context, param: &types::Param) -> Result<i32, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn i64(ctx: &Context, param: &types::Param) -> Result<i64, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn u32(ctx: &Context, param: &types::Param) -> Result<u32, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn u64(ctx: &Context, param: &types::Param) -> Result<u64, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn u16(ctx: &Context, param: &types::Param) -> Result<u16, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn f32(ctx: &Context, param: &types::Param) -> Result<FiniteF32, FacetError> {
        param
            .value
            .as_string_value()
            .parse::<f32>()
            .map_err(|e| FacetError::InvalidFloat(ctx.convert_span(&param.span), e.to_string()))
            .and_then(|v| {
                if v.is_finite() {
                    Ok(FiniteF32(v))
                } else {
                    Err(FacetError::InvalidFloat(
                        ctx.convert_span(&param.span),
                        "Only finite values allowed".to_string(),
                    ))
                }
            })
    }

    fn f64(ctx: &Context, param: &types::Param) -> Result<FiniteF64, FacetError> {
        param
            .value
            .as_string_value()
            .parse::<f64>()
            .map_err(|e| FacetError::InvalidFloat(ctx.convert_span(&param.span), e.to_string()))
            .and_then(|v| {
                if v.is_finite() {
                    Ok(FiniteF64(v))
                } else {
                    Err(FacetError::InvalidFloat(
                        ctx.convert_span(&param.span),
                        "Only finite values allowed".to_string(),
                    ))
                }
            })
    }

    fn bigint(ctx: &Context, param: &types::Param) -> Result<num_bigint::BigInt, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: num_bigint::ParseBigIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn biguint(ctx: &Context, param: &types::Param) -> Result<num_bigint::BigUint, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: num_bigint::ParseBigIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn bigdecimal(
        ctx: &Context,
        param: &types::Param,
    ) -> Result<bigdecimal::BigDecimal, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: bigdecimal::ParseBigDecimalError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn usize(ctx: &Context, param: &types::Param) -> Result<usize, FacetError> {
        param
            .value
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.span), e.to_string())
            })
    }

    fn pattern(&self, ctx: &Context, param: &types::Param) -> Result<PatternFacet, FacetError> {
        let xsd_pat = param.value.as_string_value();
        let rust_pat = super::xsd_regex::translate(&xsd_pat).map_err(|msg| {
            FacetError::InvalidPattern(ctx.convert_span(&param.span), regex::Error::Syntax(msg))
        })?;
        regex::Regex::new(&rust_pat)
            .map(|re| PatternFacet(xsd_pat, re))
            .map_err(|e| FacetError::InvalidPattern(ctx.convert_span(&param.span), e))
    }
}

/// A resolved QName: (namespace_uri, localname)
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct QNameVal {
    pub namespace_uri: String,
    pub localname: String,
}

impl QNameVal {
    /// Parse a QName string and resolve its prefix using the given namespace lookup.
    /// For unprefixed names, `default_ns` is used as the namespace URI.
    pub fn resolve(
        val: &str,
        default_ns: &str,
        lookup_ns: impl Fn(&str) -> Option<String>,
    ) -> Option<Self> {
        if let Some(pos) = val.find(':') {
            let prefix = &val[0..pos];
            let localname = &val[pos + 1..];
            if is_valid_ncname(prefix) && is_valid_ncname(localname) {
                let ns = lookup_ns(prefix)?;
                Some(QNameVal {
                    namespace_uri: ns,
                    localname: localname.to_string(),
                })
            } else {
                None
            }
        } else if is_valid_ncname(val) {
            Some(QNameVal {
                namespace_uri: default_ns.to_string(),
                localname: val.to_string(),
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use assert_matches::assert_matches;
    use codemap::CodeMap;
    use relaxng_syntax::types;

    #[test]
    fn it_works() {
        let mut map = CodeMap::new();
        let file = map.add_file("main.rnc".to_string(), "just testing".to_string());
        let ctx = Context::new(file);
        let c = Compiler;
        let name =
            types::IdentifierOrKeyword::Identifier(types::Identifier(0..0, "length".to_string()));
        let value = types::Literal(
            0..0,
            vec![types::LiteralSegment {
                body: "1".to_string(),
            }],
        );
        let param = types::Param {
            span: 0..0,
            annotations: None,
            name,
            value,
        };
        let res = c.compile(&ctx, &(0..0), "normalizedString", &[param]);
        assert_matches!(
            res,
            Ok(XsdDatatypes::NormalizedString(StringFacets {
                len: LengthFacet::Length(1),
                pattern: None
            }))
        )
    }

    // Helper: compile an XSD datatype with no params
    fn compile_no_params(name: &str) -> XsdDatatypes {
        let mut map = CodeMap::new();
        let file = map.add_file("test.rnc".to_string(), "test".to_string());
        let ctx = Context::new(file);
        let c = Compiler;
        c.compile(&ctx, &(0..0), name, &[]).unwrap()
    }

    use crate::datatype::Datatype;

    #[test]
    fn length_facet_merge_max_then_min() {
        let mut f = LengthFacet::MaxLength(10);
        f.merge(LengthFacet::MinLength(3)).unwrap();
        assert_eq!(f, LengthFacet::MinMaxLength(3, 10));
    }

    #[test]
    fn length_facet_merge_max_then_min_conflict() {
        let mut f = LengthFacet::MaxLength(2);
        assert!(f.merge(LengthFacet::MinLength(5)).is_err());
    }

    #[test]
    fn length_facet_merge_max_then_max_conflict() {
        let mut f = LengthFacet::MaxLength(10);
        assert!(f.merge(LengthFacet::MaxLength(5)).is_err());
    }

    #[test]
    fn length_facet_merge_minmax_conflict() {
        let mut f = LengthFacet::MinMaxLength(3, 10);
        assert!(f.merge(LengthFacet::MinLength(1)).is_err());
    }

    #[test]
    fn length_facet_merge_length_conflict() {
        let mut f = LengthFacet::Length(5);
        assert!(f.merge(LengthFacet::MinLength(1)).is_err());
    }

    #[test]
    fn token_is_valid() {
        let dt = XsdDatatypes::Token(StringFacets {
            len: LengthFacet::Unbounded,
            pattern: None,
        });
        assert!(dt.is_valid("hello world"));
        assert!(dt.is_valid("  spaced  "));
    }

    #[test]
    fn token_length_facet() {
        let dt = XsdDatatypes::Token(StringFacets {
            len: LengthFacet::MaxLength(5),
            pattern: None,
        });
        assert!(dt.is_valid("hello"));
        // " a b " normalizes to "a b" (3 chars)
        assert!(dt.is_valid(" a b "));
    }

    #[test]
    fn nmtoken_valid() {
        let dt = XsdDatatypes::NmToken(StringFacets {
            len: LengthFacet::Unbounded,
            pattern: None,
        });
        assert!(dt.is_valid("hello"));
        assert!(dt.is_valid("a-b.c"));
        assert!(dt.is_valid("123"));
        assert!(!dt.is_valid(""));
        assert!(!dt.is_valid("hello world"));
    }

    #[test]
    fn nmtokens_valid() {
        let dt = XsdDatatypes::NmTokens(LengthFacet::Unbounded, None);
        assert!(dt.is_valid("hello"));
        assert!(dt.is_valid("hello world"));
        assert!(!dt.is_valid(""));
        assert!(!dt.is_valid("hello @invalid"));
    }

    #[test]
    fn nmtokens_length_counts_items() {
        let dt = XsdDatatypes::NmTokens(LengthFacet::MaxLength(2), None);
        assert!(dt.is_valid("a b"));
        assert!(!dt.is_valid("a b c"));
    }

    #[test]
    fn byte_valid() {
        let dt = compile_no_params("byte");
        assert!(dt.is_valid("0"));
        assert!(dt.is_valid("-128"));
        assert!(dt.is_valid("127"));
        assert!(!dt.is_valid("128"));
        assert!(!dt.is_valid("-129"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn unsigned_byte_valid() {
        let dt = compile_no_params("unsignedByte");
        assert!(dt.is_valid("0"));
        assert!(dt.is_valid("255"));
        assert!(!dt.is_valid("256"));
        assert!(!dt.is_valid("-1"));
    }

    #[test]
    fn float_valid() {
        let dt = compile_no_params("float");
        assert!(dt.is_valid("1.0"));
        assert!(dt.is_valid("-3.14"));
        assert!(dt.is_valid("0"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn non_negative_integer_valid() {
        let dt = compile_no_params("nonNegativeInteger");
        assert!(dt.is_valid("0"));
        assert!(dt.is_valid("42"));
        assert!(!dt.is_valid("-1"));
    }

    #[test]
    fn non_positive_integer_valid() {
        let dt = compile_no_params("nonPositiveInteger");
        assert!(dt.is_valid("0"));
        assert!(dt.is_valid("-42"));
        assert!(!dt.is_valid("1"));
    }

    #[test]
    fn negative_integer_valid() {
        let dt = compile_no_params("negativeInteger");
        assert!(dt.is_valid("-1"));
        assert!(dt.is_valid("-999"));
        assert!(!dt.is_valid("0"));
        assert!(!dt.is_valid("1"));
    }

    #[test]
    fn time_valid() {
        let dt = compile_no_params("time");
        assert!(dt.is_valid("13:20:00"));
        assert!(dt.is_valid("13:20:00Z"));
        assert!(dt.is_valid("13:20:00.5"));
        assert!(dt.is_valid("13:20:00+05:30"));
        assert!(!dt.is_valid("abc"));
        assert!(!dt.is_valid("1:2:3")); // needs 2-digit fields
    }

    #[test]
    fn gyearmonth_valid() {
        let dt = compile_no_params("gYearMonth");
        assert!(dt.is_valid("2023-01"));
        assert!(dt.is_valid("-0045-12"));
        assert!(dt.is_valid("2023-01Z"));
        assert!(!dt.is_valid("2023"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn gyear_valid() {
        let dt = compile_no_params("gYear");
        assert!(dt.is_valid("2023"));
        assert!(dt.is_valid("-0045"));
        assert!(dt.is_valid("2023Z"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn gmonthday_valid() {
        let dt = compile_no_params("gMonthDay");
        assert!(dt.is_valid("--12-25"));
        assert!(dt.is_valid("--01-01Z"));
        assert!(!dt.is_valid("12-25"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn gday_valid() {
        let dt = compile_no_params("gDay");
        assert!(dt.is_valid("---25"));
        assert!(dt.is_valid("---01Z"));
        assert!(!dt.is_valid("--25"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn gmonth_valid() {
        let dt = compile_no_params("gMonth");
        assert!(dt.is_valid("--12"));
        assert!(dt.is_valid("--01Z"));
        assert!(!dt.is_valid("12"));
        assert!(!dt.is_valid("abc"));
    }

    #[test]
    fn hex_binary_valid() {
        let dt = compile_no_params("hexBinary");
        assert!(dt.is_valid(""));
        assert!(dt.is_valid("0FB7"));
        assert!(dt.is_valid("aabb"));
        assert!(dt.is_valid("0F")); // 1 octet is valid
        assert!(!dt.is_valid("0FG")); // invalid hex char
        assert!(!dt.is_valid("abc")); // odd length
    }

    #[test]
    fn base64_binary_valid() {
        let dt = compile_no_params("base64Binary");
        assert!(dt.is_valid(""));
        assert!(dt.is_valid("SGVsbG8="));
        assert!(dt.is_valid("AQID"));
        assert!(!dt.is_valid("!!!"));
    }

    #[test]
    fn name_valid() {
        let dt = compile_no_params("Name");
        assert!(dt.is_valid("foo"));
        assert!(dt.is_valid("foo:bar")); // Name allows colon
        assert!(dt.is_valid("_underscore"));
        assert!(!dt.is_valid("123"));
        assert!(!dt.is_valid(""));
    }

    #[test]
    fn idrefs_valid() {
        let dt = compile_no_params("IDREFS");
        assert!(dt.is_valid("id1"));
        assert!(dt.is_valid("id1 id2 id3"));
        assert!(!dt.is_valid(""));
        assert!(!dt.is_valid("id:1")); // NCName, no colon
    }

    #[test]
    fn entity_valid() {
        let dt = compile_no_params("ENTITY");
        assert!(dt.is_valid("foo"));
        assert!(!dt.is_valid("foo:bar")); // NCName, no colon
        assert!(!dt.is_valid(""));
    }

    #[test]
    fn entities_valid() {
        let dt = compile_no_params("ENTITIES");
        assert!(dt.is_valid("foo"));
        assert!(dt.is_valid("foo bar"));
        assert!(!dt.is_valid(""));
    }

    // Compilation test: all new types compile
    #[test]
    fn all_new_types_compile() {
        for name in [
            "byte",
            "unsignedByte",
            "float",
            "nonNegativeInteger",
            "nonPositiveInteger",
            "negativeInteger",
            "time",
            "gYearMonth",
            "gYear",
            "gMonthDay",
            "gDay",
            "gMonth",
            "hexBinary",
            "base64Binary",
            "Name",
            "IDREFS",
            "ENTITY",
            "ENTITIES",
        ] {
            compile_no_params(name);
        }
    }
}
