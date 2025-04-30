use crate::datatype::relax::normalize_whitespace;
use crate::Context;
use lazy_static::lazy_static;
use relaxng_syntax::types;
use relaxng_syntax::types::DatatypeName;
use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

pub const NAMESPACE_URI: &str = "http://www.w3.org/2001/XMLSchema-datatypes";

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum XsdDatatypeValues {
    String(String),
    Token(String),
    QName(QNameVal),
}

impl super::Datatype for XsdDatatypeValues {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            XsdDatatypeValues::String(s) => s == value,
            XsdDatatypeValues::Token(s) => s == &normalize_whitespace(value),
            XsdDatatypeValues::QName(v) => QNameVal::try_from_val(value)
                .map(|value| &value == v)
                .unwrap_or(false),
        }
    }
}

lazy_static! {
    static ref LANG_RE: regex::Regex = regex::Regex::new(r"^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$").unwrap();
    static ref DATETIME_RE: regex::Regex = regex::Regex::new(r"^-?\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:Z(?:(?:\+|-)\d{2}:\d{2})?)?$").unwrap();
    static ref DURATION_RE: regex::Regex = regex::Regex::new(r"^P(([0-9]+([.,][0-9]*)?Y)?([0-9]+([.,][0-9]*)?M)?([0-9]+([.,][0-9]*)?D)?T?([0-9]+([.,][0-9]*)?H)?([0-9]+([.,][0-9]*)?M)?([0-9]+([.,][0-9]*)?S)?)|\d{4}-?(0[1-9]|11|12)-?(?:[0-2]\d|30|31)T((?:[0-1][0-9]|[2][0-3]):?(?:[0-5][0-9]):?(?:[0-5][0-9]|60)|2400|24:00)$").unwrap();
}

// TODO: actually apply all required facets to each datatype
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum XsdDatatypes {
    NormalizedString(StringFacets),
    String(StringFacets),
    Integer(MinMaxFacet<num_bigint::BigInt>, Option<PatternFacet>),
    UnsignedInt(MinMaxFacet<num_bigint::BigUint>, Option<PatternFacet>),
    UnsignedLong(MinMaxFacet<u64>, Option<PatternFacet>),
    Decimal {
        min_max: MinMaxFacet<bigdecimal::BigDecimal>,
        pattern: Option<PatternFacet>,
        fraction_digits: Option<u16>,
        total_digits: Option<u16>,
    },
    Double(Option<PatternFacet>),
    NmTokens(LengthFacet),
    NmToken(LengthFacet),
    NcName(LengthFacet),
    Token(LengthFacet),
    Duration(Option<PatternFacet>),
    Date(Option<PatternFacet>),
    Datetime(Option<PatternFacet>),
    AnyURI(Option<PatternFacet>),
    Language(Option<PatternFacet>),
    Boolean(Option<PatternFacet>),
    Id(Option<PatternFacet>),
    IdRef(Option<PatternFacet>),
}
impl super::Datatype for XsdDatatypes {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            XsdDatatypes::NormalizedString(str_facets) => {
                let normal_val = super::relax::normalize_whitespace(value);
                str_facets.is_valid(&normal_val)
            }
            XsdDatatypes::String(str_facets) => str_facets.is_valid(value),
            XsdDatatypes::Integer(min_max, pat) => {
                let pat_valid = if let Some(p) = pat {
                    p.is_valid(value)
                } else {
                    true
                };
                pat_valid
                    && if let Ok(v) = num_bigint::BigInt::from_str(value) {
                        min_max.is_valid(&v)
                    } else {
                        false
                    }
            }
            XsdDatatypes::Decimal {
                min_max,
                pattern: pat,
                fraction_digits: _,
                total_digits: _,
            } => {
                bigdecimal::BigDecimal::from_str(value)
                    .map(|v| min_max.is_valid(&v))
                    .unwrap_or(false)
                    && pat.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::NmTokens(_len) => {
                unimplemented!()
            }
            XsdDatatypes::NmToken(_len) => {
                unimplemented!()
            }
            XsdDatatypes::NcName(len) => len.is_valid(value) && is_valid_ncname(value),
            XsdDatatypes::Token(_len) => {
                unimplemented!()
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
            XsdDatatypes::Double(patt) => {
                value.parse::<f64>().is_ok()
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::AnyURI(patt) => {
                uriparse::URIReference::try_from(value).is_ok()
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Language(patt) => {
                LANG_RE.is_match(value)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Boolean(patt) => {
                (value == "true" || value == "false" || value == "1" || value == "0")
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedInt(min_max, patt) => {
                num_bigint::BigUint::from_str(value)
                    .map(|v| min_max.is_valid(&v))
                    .unwrap_or(false)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::UnsignedLong(min_max, patt) => {
                u64::from_str(value)
                    .map(|v| min_max.is_valid(&v))
                    .unwrap_or(false)
                    && patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true)
            }
            XsdDatatypes::Id(patt) => patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true),
            XsdDatatypes::IdRef(patt) => patt.as_ref().map(|p| p.1.is_match(value)).unwrap_or(true),
        }
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
        let actual = value.chars().count();
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
                    return Err(FacetError::ConflictingFacet("minLength"))
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
            LengthFacet::MaxLength(_) => {
                unimplemented!()
            }
            LengthFacet::MinMaxLength(_, _) => {
                unimplemented!()
            }
            LengthFacet::Length(_) => {
                unimplemented!()
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
    T: PartialOrd,
{
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
                ))
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
                ))
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
                ))
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
                ))
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
    ) -> Result<Self::DTValue, Self::Error> {
        match datatype_name {
            DatatypeName::CName(types::QName(_namespace_uri, name)) => {
                self.compile_value(ctx, &name.0, &name.1, value)
            }
            DatatypeName::NamespacedName(_) => {
                unimplemented!()
            }
            _ => panic!("Unexpected {:?}", datatype_name),
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
            _ => panic!("Unexpected {:?}", datatype_name),
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
            "integer" => self
                .integer(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "integer",
                    facet,
                }),
            "decimal" => self
                .decimal(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "decimal",
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
    ) -> Result<XsdDatatypeValues, XsdDatatypeError> {
        match name {
            "string" => Ok(XsdDatatypeValues::String(value.to_string())),
            "token" => Ok(XsdDatatypeValues::Token(normalize_whitespace(value))),
            "QName" => Ok(XsdDatatypeValues::QName(
                QNameVal::try_from_val(value).map_err(|_| {
                    XsdDatatypeError::InvalidValueOfType {
                        span: ctx.convert_span(span),
                        type_name: "QName",
                    }
                })?,
            )),
            _ => unimplemented!("{:?} not yet supported", name),
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
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
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
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::String(StringFacets { len, pattern }))
    }
    fn integer(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Integer(min_max, pattern))
    }
    fn decimal(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut min_max = MinMaxFacet::default();
        let mut pattern = None;
        let mut fraction_digits = None;
        let mut total_digits = None;

        for param in params {
            match &param.2.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::bigdecimal(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::bigdecimal(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::bigdecimal(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::bigdecimal(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                "fractionDigits" => fraction_digits = Some(Self::u16(ctx, param)?),
                "totalDigits" => total_digits = Some(Self::u16(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
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
            match &param.2.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::f64(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::f64(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::f64(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::f64(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Double(pattern))
    }

    fn nmtokens(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;

        for param in params {
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::NmTokens(len))
    }

    fn nmtoken(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;

        for param in params {
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::NmToken(len))
    }

    fn ncname(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;

        for param in params {
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::NcName(len))
    }

    fn token(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut len = LengthFacet::Unbounded;

        for param in params {
            match &param.2.to_string()[..] {
                "length" => len.merge(LengthFacet::Length(Self::usize(ctx, param)?))?,
                "minLength" => len.merge(LengthFacet::MinLength(Self::usize(ctx, param)?))?,
                "maxLength" => len.merge(LengthFacet::MaxLength(Self::usize(ctx, param)?))?,
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Token(len))
    }

    fn duration(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Duration(pattern))
    }

    fn date(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Date(pattern))
    }

    fn datetime(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Datetime(pattern))
    }

    fn any_uri(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::AnyURI(pattern))
    }

    fn language(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Language(pattern))
    }

    fn boolean(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
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
            match &param.2.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::biguint(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::biguint(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::biguint(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::biguint(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
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
            match &param.2.to_string()[..] {
                "minInclusive" => min_max.min_inclusive(Self::u64(ctx, param)?)?,
                "minExclusive" => min_max.min_exclusive(Self::u64(ctx, param)?)?,
                "maxInclusive" => min_max.max_inclusive(Self::u64(ctx, param)?)?,
                "maxExclusive" => min_max.max_exclusive(Self::u64(ctx, param)?)?,
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::UnsignedLong(min_max, pattern))
    }

    fn id(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::Id(pattern))
    }

    fn idref(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
        let mut pattern = None;

        for param in params {
            match &param.2.to_string()[..] {
                "pattern" => pattern = Some(self.pattern(ctx, param)?),
                _ => {
                    return Err(FacetError::InvalidFacet(
                        ctx.convert_span(&param.0),
                        param.2.to_string(),
                    ))
                }
            }
        }

        Ok(XsdDatatypes::IdRef(pattern))
    }

    fn u64(ctx: &Context, param: &types::Param) -> Result<u64, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn u16(ctx: &Context, param: &types::Param) -> Result<u16, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn f64(ctx: &Context, param: &types::Param) -> Result<f64, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseFloatError| {
                FacetError::InvalidFloat(ctx.convert_span(&param.0), e.to_string())
            })
            .and_then(|v: f64| {
                if v.is_finite() {
                    Ok(v)
                } else {
                    Err(FacetError::InvalidFloat(
                        ctx.convert_span(&param.0),
                        "Only finite values allowed".to_string(),
                    ))
                }
            })
    }

    fn bigint(ctx: &Context, param: &types::Param) -> Result<num_bigint::BigInt, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: num_bigint::ParseBigIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn biguint(ctx: &Context, param: &types::Param) -> Result<num_bigint::BigUint, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: num_bigint::ParseBigIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn bigdecimal(
        ctx: &Context,
        param: &types::Param,
    ) -> Result<bigdecimal::BigDecimal, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: bigdecimal::ParseBigDecimalError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn usize(ctx: &Context, param: &types::Param) -> Result<usize, FacetError> {
        param
            .3
            .as_string_value()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                FacetError::InvalidInt(ctx.convert_span(&param.0), e.to_string())
            })
    }

    fn pattern(&self, ctx: &Context, param: &types::Param) -> Result<PatternFacet, FacetError> {
        regex::Regex::new(&param.3.as_string_value())
            .map(|re| PatternFacet(param.3.as_string_value(), re))
            .map_err(|e| FacetError::InvalidPattern(ctx.convert_span(&param.0), e))
    }
}

trait TryFromVal: Sized {
    fn try_from_val(value: &str) -> Result<Self, ()>;
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct QNameVal(String);
impl TryFromVal for QNameVal {
    fn try_from_val(val: &str) -> Result<Self, ()> {
        if let Some(pos) = val.find(':') {
            let prefix = &val[0..pos];
            let localname = &val[pos + 1..];
            if is_valid_ncname(prefix) && is_valid_ncname(localname) {
                unimplemented!("Need to be able to look up prefix {:?}", prefix);
            //Ok(QNameVal(val.to_string()))
            } else {
                Err(())
            }
        } else if is_valid_ncname(val) {
            Ok(QNameVal(val.to_string()))
        } else {
            Err(())
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
        let param = types::Param(0..0, None, name, value);
        let res = c.compile(&ctx, &(0..0), "normalizedString", &[param]);
        assert_matches!(
            res,
            Ok(XsdDatatypes::NormalizedString(StringFacets {
                len: LengthFacet::Length(1),
                pattern: None
            }))
        )
    }
}
