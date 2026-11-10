use crate::model::compiler::Context;
use crate::syntax::types;
use crate::syntax::types::DatatypeName;
use crate::xml_whitespace::{
    collapse_xml_whitespace, is_xml_whitespace, split_xml_whitespace, trim_xml_whitespace,
};
use lazy_static::lazy_static;
use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

/// XSD 1.0 anyURI lexical check: the value must be a valid URI reference
/// once the characters that XLink 1.0 §5.4 says to escape (controls, space,
/// `<>"{}|\\^\``, DEL and non-ASCII) have been %-escaped. This is what Jing
/// does. Other URI syntax, such as `%`, `#` and `:`, still has to be valid.
fn is_valid_any_uri(value: &str) -> bool {
    use std::borrow::Cow;
    use std::fmt::Write;
    fn must_escape(c: char) -> bool {
        matches!(
            c,
            '\0'..=' ' | '<' | '>' | '"' | '{' | '}' | '|' | '\\' | '^' | '`'
        ) || c >= '\u{7f}'
    }
    let escaped = if value.chars().any(must_escape) {
        let mut out = String::with_capacity(value.len() * 3);
        for c in value.chars() {
            if must_escape(c) {
                let mut buf = [0u8; 4];
                for b in c.encode_utf8(&mut buf).bytes() {
                    let _ = write!(out, "%{b:02X}");
                }
            } else {
                out.push(c);
            }
        }
        Cow::Owned(out)
    } else {
        Cow::Borrowed(value)
    };
    uriparse::URIReference::try_from(escaped.as_ref()).is_ok()
}

pub(crate) const NAMESPACE_URI: &str = "http://www.w3.org/2001/XMLSchema-datatypes";

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
    // TODO: refactor to remove this type?
    /// QName values are compared after resolving their prefix, which needs the namespace
    /// bindings of both the schema and the instance document.
    QName(QNameVal),
    /// A value of any other datatype, compared in the datatype's value space
    Typed(Box<TypedValue>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct TypedValue {
    pub datatype: XsdDatatypes,
    /// The value as written in the schema
    pub lexical: String,
    value: XsdValue<'static>,
}
impl TypedValue {
    fn is_valid(&self, text: &str) -> bool {
        self.datatype
            .parse_value(text)
            .is_some_and(|v| v == self.value)
    }
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
            XsdDatatypeValues::QName(v) => QNameVal::resolve(value, default_ns, lookup_ns)
                .is_some_and(|resolved| &resolved == v),
            XsdDatatypeValues::Typed(t) => t.is_valid(value),
        }
    }
}

impl super::Datatype for XsdDatatypeValues {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            XsdDatatypeValues::QName(_) => {
                // QName comparison requires namespace context; fall back to false
                // when called without context. Use is_valid_with_ns instead.
                false
            }
            XsdDatatypeValues::Typed(t) => t.is_valid(value),
        }
    }
}

/// A value in the value space of an XSD datatype: two lexical forms of a datatype represent the
/// same value exactly when their `XsdValue`s are equal.
///
/// Equality follows Jing:
///
///  - numbers compare numerically, with float/double `-0` equal to `0` and `NaN` equal to itself
///  - durations compare field by field, so `P1D` is not equal to `PT24H`
///  - date/time values compare as points on the timeline, normalized to UTC when they have a
///    timezone; a value with a timezone never equals one without. `time` values do not wrap
///    around midnight, so `00:00:00Z` is not equal to `23:00:00-01:00`
///  - `language` and `anyURI` compare as strings, case-sensitively
///
/// Values borrow from the text they were parsed from where they can, so that checking a value is
/// valid doesn't allocate.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) enum XsdValue<'a> {
    /// Types whose value space is a string, after whitespace normalization. For `QName` this is
    /// the lexical form, since its value depends on namespace bindings; see [`QNameVal`].
    String(Cow<'a, str>),
    /// `NMTOKENS`, `IDREFS` and `ENTITIES`
    List(Vec<Cow<'a, str>>),
    /// `integer` and all types derived from it
    Integer(XsdInteger),
    /// Normalized, so that `1.0` and `1` are the same value
    Decimal(bigdecimal::BigDecimal),
    /// The bits of the value, with `-0` stored as `0` and every `NaN` as the same `NaN`
    Float(u32),
    Double(u64),
    Boolean(bool),
    /// `hexBinary` and `base64Binary`
    Binary(Vec<u8>),
    Duration(Box<Duration>),
    /// date and time types
    Instant(Instant<'a>),
}

impl XsdValue<'_> {
    fn into_owned(self) -> XsdValue<'static> {
        fn owned(s: Cow<'_, str>) -> Cow<'static, str> {
            Cow::Owned(s.into_owned())
        }
        match self {
            XsdValue::String(s) => XsdValue::String(owned(s)),
            XsdValue::List(l) => XsdValue::List(l.into_iter().map(owned).collect()),
            XsdValue::Integer(v) => XsdValue::Integer(v),
            XsdValue::Decimal(v) => XsdValue::Decimal(v),
            XsdValue::Float(v) => XsdValue::Float(v),
            XsdValue::Double(v) => XsdValue::Double(v),
            XsdValue::Boolean(v) => XsdValue::Boolean(v),
            XsdValue::Binary(v) => XsdValue::Binary(v),
            XsdValue::Duration(v) => XsdValue::Duration(v),
            XsdValue::Instant(Instant {
                has_timezone,
                year,
                second,
                fraction,
            }) => XsdValue::Instant(Instant {
                has_timezone,
                year,
                second,
                fraction: owned(fraction),
            }),
        }
    }
}

/// An integer of any size, held without allocating when it fits in an `i64`
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) enum XsdInteger {
    Small(i64),
    /// Only used for values that don't fit in an `i64`, so that each value has one representation
    Big(num_bigint::BigInt),
}
impl XsdInteger {
    /// The `xsd:integer` lexical space: an optional sign, then one or more ASCII digits
    fn parse(s: &str) -> Option<Self> {
        let digits = s.strip_prefix(['+', '-']).unwrap_or(s);
        if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
            return None;
        }
        // `i64::from_str` fails only on overflow here, since the syntax has been checked
        Some(match i64::from_str(s) {
            Ok(v) => XsdInteger::Small(v),
            Err(_) => XsdInteger::Big(
                num_bigint::BigInt::from_str(s.strip_prefix('+').unwrap_or(s)).ok()?,
            ),
        })
    }

    fn from_bigint(v: num_bigint::BigInt) -> Self {
        match i64::try_from(&v) {
            Ok(small) => XsdInteger::Small(small),
            Err(_) => XsdInteger::Big(v),
        }
    }

    fn to_bigint(&self) -> num_bigint::BigInt {
        match self {
            XsdInteger::Small(v) => (*v).into(),
            XsdInteger::Big(v) => v.clone(),
        }
    }

    fn sign(&self) -> std::cmp::Ordering {
        match self {
            XsdInteger::Small(v) => v.cmp(&0),
            XsdInteger::Big(v) => match v.sign() {
                num_bigint::Sign::Minus => std::cmp::Ordering::Less,
                num_bigint::Sign::NoSign => std::cmp::Ordering::Equal,
                num_bigint::Sign::Plus => std::cmp::Ordering::Greater,
            },
        }
    }

    /// Convert to a fixed-size integer type, or `None` if the value is out of its range
    fn to_prim<T>(&self) -> Option<T>
    where
        T: TryFrom<i64> + for<'b> TryFrom<&'b num_bigint::BigInt>,
    {
        match self {
            XsdInteger::Small(v) => T::try_from(*v).ok(),
            XsdInteger::Big(v) => T::try_from(v).ok(),
        }
    }

    /// The year after (or before) this one. There is no year zero in XSD 1.0.
    fn step_year(&self, forward: bool) -> Self {
        let delta = if forward { 1 } else { -1 };
        if let XsdInteger::Small(v) = self
            && let Some(next) = v.checked_add(delta)
        {
            return XsdInteger::Small(if next == 0 { delta } else { next });
        }
        XsdInteger::from_bigint(self.to_bigint() + delta)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct Duration {
    negative: bool,
    years: XsdInteger,
    months: XsdInteger,
    days: XsdInteger,
    hours: XsdInteger,
    minutes: XsdInteger,
    /// Normalized, so that `1.0` and `1` are the same value
    seconds: bigdecimal::BigDecimal,
}

/// A point on the timeline. Fields missing from the lexical form take the values of
/// `2000-01-01T00:00:00`, so that `--02-29` is a valid `gMonthDay`. When there is a timezone the
/// point is normalized to UTC.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct Instant<'a> {
    has_timezone: bool,
    year: XsdInteger,
    /// Whole seconds since the start of `year`
    second: i64,
    /// Digits of the fractional second, without trailing zeros
    fraction: Cow<'a, str>,
}

lazy_static! {
    static ref LANG_RE: regex::Regex = regex::Regex::new(r"^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$").unwrap();
    static ref FLOAT_RE: regex::Regex =
        regex::Regex::new(r"^[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$").unwrap();
    static ref DECIMAL_RE: regex::Regex =
        regex::Regex::new(r"^([+-]?)([0-9]*)(?:\.([0-9]*))?$").unwrap();
    static ref BASE64_RE: regex::Regex = regex::Regex::new(r"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}[AEIMQUYcgkosw048]=|[A-Za-z0-9+/][AQgw]==)?$").unwrap();
    static ref DURATION_RE: regex::Regex = regex::Regex::new(
        r"^(-)?P(?:([0-9]+)Y)?(?:([0-9]+)M)?(?:([0-9]+)D)?(?:T(?:([0-9]+)H)?(?:([0-9]+)M)?(?:([0-9]+(?:\.[0-9]*)?|\.[0-9]+)S)?)?$"
    )
    .unwrap();
    static ref DATETIME_RE: regex::Regex =
        date_time_regex(&format!("{YEAR}-(?P<mo>[0-9]{{2}})-(?P<d>[0-9]{{2}})T{TIME}"));
    static ref DATE_RE: regex::Regex =
        date_time_regex(&format!("{YEAR}-(?P<mo>[0-9]{{2}})-(?P<d>[0-9]{{2}})"));
    static ref TIME_RE: regex::Regex = date_time_regex(TIME);
    static ref GYEARMONTH_RE: regex::Regex =
        date_time_regex(&format!("{YEAR}-(?P<mo>[0-9]{{2}})"));
    static ref GYEAR_RE: regex::Regex = date_time_regex(YEAR);
    static ref GMONTHDAY_RE: regex::Regex =
        date_time_regex("--(?P<mo>[0-9]{2})-(?P<d>[0-9]{2})");
    static ref GDAY_RE: regex::Regex = date_time_regex("---(?P<d>[0-9]{2})");
    static ref GMONTH_RE: regex::Regex = date_time_regex("--(?P<mo>[0-9]{2})");
}

// Building blocks of the date and time regexes. These use only the ASCII digits: the regex
// crate's `\d` matches every Unicode decimal digit.
const YEAR: &str = r"(?P<y>-?[0-9]{4,})";
const TIME: &str = r"(?P<h>[0-9]{2}):(?P<mi>[0-9]{2}):(?P<s>[0-9]{2})(?:\.(?P<f>[0-9]+))?";
const TIMEZONE: &str = r"(?P<tz>Z|[+-][0-9]{2}:[0-9]{2})?";

fn date_time_regex(body: &str) -> regex::Regex {
    regex::Regex::new(&format!("^{body}{TIMEZONE}$")).unwrap()
}

/// The `xsd:decimal` lexical space, without normalizing, so that `1.50` has a scale of 2
fn parse_decimal(s: &str) -> Option<bigdecimal::BigDecimal> {
    let caps = DECIMAL_RE.captures(s)?;
    let int = caps.get(2).map_or("", |m| m.as_str());
    let frac = caps.get(3).map_or("", |m| m.as_str());
    if int.is_empty() && frac.is_empty() {
        return None;
    }
    let digits = num_bigint::BigInt::from_str(&format!("{}0{int}{frac}", &caps[1])).ok()?;
    Some(bigdecimal::BigDecimal::new(
        digits,
        i64::try_from(frac.len()).ok()?,
    ))
}

/// The `xsd:float` / `xsd:double` lexical space, which is narrower than what Rust's `FromStr`
/// accepts: no `inf`, `Infinity`, `+INF` or `-NaN`
fn parse_float<F: FromStr>(s: &str) -> Option<F> {
    match s {
        "INF" => "inf".parse().ok(),
        "-INF" => "-inf".parse().ok(),
        "NaN" => "NaN".parse().ok(),
        _ if FLOAT_RE.is_match(s) => s.parse().ok(),
        _ => None,
    }
}

fn parse_hex(s: &str) -> Option<Vec<u8>> {
    fn nibble(b: u8) -> Option<u8> {
        (b as char).to_digit(16).map(|d| d as u8)
    }
    let bytes = s.as_bytes();
    if !bytes.len().is_multiple_of(2) {
        return None;
    }
    bytes
        .chunks(2)
        .map(|pair| Some(nibble(pair[0])? << 4 | nibble(pair[1])?))
        .collect()
}

fn parse_base64(s: &str) -> Option<Vec<u8>> {
    let stripped: String = s.chars().filter(|&c| !is_xml_whitespace(c)).collect();
    if !BASE64_RE.is_match(&stripped) {
        return None;
    }
    let mut out = Vec::with_capacity(stripped.len() * 3 / 4);
    let mut acc = 0u32;
    let mut bits = 0;
    for c in stripped.bytes().take_while(|&c| c != b'=') {
        let v = match c {
            b'A'..=b'Z' => c - b'A',
            b'a'..=b'z' => c - b'a' + 26,
            b'0'..=b'9' => c - b'0' + 52,
            b'+' => 62,
            _ => 63, // '/', as the regex allows nothing else
        };
        acc = (acc << 6) | u32::from(v);
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((acc >> bits) as u8);
            acc &= (1 << bits) - 1;
        }
    }
    Some(out)
}

fn parse_duration(s: &str) -> Option<Duration> {
    // The regex allows every field to be absent, and allows a `T` with no time fields after it
    if s.ends_with('P') || s.ends_with('T') {
        return None;
    }
    let caps = DURATION_RE.captures(s)?;
    let int = |i| match caps.get(i) {
        Some(m) => XsdInteger::parse(m.as_str()),
        None => Some(XsdInteger::Small(0)),
    };
    let seconds = match caps.get(7) {
        Some(m) => parse_decimal(m.as_str())?.normalized(),
        None => bigdecimal::BigDecimal::from(0),
    };
    let mut d = Duration {
        negative: caps.get(1).is_some(),
        years: int(2)?,
        months: int(3)?,
        days: int(4)?,
        hours: int(5)?,
        minutes: int(6)?,
        seconds,
    };
    let zero = XsdInteger::Small(0);
    if [&d.years, &d.months, &d.days, &d.hours, &d.minutes]
        .iter()
        .all(|v| **v == zero)
        && d.seconds == 0
    {
        d.negative = false;
    }
    Some(d)
}

const REFERENCE_YEAR: i64 = 2000;
const DAYS_BEFORE_MONTH: [i64; 12] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
const SECONDS_PER_DAY: i64 = 24 * 60 * 60;

/// The proleptic Gregorian rule, applied to negative years by their position in the 400 year
/// cycle, so that -0001 is not a leap year
fn is_leap_year(year: &XsdInteger) -> bool {
    let y = match year {
        XsdInteger::Small(v) => v.rem_euclid(400),
        XsdInteger::Big(v) => i64::try_from(v % 400).unwrap().rem_euclid(400),
    };
    y % 4 == 0 && (y % 100 != 0 || y == 0)
}

fn days_in_month(month: i64, leap: bool) -> i64 {
    match month {
        2 if leap => 29,
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}

fn seconds_in_year(year: &XsdInteger) -> i64 {
    (if is_leap_year(year) { 366 } else { 365 }) * SECONDS_PER_DAY
}

/// The offset from UTC in minutes
fn parse_timezone(tz: &str) -> Option<i64> {
    if tz == "Z" {
        return Some(0);
    }
    let hours: i64 = tz[1..3].parse().ok()?;
    let minutes: i64 = tz[4..6].parse().ok()?;
    if minutes >= 60 || hours > 14 || (hours == 14 && minutes > 0) {
        return None;
    }
    let offset = hours * 60 + minutes;
    Some(if tz.starts_with('-') { -offset } else { offset })
}

fn parse_instant<'a>(re: &regex::Regex, s: &'a str) -> Option<Instant<'a>> {
    let caps = re.captures(s)?;
    // Every numeric group except the year is exactly two ASCII digits
    let num = |name, default: i64| {
        caps.name(name)
            .map_or(Some(default), |m| m.as_str().parse().ok())
    };
    let mut year = match caps.name("y") {
        Some(m) => {
            let digits = m.as_str().trim_start_matches('-');
            // More than four digits must not have leading zeros, and there is no year 0000
            if (digits.len() > 4 && digits.starts_with('0')) || digits == "0000" {
                return None;
            }
            XsdInteger::parse(m.as_str())?
        }
        None => XsdInteger::Small(REFERENCE_YEAR),
    };
    let month = num("mo", 1)?;
    let day = num("d", 1)?;
    let hour = num("h", 0)?;
    let minute = num("mi", 0)?;
    let second = num("s", 0)?;
    let fraction = caps
        .name("f")
        .map_or("", |m| m.as_str().trim_end_matches('0'));
    let timezone = match caps.name("tz") {
        Some(m) => Some(parse_timezone(m.as_str())?),
        None => None,
    };

    let leap = is_leap_year(&year);
    if !(1..=12).contains(&month) || day < 1 || day > days_in_month(month, leap) {
        return None;
    }
    // Hour 24 is allowed only as 24:00:00, meaning the first instant of the following day.
    // Seconds may be 60 to allow for leap seconds.
    let time_ok = if hour == 24 {
        minute == 0 && second == 0 && fraction.is_empty()
    } else {
        hour < 24 && minute < 60 && second <= 60
    };
    if !time_ok {
        return None;
    }

    let day_of_year =
        DAYS_BEFORE_MONTH[month as usize - 1] + i64::from(leap && month > 2) + day - 1;
    let mut second = day_of_year * SECONDS_PER_DAY + hour * 3600 + minute * 60 + second
        - timezone.unwrap_or(0) * 60;
    // the timezone adjustment, 24:00:00 and leap seconds can move the instant into the year
    // before or after
    while second < 0 {
        year = year.step_year(false);
        second += seconds_in_year(&year);
    }
    while second >= seconds_in_year(&year) {
        second -= seconds_in_year(&year);
        year = year.step_year(true);
    }

    Some(Instant {
        has_timezone: timezone.is_some(),
        year,
        second,
        fraction: Cow::Borrowed(fraction),
    })
}

fn float_bits(v: f32) -> u32 {
    if v.is_nan() {
        f32::NAN.to_bits()
    } else if v == 0.0 {
        0
    } else {
        v.to_bits()
    }
}

fn double_bits(v: f64) -> u64 {
    if v.is_nan() {
        f64::NAN.to_bits()
    } else if v == 0.0 {
        0
    } else {
        v.to_bits()
    }
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
    QName(StringFacets),
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
impl XsdDatatypes {
    /// The name of the datatype within the XSD datatype library, e.g. `"byte"`
    pub fn name(&self) -> &'static str {
        match self {
            XsdDatatypes::String(_) => "string",
            XsdDatatypes::NormalizedString(_) => "normalizedString",
            XsdDatatypes::Token(_) => "token",
            XsdDatatypes::Byte(..) => "byte",
            XsdDatatypes::Short(..) => "short",
            XsdDatatypes::UnsignedByte(..) => "unsignedByte",
            XsdDatatypes::UnsignedShort(..) => "unsignedShort",
            XsdDatatypes::Int(..) => "int",
            XsdDatatypes::Integer(..) => "integer",
            XsdDatatypes::Long(..) => "long",
            XsdDatatypes::UnsignedInt(..) => "unsignedInt",
            XsdDatatypes::UnsignedLong(..) => "unsignedLong",
            XsdDatatypes::PositiveInteger(..) => "positiveInteger",
            XsdDatatypes::NonNegativeInteger(..) => "nonNegativeInteger",
            XsdDatatypes::NonPositiveInteger(..) => "nonPositiveInteger",
            XsdDatatypes::NegativeInteger(..) => "negativeInteger",
            XsdDatatypes::Decimal { .. } => "decimal",
            XsdDatatypes::Float(..) => "float",
            XsdDatatypes::Double(..) => "double",
            XsdDatatypes::NmTokens(..) => "NMTOKENS",
            XsdDatatypes::NmToken(_) => "NMTOKEN",
            XsdDatatypes::NcName(_) => "NCName",
            XsdDatatypes::QName(_) => "QName",
            XsdDatatypes::Name(_) => "Name",
            XsdDatatypes::Duration(_) => "duration",
            XsdDatatypes::Date(_) => "date",
            XsdDatatypes::Datetime(_) => "dateTime",
            XsdDatatypes::Time(_) => "time",
            XsdDatatypes::GYearMonth(_) => "gYearMonth",
            XsdDatatypes::GYear(_) => "gYear",
            XsdDatatypes::GMonthDay(_) => "gMonthDay",
            XsdDatatypes::GDay(_) => "gDay",
            XsdDatatypes::GMonth(_) => "gMonth",
            XsdDatatypes::HexBinary(..) => "hexBinary",
            XsdDatatypes::Base64Binary(..) => "base64Binary",
            XsdDatatypes::AnyURI(_) => "anyURI",
            XsdDatatypes::Language(_) => "language",
            XsdDatatypes::Boolean(_) => "boolean",
            XsdDatatypes::Id(_) => "ID",
            XsdDatatypes::IdRef(_) => "IDREF",
            XsdDatatypes::IdRefs(..) => "IDREFS",
            XsdDatatypes::Entity(_) => "ENTITY",
            XsdDatatypes::Entities(..) => "ENTITIES",
        }
    }

    /// Validate a value with namespace context, so that the prefix of a QName can be checked
    /// against the namespace bindings in scope in the instance document.
    pub fn is_valid_with_ns(
        &self,
        value: &str,
        lookup_ns: impl Fn(&str) -> Option<String>,
    ) -> bool {
        use super::Datatype;
        match self {
            XsdDatatypes::QName(_) => {
                self.is_valid(value)
                    && match parse_qname(value) {
                        Some((Some(prefix), _)) => lookup_ns(prefix).is_some(),
                        _ => true,
                    }
            }
            _ => self.is_valid(value),
        }
    }
}
impl XsdDatatypes {
    /// The value that `text` represents in this datatype's value space, or `None` if `text` is
    /// not a valid lexical form of the datatype or the value is not allowed by its facets.
    pub(crate) fn parse_value<'a>(&self, text: &'a str) -> Option<XsdValue<'a>> {
        // The whiteSpace facet is applied before anything else, so the lexical checks and the
        // pattern / length facets all see the normalized value.
        let normalized = match self {
            XsdDatatypes::String(_) => Cow::Borrowed(text),
            _ => collapse_xml_whitespace(text),
        };
        match normalized {
            Cow::Borrowed(s) => self.parse_normalized(s),
            Cow::Owned(s) => self.parse_normalized(&s).map(XsdValue::into_owned),
        }
    }

    fn pattern(&self) -> Option<&PatternFacet> {
        match self {
            XsdDatatypes::NormalizedString(f)
            | XsdDatatypes::String(f)
            | XsdDatatypes::NmToken(f)
            | XsdDatatypes::NcName(f)
            | XsdDatatypes::QName(f)
            | XsdDatatypes::Name(f)
            | XsdDatatypes::Token(f)
            | XsdDatatypes::AnyURI(f)
            | XsdDatatypes::Language(f)
            | XsdDatatypes::Id(f)
            | XsdDatatypes::IdRef(f)
            | XsdDatatypes::Entity(f) => f.pattern.as_ref(),
            XsdDatatypes::Byte(_, p)
            | XsdDatatypes::Short(_, p)
            | XsdDatatypes::UnsignedByte(_, p)
            | XsdDatatypes::UnsignedShort(_, p)
            | XsdDatatypes::Long(_, p)
            | XsdDatatypes::Int(_, p)
            | XsdDatatypes::Integer(_, p)
            | XsdDatatypes::PositiveInteger(_, p)
            | XsdDatatypes::NonNegativeInteger(_, p)
            | XsdDatatypes::NonPositiveInteger(_, p)
            | XsdDatatypes::NegativeInteger(_, p)
            | XsdDatatypes::UnsignedInt(_, p)
            | XsdDatatypes::UnsignedLong(_, p)
            | XsdDatatypes::Float(_, p)
            | XsdDatatypes::Double(_, p)
            | XsdDatatypes::NmTokens(_, p)
            | XsdDatatypes::HexBinary(_, p)
            | XsdDatatypes::Base64Binary(_, p)
            | XsdDatatypes::IdRefs(_, p)
            | XsdDatatypes::Entities(_, p)
            | XsdDatatypes::Decimal { pattern: p, .. }
            | XsdDatatypes::Duration(p)
            | XsdDatatypes::Date(p)
            | XsdDatatypes::Datetime(p)
            | XsdDatatypes::Time(p)
            | XsdDatatypes::GYearMonth(p)
            | XsdDatatypes::GYear(p)
            | XsdDatatypes::GMonthDay(p)
            | XsdDatatypes::GDay(p)
            | XsdDatatypes::GMonth(p)
            | XsdDatatypes::Boolean(p) => p.as_ref(),
        }
    }

    /// Parse whitespace-normalized text: check it is in the lexical space, convert it to a value,
    /// and check the value against the datatype's own range and its facets.
    fn parse_normalized<'a>(&self, s: &'a str) -> Option<XsdValue<'a>> {
        fn string<'a>(s: &'a str, valid: bool, facets: &StringFacets) -> Option<XsdValue<'a>> {
            (valid && facets.len.is_valid(s)).then_some(XsdValue::String(Cow::Borrowed(s)))
        }
        fn list<'a>(
            s: &'a str,
            item_valid: fn(&str) -> bool,
            len: &LengthFacet,
        ) -> Option<XsdValue<'a>> {
            let items: Vec<Cow<'a, str>> = split_xml_whitespace(s).map(Cow::Borrowed).collect();
            (!items.is_empty()
                && items.iter().all(|i| item_valid(i))
                && len.is_valid_count(items.len()))
            .then_some(XsdValue::List(items))
        }
        /// A fixed-size integer type, whose range comes from `T`
        fn sized<'a, T>(s: &str, min_max: &MinMaxFacet<T>) -> Option<XsdValue<'a>>
        where
            T: PartialOrd + TryFrom<i64> + for<'b> TryFrom<&'b num_bigint::BigInt>,
        {
            let i = XsdInteger::parse(s)?;
            let v: T = i.to_prim()?;
            min_max.is_valid(&v).then_some(XsdValue::Integer(i))
        }
        /// An unbounded integer type, whose range is given by the allowed signs
        fn unbounded<'a, T: PartialOrd>(
            s: &str,
            signs: &[std::cmp::Ordering],
            min_max: &MinMaxFacet<T>,
            convert: impl FnOnce(num_bigint::BigInt) -> Option<T>,
        ) -> Option<XsdValue<'a>> {
            let i = XsdInteger::parse(s)?;
            if !signs.contains(&i.sign()) {
                return None;
            }
            (!min_max.bounded() || min_max.is_valid(&convert(i.to_bigint())?))
                .then_some(XsdValue::Integer(i))
        }
        use std::cmp::Ordering::{Equal, Greater, Less};

        if self.pattern().is_some_and(|p| !p.is_valid(s)) {
            return None;
        }
        match self {
            XsdDatatypes::String(f)
            | XsdDatatypes::NormalizedString(f)
            | XsdDatatypes::Token(f) => string(s, true, f),
            XsdDatatypes::Language(f) => string(s, LANG_RE.is_match(s), f),
            XsdDatatypes::NmToken(f) => string(s, is_valid_nmtoken(s), f),
            XsdDatatypes::Name(f) => string(s, is_valid_name(s), f),
            XsdDatatypes::NcName(f)
            | XsdDatatypes::Id(f)
            | XsdDatatypes::IdRef(f)
            | XsdDatatypes::Entity(f) => string(s, is_valid_ncname(s), f),
            XsdDatatypes::QName(f) => string(s, parse_qname(s).is_some(), f),
            XsdDatatypes::AnyURI(f) => string(s, is_valid_any_uri(s), f),
            XsdDatatypes::NmTokens(len, _) => list(s, is_valid_nmtoken, len),
            XsdDatatypes::IdRefs(len, _) | XsdDatatypes::Entities(len, _) => {
                list(s, is_valid_ncname, len)
            }
            XsdDatatypes::Byte(min_max, _) => sized(s, min_max),
            XsdDatatypes::Short(min_max, _) => sized(s, min_max),
            XsdDatatypes::Int(min_max, _) => sized(s, min_max),
            XsdDatatypes::Long(min_max, _) => sized(s, min_max),
            XsdDatatypes::UnsignedByte(min_max, _) => sized(s, min_max),
            XsdDatatypes::UnsignedShort(min_max, _) => sized(s, min_max),
            XsdDatatypes::UnsignedInt(min_max, _) => sized(s, min_max),
            XsdDatatypes::UnsignedLong(min_max, _) => sized(s, min_max),
            XsdDatatypes::Integer(min_max, _) => {
                unbounded(s, &[Less, Equal, Greater], min_max, Some)
            }
            XsdDatatypes::NonPositiveInteger(min_max, _) => {
                unbounded(s, &[Less, Equal], min_max, Some)
            }
            XsdDatatypes::NegativeInteger(min_max, _) => unbounded(s, &[Less], min_max, Some),
            XsdDatatypes::NonNegativeInteger(min_max, _) => {
                unbounded(s, &[Equal, Greater], min_max, |v| v.try_into().ok())
            }
            XsdDatatypes::PositiveInteger(min_max, _) => {
                unbounded(s, &[Greater], min_max, |v| v.try_into().ok())
            }
            XsdDatatypes::Decimal {
                min_max,
                pattern: _,
                fraction_digits,
                total_digits,
            } => {
                let v = parse_decimal(s)?;
                if !min_max.is_valid(&v) {
                    return None;
                }
                if let Some(td) = total_digits {
                    // totalDigits counts significant digits (ignoring sign, decimal point, leading zeros)
                    let stripped = s.trim_start_matches(['+', '-']).trim_start_matches('0');
                    let sig_digits = stripped.chars().filter(|c| c.is_ascii_digit()).count();
                    if sig_digits.max(1) > *td as usize {
                        return None;
                    }
                }
                if let Some(fd) = fraction_digits {
                    let (_digits, scale) = v.as_bigint_and_exponent();
                    if scale > i64::from(*fd) {
                        return None;
                    }
                }
                Some(XsdValue::Decimal(v.normalized()))
            }
            XsdDatatypes::Float(min_max, _) => {
                let v: f32 = parse_float(s)?;
                min_max
                    .is_valid(&FiniteF32(v))
                    .then_some(XsdValue::Float(float_bits(v)))
            }
            XsdDatatypes::Double(min_max, _) => {
                let v: f64 = parse_float(s)?;
                min_max
                    .is_valid(&FiniteF64(v))
                    .then_some(XsdValue::Double(double_bits(v)))
            }
            XsdDatatypes::Boolean(_) => match s {
                "true" | "1" => Some(XsdValue::Boolean(true)),
                "false" | "0" => Some(XsdValue::Boolean(false)),
                _ => None,
            },
            XsdDatatypes::HexBinary(len, _) => {
                let bytes = parse_hex(s)?;
                len.is_valid_count(bytes.len())
                    .then_some(XsdValue::Binary(bytes))
            }
            XsdDatatypes::Base64Binary(len, _) => {
                let bytes = parse_base64(s)?;
                len.is_valid_count(bytes.len())
                    .then_some(XsdValue::Binary(bytes))
            }
            XsdDatatypes::Duration(_) => parse_duration(s).map(|d| XsdValue::Duration(Box::new(d))),
            XsdDatatypes::Datetime(_) => parse_instant(&DATETIME_RE, s).map(XsdValue::Instant),
            XsdDatatypes::Date(_) => parse_instant(&DATE_RE, s).map(XsdValue::Instant),
            XsdDatatypes::Time(_) => parse_instant(&TIME_RE, s).map(XsdValue::Instant),
            XsdDatatypes::GYearMonth(_) => parse_instant(&GYEARMONTH_RE, s).map(XsdValue::Instant),
            XsdDatatypes::GYear(_) => parse_instant(&GYEAR_RE, s).map(XsdValue::Instant),
            XsdDatatypes::GMonthDay(_) => parse_instant(&GMONTHDAY_RE, s).map(XsdValue::Instant),
            XsdDatatypes::GDay(_) => parse_instant(&GDAY_RE, s).map(XsdValue::Instant),
            XsdDatatypes::GMonth(_) => parse_instant(&GMONTH_RE, s).map(XsdValue::Instant),
        }
    }
}

impl super::Datatype for XsdDatatypes {
    fn is_valid(&self, value: &str) -> bool {
        self.parse_value(value).is_some()
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
    match crate::syntax::compact::nc_name(crate::syntax::compact::Span::from(text)) {
        Ok((rest, _name)) => rest.fragment().is_empty(),
        Err(_) => false,
    }
}

/// Split a QName into its optional prefix and local part, after collapsing surrounding
/// whitespace.  Returns `None` if the text is not lexically a QName.
fn parse_qname(text: &str) -> Option<(Option<&str>, &str)> {
    let text = trim_xml_whitespace(text);
    match text.split_once(':') {
        Some((prefix, local)) if is_valid_ncname(prefix) && is_valid_ncname(local) => {
            Some((Some(prefix), local))
        }
        Some(_) => None,
        None if is_valid_ncname(text) => Some((None, text)),
        None => None,
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct StringFacets {
    len: LengthFacet,
    pattern: Option<PatternFacet>,
}
impl StringFacets {
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
pub(crate) struct Compiler;
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
            "QName" => self
                .qname(ctx, params)
                .map_err(|facet| XsdDatatypeError::Facet {
                    type_name: "QName",
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
            _ => {
                let datatype = self.compile(ctx, span, name, &[])?;
                let parsed = datatype.parse_value(value).ok_or_else(|| {
                    XsdDatatypeError::InvalidValueOfType {
                        span: ctx.convert_span(span),
                        type_name: datatype.name(),
                    }
                })?;
                Ok(XsdDatatypeValues::Typed(Box::new(TypedValue {
                    datatype,
                    lexical: value.to_string(),
                    value: parsed.into_owned(),
                })))
            }
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

    fn qname(&self, ctx: &Context, params: &[types::Param]) -> Result<XsdDatatypes, FacetError> {
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

        Ok(XsdDatatypes::QName(StringFacets { len, pattern }))
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
        let (prefix, localname) = parse_qname(val)?;
        let namespace_uri = match prefix {
            Some(prefix) => lookup_ns(prefix)?,
            None => default_ns.to_string(),
        };
        Some(QNameVal {
            namespace_uri,
            localname: localname.to_string(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax::types;
    use assert_matches::assert_matches;
    use codemap::CodeMap;

    #[test]
    fn any_uri_accepts_the_full_xsd_lexical_space() {
        let mut map = CodeMap::new();
        let file = map.add_file("main.rnc".to_string(), "just testing".to_string());
        let ctx = Context::new(file);
        let c = Compiler;
        let dt = c.any_uri(&ctx, &[]).unwrap();
        // Characters that XLink escaping makes valid: spaces, non-ASCII etc.
        for value in [
            "https://example.com/x",
            "a name with spaces",
            "\u{fc}n\u{ef}code",
            "<x>",
            "a|b",
            "",
        ] {
            assert!(
                crate::model::datatype::Datatype::is_valid(&dt, value),
                "anyURI must accept {value:?}"
            );
        }
        // Still invalid after escaping (Jing rejects these too)
        for value in ["100%", "%zz", "a#b#c", "1abc:foo", ":foo", "http://["] {
            assert!(
                !crate::model::datatype::Datatype::is_valid(&dt, value),
                "anyURI must reject {value:?}"
            );
        }
    }

    #[test]
    fn date_and_time_types_reject_non_ascii_digits() {
        // XSD lexical spaces use only the ASCII digits 0-9, but the regex crate's `\d` matches
        // every Unicode decimal digit (here Arabic-Indic U+0660..U+0669).
        let cases = [
            ("dateTime", "2025-01-01T10:30:00", "٢٠٢٥-٠١-٠١T١٠:٣٠:٠٠"),
            ("time", "10:30:00", "١٠:٣٠:٠٠"),
            ("gYearMonth", "2025-01", "٢٠٢٥-٠١"),
            ("gYear", "2025", "٢٠٢٥"),
            ("gMonthDay", "--01-31", "--٠١-٣١"),
            ("gDay", "---31", "---٣١"),
            ("gMonth", "--01", "--٠١"),
            ("duration", "P2025Y1DT10H30M", "P٢٠٢٥Y١DT١٠H٣٠M"),
        ];
        for (type_name, ascii, non_ascii) in cases {
            let dt = compile_no_params(type_name);
            assert!(
                crate::model::datatype::Datatype::is_valid(&dt, ascii),
                "{type_name} must accept {ascii:?}"
            );
            assert!(
                !crate::model::datatype::Datatype::is_valid(&dt, non_ascii),
                "{type_name} must reject {non_ascii:?}"
            );
        }
    }

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

    // Helper: compile an XSD datatype with a single param
    fn compile_one_param(name: &str, param_name: &str, param_value: &str) -> XsdDatatypes {
        let mut map = CodeMap::new();
        let file = map.add_file("test.rnc".to_string(), "test".to_string());
        let ctx = Context::new(file);
        let c = Compiler;
        let param = types::Param {
            span: 0..0,
            annotations: None,
            name: types::IdentifierOrKeyword::Identifier(types::Identifier(
                0..0,
                param_name.to_string(),
            )),
            value: types::Literal(
                0..0,
                vec![types::LiteralSegment {
                    body: param_value.to_string(),
                }],
            ),
        };
        c.compile(&ctx, &(0..0), name, &[param]).unwrap()
    }

    use crate::model::datatype::Datatype;

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
    fn positive_integer_valid() {
        let dt = compile_no_params("positiveInteger");
        assert!(dt.is_valid("1"));
        assert!(dt.is_valid("42"));
        assert!(!dt.is_valid("0"));
        assert!(!dt.is_valid("00"));
        assert!(!dt.is_valid("-1"));
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
        assert!(dt.is_valid("AQ\nID"));
        for bad in ["A", "AQ", "AQI", "AQ=", "AQIDB", "AQID=", "AQ\nI"] {
            assert!(!dt.is_valid(bad), "{bad:?} should be rejected");
        }
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
    fn only_xml_whitespace_is_collapsed() {
        let dt = compile_no_params("NMTOKENS");
        assert!(dt.is_valid(" a\tb\r\nc "));
        assert!(!dt.is_valid("a\u{A0}b"));
        let dt = compile_no_params("IDREFS");
        assert!(!dt.is_valid("a\u{2003}b"));
        let dt = compile_no_params("QName");
        assert!(dt.is_valid("\t\neg:foo\r "));
        assert!(!dt.is_valid("\u{A0}eg:foo"));
        let dt = compile_no_params("base64Binary");
        assert!(dt.is_valid("AQID\nBAUG"));
        assert!(!dt.is_valid("AQID\u{A0}BAUG"));
    }

    #[test]
    fn collapsing_types_ignore_surrounding_whitespace() {
        for (type_name, value) in [
            ("ID", "foo"),
            ("IDREF", "foo"),
            ("ENTITY", "foo"),
            ("NCName", "foo"),
            ("Name", "a:b"),
            ("NMTOKEN", "1a"),
            ("language", "en-GB"),
            ("anyURI", "http://example.com/"),
            ("boolean", "true"),
            ("byte", "-5"),
            ("short", "5"),
            ("int", "5"),
            ("long", "5"),
            ("unsignedByte", "5"),
            ("unsignedShort", "5"),
            ("unsignedInt", "5"),
            ("unsignedLong", "5"),
            ("integer", "5"),
            ("positiveInteger", "5"),
            ("nonNegativeInteger", "5"),
            ("nonPositiveInteger", "-5"),
            ("negativeInteger", "-5"),
            ("decimal", "1.5"),
            ("float", "1.5"),
            ("double", "1.5"),
            ("date", "2024-01-31"),
            ("dateTime", "2024-01-31T12:00:00"),
            ("time", "12:00:00"),
            ("duration", "P1D"),
            ("gYear", "2024"),
            ("gYearMonth", "2024-01"),
            ("gMonth", "--01"),
            ("gMonthDay", "--01-31"),
            ("gDay", "---31"),
            ("hexBinary", "0A"),
            ("base64Binary", "AQID"),
        ] {
            let dt = compile_no_params(type_name);
            assert!(dt.is_valid(value), "{type_name} {value:?}");
            let padded = format!(" \t\r\n{value}\n ");
            assert!(dt.is_valid(&padded), "{type_name} {padded:?}");
            // Only XML whitespace is collapsed
            let nbsp = format!("\u{A0}{value}");
            assert!(!dt.is_valid(&nbsp), "{type_name} {nbsp:?}");
        }
    }

    #[test]
    fn date_lexical_form_is_strict() {
        let dt = compile_no_params("date");
        for v in [
            "2024-01-31",
            "-0001-01-01",
            "2024-01-31Z",
            "2024-01-31+14:00",
            "2024-01-31-14:00",
            "2024-01-31-05:30",
            "12345-01-01",
            "99999999999-12-31",
            "2024-02-29",
            "2000-02-29",
            "-0004-02-29",
            "10000-02-29",
        ] {
            assert!(dt.is_valid(v), "{v:?}");
        }
        for v in [
            "2024-02-30",
            "2024- 01-31",
            "2024-1-31",
            "024-01-31",
            "+2024-01-31",
            "\u{2003}2024-01-31",
            "0000-01-01",
            "-0000-01-01",
            "012345-01-01",
            "2024-00-01",
            "2024-13-01",
            "2024-01-00",
            "2024-04-31",
            "2023-02-29",
            "1900-02-29",
            "1500-02-29",
            "-0001-02-29",
            "12100-02-29",
            "2024-01-31T00:00:00",
            "2024-01-31Z+01:00",
            "2024-01-31+0100",
            "2024-01-31+14:01",
            "2024-01-31+15:00",
            "2024-01-31+01:60",
        ] {
            assert!(!dt.is_valid(v), "{v:?}");
        }
    }

    #[test]
    fn datetime_lexical_form() {
        let dt = compile_no_params("dateTime");
        for v in [
            "2024-01-01T00:00:00",
            "2024-01-01T00:00:00Z",
            "2024-01-01T00:00:00+01:00",
            "2024-01-01T00:00:00-00:30",
            "2024-01-01T00:00:00.5",
            "2024-01-01T00:00:00.123456789012Z",
            "-0001-01-01T00:00:00",
            "12345-01-01T00:00:00",
            "99999999999-12-31T23:59:59",
        ] {
            assert!(dt.is_valid(v), "{v:?}");
        }
        for v in [
            "2024-01-01",
            "2024-1-01T00:00:00",
            "024-01-01T00:00:00",
            "+2024-01-01T00:00:00",
            "012345-01-01T00:00:00",
            "0000-01-01T00:00:00",
            "-0000-01-01T00:00:00",
            "2024-01-01T00:00",
            "2024-01-01T00:00:00.",
            "2024-01-01T00:00:00Z+01:00",
            "2024-01-01T00:00:00+0100",
            "2024-01-01t00:00:00",
        ] {
            assert!(!dt.is_valid(v), "{v:?}");
        }
    }

    #[test]
    fn datetime_date_must_exist() {
        let dt = compile_no_params("dateTime");
        for v in [
            "2024-01-31T00:00:00",
            "2024-04-30T00:00:00",
            "2024-02-29T00:00:00",
            "2000-02-29T00:00:00",
            "1500-03-01T00:00:00",
            "-0004-02-29T00:00:00",
            "10000-02-29T00:00:00",
            "12004-02-29T00:00:00",
        ] {
            assert!(dt.is_valid(v), "{v:?}");
        }
        for v in [
            "2024-00-01T00:00:00",
            "2024-13-01T00:00:00",
            "2024-01-00T00:00:00",
            "2024-01-32T00:00:00",
            "2024-04-31T00:00:00",
            "2023-02-29T00:00:00",
            "1900-02-29T00:00:00",
            // proleptic Gregorian, not Julian
            "1500-02-29T00:00:00",
            // XSD 1.0 applies the leap year rule to the year number as written
            "-0001-02-29T00:00:00",
            "-0005-02-29T00:00:00",
            "12100-02-29T00:00:00",
        ] {
            assert!(!dt.is_valid(v), "{v:?}");
        }
    }

    #[test]
    fn datetime_time_and_timezone_ranges() {
        let dt = compile_no_params("dateTime");
        for v in [
            "2024-01-01T23:59:59.999",
            "2024-01-01T24:00:00",
            "2024-01-01T24:00:00.000",
            "2024-12-31T23:59:60",
            "2024-01-01T00:00:00+14:00",
            "2024-01-01T00:00:00-14:00",
            "2024-01-01T00:00:00-13:59",
        ] {
            assert!(dt.is_valid(v), "{v:?}");
        }
        for v in [
            "2024-01-01T25:00:00",
            "2024-01-01T24:00:01",
            "2024-01-01T24:01:00",
            "2024-01-01T24:00:00.1",
            "2024-01-01T23:60:00",
            "2024-01-01T23:59:61",
            "2024-01-01T00:00:00+14:01",
            "2024-01-01T00:00:00+15:00",
            "2024-01-01T00:00:00+01:60",
        ] {
            assert!(!dt.is_valid(v), "{v:?}");
        }
    }

    #[test]
    fn facets_see_collapsed_value() {
        let dt = compile_one_param("integer", "pattern", "[0-9]+");
        assert!(dt.is_valid(" 42\n"));
        let dt = compile_one_param("NCName", "maxLength", "3");
        assert!(dt.is_valid("  foo  "));
        let dt = compile_one_param("decimal", "totalDigits", "2");
        assert!(dt.is_valid(" 12 "));
        let dt = compile_one_param("NMTOKENS", "pattern", "a b");
        assert!(dt.is_valid("\ta\n\nb "));
    }

    #[test]
    fn string_preserves_whitespace() {
        let dt = compile_one_param("string", "length", "3");
        assert!(dt.is_valid(" a\n"));
        assert!(!dt.is_valid("a"));
    }

    #[test]
    fn qname_valid() {
        let dt = compile_no_params("QName");
        assert!(dt.is_valid("foo"));
        assert!(dt.is_valid("style:font-face"));
        assert!(dt.is_valid(" eg:foo ")); // surrounding whitespace is collapsed
        assert!(!dt.is_valid("a:b:c")); // at most one colon
        assert!(!dt.is_valid(":foo")); // empty prefix
        assert!(!dt.is_valid("eg:")); // empty local part
        assert!(!dt.is_valid("1a:foo")); // prefix must be an NCName
        assert!(!dt.is_valid(""));
    }

    #[test]
    fn qname_pattern_facet() {
        let dt = compile_one_param("QName", "pattern", "[a-z]+:[a-z\\-]+");
        assert!(dt.is_valid("style:font-face"));
        assert!(!dt.is_valid("font-face")); // pattern requires a prefix
        assert!(dt.is_valid(" style:font-face ")); // facets see the collapsed value
    }

    #[test]
    fn qname_length_facet_ignores_surrounding_whitespace() {
        let dt = compile_one_param("QName", "length", "5");
        assert!(dt.is_valid(" eg:ab "));
        assert!(!dt.is_valid("eg:abc"));
    }

    #[test]
    fn qname_prefix_resolution() {
        let dt = compile_no_params("QName");
        let lookup = |p: &str| (p == "eg").then(|| "http://example.com/ns".to_string());
        assert!(dt.is_valid_with_ns("eg:foo", lookup));
        assert!(dt.is_valid_with_ns("foo", lookup));
        assert!(!dt.is_valid_with_ns("other:foo", lookup));
        assert!(!dt.is_valid_with_ns("a:b:c", lookup));
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

    fn compile_value(name: &str, value: &str) -> Result<XsdDatatypeValues, XsdDatatypeError> {
        let mut map = CodeMap::new();
        let file = map.add_file("test.rnc".to_string(), "test".to_string());
        let ctx = Context::new(file);
        Compiler.compile_value(&ctx, &(0..0), name, value, None, &[])
    }

    #[test]
    fn typed_values_compare_in_value_space() {
        // (type, value in schema, instance text, expected match). The expectations were checked
        // against Jing.
        let cases = [
            ("byte", "01", "1", true),
            ("byte", "01", "+1", true),
            ("byte", "1", " 1 ", true),
            ("byte", "1", "2", false),
            ("short", "-5", "-05", true),
            (
                "unsignedLong",
                "18446744073709551615",
                "18446744073709551615",
                true,
            ),
            ("nonNegativeInteger", "255", "0255", true),
            ("positiveInteger", "1", "01", true),
            ("integer", "0", "-0", true),
            ("integer", "1", "1.0", false),
            ("decimal", "1.0", "1", true),
            ("decimal", "1.50", "+01.5", true),
            ("decimal", "0", "-0.0", true),
            ("decimal", ".5", "0.5", true),
            ("decimal", "0.5", "0.51", false),
            ("float", "NaN", "NaN", true),
            ("float", "0", "-0", true),
            ("float", "1", "1.0E0", true),
            ("float", "0.1", "0.10000000149011612", true),
            ("float", "INF", "INF", true),
            ("float", "INF", "-INF", false),
            ("double", "1e308", "1E308", true),
            (
                "double",
                "0.1",
                "0.1000000000000000055511151231257827",
                true,
            ),
            ("double", "0.1", "0.10000000149011612", false),
            ("boolean", "true", "1", true),
            ("boolean", "false", "0", true),
            ("boolean", "true", "false", false),
            ("language", "en-GB", "en-gb", false),
            ("language", "en-GB", " en-GB ", true),
            ("normalizedString", "a b", "a\tb", true),
            ("anyURI", "http://x/%7e", "http://x/~", false),
            ("anyURI", " http://x/ ", "http://x/", true),
            ("NMTOKEN", "a", " a ", true),
            ("NCName", "a", "b", false),
            ("hexBinary", "0a", "0A", true),
            ("hexBinary", "0a", "0b", false),
            ("base64Binary", "QUJD", "QU JD", true),
            ("base64Binary", "AA==", "AA==", true),
            ("base64Binary", "AA==", "AQ==", false),
            ("NMTOKENS", "a  b", " a b", true),
            ("NMTOKENS", "a b", "b a", false),
            ("IDREFS", "a b", "a  b", true),
            ("duration", "P1D", "PT24H", false),
            ("duration", "P1Y", "P12M", false),
            ("duration", "PT60S", "PT1M", false),
            ("duration", "P1M", "P30D", false),
            ("duration", "PT1.0S", "PT1S", true),
            ("duration", "P1Y2M", "P1Y2M0D", true),
            (
                "dateTime",
                "2000-01-01T00:00:00Z",
                "2000-01-01T01:00:00+01:00",
                true,
            ),
            (
                "dateTime",
                "2000-01-01T00:00:00Z",
                "1999-12-31T23:00:00-01:00",
                true,
            ),
            (
                "dateTime",
                "2000-01-01T00:00:00",
                "2000-01-01T00:00:00Z",
                false,
            ),
            (
                "dateTime",
                "2000-01-01T00:00:00",
                "2000-01-01T00:00:00.000",
                true,
            ),
            (
                "dateTime",
                "2000-01-02T00:00:00",
                "2000-01-01T24:00:00",
                true,
            ),
            (
                "dateTime",
                "2001-01-01T00:00:00Z",
                "2000-12-31T23:59:00-00:01",
                true,
            ),
            (
                "dateTime",
                "-0001-12-31T23:00:00Z",
                "0001-01-01T00:00:00+01:00",
                true,
            ),
            ("date", "2000-01-01Z", "2000-01-01+00:00", true),
            ("date", "2000-01-01+01:00", "2000-01-01Z", false),
            ("date", "2000-01-02+12:00", "2000-01-01-12:00", true),
            ("time", "00:00:00Z", "01:00:00+01:00", true),
            ("time", "00:00:00Z", "23:00:00-01:00", false),
            ("time", "12:00:00", "12:00:00.0", true),
            ("gYear", "2000Z", "2000+00:00", true),
            ("gYear", "2000+01:00", "2000Z", false),
            ("gYear", "2000", "2001", false),
            ("gMonth", "--01", "--01", true),
            ("gDay", "---01Z", "---01-00:00", true),
            ("gMonthDay", "--02-29", "--02-29", true),
            ("gYearMonth", "2000-01Z", "2000-01Z", true),
        ];
        for (type_name, schema_value, instance, expected) in cases {
            let v = compile_value(type_name, schema_value)
                .unwrap_or_else(|e| panic!("{type_name} {schema_value:?}: {e:?}"));
            assert_eq!(
                v.is_valid(instance),
                expected,
                "xsd:{type_name} {schema_value:?} against {instance:?}"
            );
        }
    }

    #[test]
    fn typed_value_must_be_valid_for_its_type() {
        for (type_name, value) in [
            ("byte", "128"),
            ("unsignedByte", "-1"),
            ("positiveInteger", "0"),
            ("boolean", "yes"),
            ("integer", "1_000"),
            ("decimal", "1e5"),
            ("hexBinary", "0"),
            ("date", "2001-02-29"),
            ("gMonthDay", "--02-30"),
            ("time", "25:00:00"),
            ("NCName", "a:b"),
        ] {
            assert_matches!(
                compile_value(type_name, value),
                Err(XsdDatatypeError::InvalidValueOfType { type_name: t, .. }) if t == type_name,
                "xsd:{type_name} {value:?}"
            );
        }
        assert_matches!(
            compile_value("NOTATION", "x"),
            Err(XsdDatatypeError::UnsupportedDatatype { .. })
        );
    }

    #[test]
    fn lexical_spaces_match_jing() {
        // (type, text, expected validity), checked against Jing
        let cases = [
            ("float", "-INF", true),
            ("float", "+INF", false),
            ("float", "inf", false),
            ("float", "Infinity", false),
            ("float", "-NaN", false),
            ("float", "1.", true),
            ("float", ".5", true),
            ("float", "+1.5E+3", true),
            ("float", ".", false),
            ("decimal", "1e5", false),
            ("decimal", "+.5", true),
            ("decimal", "1.", true),
            ("decimal", ".", false),
            ("duration", "-P1D", true),
            ("duration", "P1Dx", false),
            ("duration", "P", false),
            ("duration", "PT", false),
            ("duration", "P1DT", false),
            ("duration", "PT1.S", true),
            ("duration", "PT.5S", true),
            ("duration", "P1.5D", false),
            ("duration", "P-1D", false),
            ("duration", "0001-01-01T10:30:00", false),
            ("time", "99:00:00", false),
            ("time", "12:60:00", false),
            ("time", "23:59:60", true),
            ("time", "12:00:00+14:01", false),
            ("gMonthDay", "--02-29", true),
            ("gMonthDay", "--02-30", false),
            ("gMonthDay", "--13-01", false),
            ("gDay", "---31", true),
            ("gDay", "---32", false),
            ("gDay", "---00", false),
            ("gMonth", "--13", false),
            ("gMonth", "--02--", false),
            ("gYear", "10000", true),
            ("gYear", "-0001", true),
            ("gYear", "0000", false),
            ("gYear", "-0000", false),
            ("gYear", "02000", false),
            ("gYearMonth", "2000-13", false),
            ("nonNegativeInteger", "+5", true),
            ("nonNegativeInteger", "-0", true),
            ("unsignedByte", "-0", true),
            ("positiveInteger", "+1", true),
            ("nonPositiveInteger", "+0", true),
            ("integer", "1_000", false),
            ("hexBinary", "+a", false),
        ];
        for (type_name, text, expected) in cases {
            assert_eq!(
                compile_no_params(type_name).is_valid(text),
                expected,
                "xsd:{type_name} {text:?}"
            );
        }

        // (type, facet, facet value, text, expected validity)
        let cases = [
            ("normalizedString", "maxLength", "3", " a\t", true),
            ("normalizedString", "pattern", "a b", "a\tb", true),
            ("decimal", "totalDigits", "2", "1.50", false),
            ("decimal", "totalDigits", "1", "+01", true),
            ("decimal", "totalDigits", "2", "100", false),
            ("decimal", "fractionDigits", "1", "1.50", false),
            ("float", "maxInclusive", "1e2", "50", true),
            ("float", "minInclusive", "0", "NaN", false),
            ("int", "pattern", "0.*", " 05", true),
        ];
        for (type_name, facet, facet_value, text, expected) in cases {
            assert_eq!(
                compile_one_param(type_name, facet, facet_value).is_valid(text),
                expected,
                "xsd:{type_name} {{ {facet} = {facet_value:?} }} {text:?}"
            );
        }
    }
}
