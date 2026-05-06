use arbitrary::Unstructured;
use relaxng_model::datatype::{
    DatatypeValues, Datatypes,
    relax::{BuiltinDatatype, BuiltinDatatypeValue},
    xsd::{XsdDatatypeValues, XsdDatatypes},
};

/// Generate a concrete string value for a `DatatypeValue` constraint (exact match required).
pub fn generate_datatype_value(dt: &DatatypeValues) -> String {
    match dt {
        DatatypeValues::Relax(relax) => match relax {
            BuiltinDatatypeValue::TokenValue(s) | BuiltinDatatypeValue::StringValue(s) => s.clone(),
        },
        DatatypeValues::Xsd(xsd) => match xsd {
            XsdDatatypeValues::String(s)
            | XsdDatatypeValues::Token(s)
            | XsdDatatypeValues::Name(s) => s.clone(),
            XsdDatatypeValues::QName(qn) => qn.localname.clone(),
        },
    }
}

/// Generate a valid string value for a `Datatypes` constraint.
pub fn generate_datatype(dt: &Datatypes, u: &mut Unstructured) -> String {
    match dt {
        Datatypes::Relax(relax) => generate_relax_datatype(relax, u),
        Datatypes::Xsd(xsd) => generate_xsd_datatype(xsd, u),
    }
}

fn generate_relax_datatype(dt: &BuiltinDatatype, u: &mut Unstructured) -> String {
    match dt {
        BuiltinDatatype::String | BuiltinDatatype::Token => gen_short_ascii(u),
    }
}

fn generate_xsd_datatype(dt: &XsdDatatypes, u: &mut Unstructured) -> String {
    use relaxng_model::datatype::xsd::*;
    match dt {
        XsdDatatypes::String(_) | XsdDatatypes::NormalizedString(_) | XsdDatatypes::Token(_) => {
            gen_short_ascii(u)
        }
        XsdDatatypes::Boolean(_) => {
            if u.arbitrary::<bool>().unwrap_or(false) {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        XsdDatatypes::Short(min_max, _) => {
            let lo = min_max.min().unwrap_or(i16::MIN) as i32;
            let hi = min_max.max().unwrap_or(i16::MAX) as i32;
            let range = (hi - lo).max(0) as u32;
            let v = lo + u.int_in_range(0u32..=range).unwrap_or(0) as i32;
            v.to_string()
        }
        XsdDatatypes::UnsignedShort(min_max, _) => {
            let lo = min_max.min().unwrap_or(0u16) as u32;
            let hi = min_max.max().unwrap_or(u16::MAX) as u32;
            let range = (hi - lo).max(0);
            let v = lo + u.int_in_range(0u32..=range).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::Long(min_max, _) => {
            let lo = min_max.min().unwrap_or(-1_000_000i64);
            let hi = min_max.max().unwrap_or(1_000_000i64);
            let range = (hi - lo).max(0) as u64;
            let v = lo + u.int_in_range(0u64..=range).unwrap_or(0) as i64;
            v.to_string()
        }
        XsdDatatypes::Int(min_max, _) => {
            let lo = min_max.min().unwrap_or(-1_000_000i32);
            let hi = min_max.max().unwrap_or(1_000_000i32);
            let range = (hi - lo).max(0) as u32;
            let v = lo + u.int_in_range(0u32..=range).unwrap_or(0) as i32;
            v.to_string()
        }
        XsdDatatypes::Integer(min_max, _) => {
            // BigInt: use cloned accessors
            let lo = min_max
                .min_cloned()
                .unwrap_or_else(|| (-1_000_000i64).into());
            let hi = min_max.max_cloned().unwrap_or_else(|| 1_000_000i64.into());
            let offset = num_bigint::BigInt::from(u.int_in_range(0u64..=1_000_000u64).unwrap_or(0));
            let v = lo + offset;
            if v <= hi { v } else { hi }.to_string()
        }
        XsdDatatypes::PositiveInteger(min_max, _) => {
            let lo: num_bigint::BigUint = min_max.min_cloned().unwrap_or_else(|| 1u64.into());
            let hi = min_max.max_cloned().unwrap_or_else(|| 1_000_001u64.into());
            let offset =
                num_bigint::BigUint::from(u.int_in_range(0u64..=1_000_000u64).unwrap_or(0));
            let v = lo + offset;
            if v <= hi { v } else { hi }.to_string()
        }
        XsdDatatypes::UnsignedInt(min_max, _) => {
            let lo = min_max.min().unwrap_or(0u32);
            let hi = min_max.max().unwrap_or(1_000_000u32);
            let range = (hi - lo).max(0);
            let v = lo + u.int_in_range(0u32..=range).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::UnsignedLong(min_max, _) => {
            let lo = min_max.min().unwrap_or(0u64);
            let hi = min_max.max().unwrap_or(1_000_000u64);
            let range = (hi - lo).max(0);
            let v = lo + u.int_in_range(0u64..=range).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::Decimal { min_max, .. } => {
            let lo = min_max
                .min_cloned()
                .unwrap_or_else(|| bigdecimal::BigDecimal::from(-1_000_000i64));
            let offset =
                bigdecimal::BigDecimal::from(u.int_in_range(0u32..=1_000_000u32).unwrap_or(0));
            let v = lo + offset;
            v.to_string()
        }
        XsdDatatypes::Double(..) => {
            let v = u.int_in_range(-1000i32..=1000i32).unwrap_or(0);
            format!("{}.0", v)
        }
        XsdDatatypes::NcName(_) => gen_ncname(u),
        XsdDatatypes::NmToken(_) => gen_ncname(u),
        XsdDatatypes::NmTokens(..) => {
            let n = u.int_in_range(1usize..=3usize).unwrap_or(1);
            (0..n).map(|_| gen_ncname(u)).collect::<Vec<_>>().join(" ")
        }
        XsdDatatypes::Date(_) => {
            let year = u.int_in_range(2000u32..=2030u32).unwrap_or(2024);
            let month = u.int_in_range(1u32..=12u32).unwrap_or(1);
            let day = u.int_in_range(1u32..=28u32).unwrap_or(1);
            format!("{:04}-{:02}-{:02}", year, month, day)
        }
        XsdDatatypes::Datetime(_) => {
            let year = u.int_in_range(2000u32..=2030u32).unwrap_or(2024);
            let month = u.int_in_range(1u32..=12u32).unwrap_or(1);
            let day = u.int_in_range(1u32..=28u32).unwrap_or(1);
            let hour = u.int_in_range(0u32..=23u32).unwrap_or(0);
            let min = u.int_in_range(0u32..=59u32).unwrap_or(0);
            let sec = u.int_in_range(0u32..=59u32).unwrap_or(0);
            format!(
                "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}",
                year, month, day, hour, min, sec
            )
        }
        XsdDatatypes::Duration(_) => "P1Y".to_string(),
        XsdDatatypes::AnyURI(_) => {
            const URIS: &[&str] = &[
                "http://example.com",
                "http://example.org/path",
                "urn:example:value",
            ];
            u.choose(URIS)
                .copied()
                .unwrap_or("http://example.com")
                .to_string()
        }
        XsdDatatypes::Language(_) => {
            const LANGS: &[&str] = &["en", "fr", "de", "es", "en-US", "zh"];
            u.choose(LANGS).copied().unwrap_or("en").to_string()
        }
        XsdDatatypes::Id(_) | XsdDatatypes::IdRef(_) | XsdDatatypes::Entity(_) => gen_ncname(u),
        XsdDatatypes::Name(_) => gen_ncname(u),
        XsdDatatypes::Byte(_, _) => {
            let v = u.int_in_range(i8::MIN as i32..=i8::MAX as i32).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::UnsignedByte(min_max, _) => {
            let lo = min_max.min().unwrap_or(0u8) as u32;
            let hi = min_max.max().unwrap_or(u8::MAX) as u32;
            let range = (hi - lo).max(0);
            let v = lo + u.int_in_range(0u32..=range).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::NonNegativeInteger(_, _) => {
            let v = u.int_in_range(0u64..=1_000_000u64).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::NonPositiveInteger(_, _) => {
            let v = u.int_in_range(-1_000_000i64..=0i64).unwrap_or(0);
            v.to_string()
        }
        XsdDatatypes::NegativeInteger(_, _) => {
            let v = u.int_in_range(-1_000_000i64..=-1i64).unwrap_or(-1);
            v.to_string()
        }
        XsdDatatypes::Float(..) => {
            let v = u.int_in_range(-1000i32..=1000i32).unwrap_or(0);
            format!("{}.0", v)
        }
        XsdDatatypes::Time(_) => {
            let h = u.int_in_range(0u32..=23u32).unwrap_or(0);
            let m = u.int_in_range(0u32..=59u32).unwrap_or(0);
            let s = u.int_in_range(0u32..=59u32).unwrap_or(0);
            format!("{:02}:{:02}:{:02}", h, m, s)
        }
        XsdDatatypes::GYear(_) => {
            let y = u.int_in_range(2000u32..=2030u32).unwrap_or(2024);
            format!("{:04}", y)
        }
        XsdDatatypes::GYearMonth(_) => {
            let y = u.int_in_range(2000u32..=2030u32).unwrap_or(2024);
            let m = u.int_in_range(1u32..=12u32).unwrap_or(1);
            format!("{:04}-{:02}", y, m)
        }
        XsdDatatypes::GMonth(_) => {
            let m = u.int_in_range(1u32..=12u32).unwrap_or(1);
            format!("--{:02}", m)
        }
        XsdDatatypes::GMonthDay(_) => {
            let m = u.int_in_range(1u32..=12u32).unwrap_or(1);
            let d = u.int_in_range(1u32..=28u32).unwrap_or(1);
            format!("--{:02}-{:02}", m, d)
        }
        XsdDatatypes::GDay(_) => {
            let d = u.int_in_range(1u32..=28u32).unwrap_or(1);
            format!("---{:02}", d)
        }
        XsdDatatypes::HexBinary(..) => {
            let n = u.int_in_range(1usize..=4usize).unwrap_or(1);
            (0..n)
                .map(|_| {
                    let b = u.int_in_range(0u8..=255u8).unwrap_or(0);
                    format!("{:02x}", b)
                })
                .collect::<String>()
        }
        XsdDatatypes::Base64Binary(..) => {
            const ALPHABET: &[u8] =
                b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
            let groups = u.int_in_range(1usize..=2usize).unwrap_or(1);
            let mut out = String::new();
            for _ in 0..groups {
                for _ in 0..4 {
                    out.push(*u.choose(ALPHABET).unwrap_or(&b'A') as char);
                }
            }
            out
        }
        XsdDatatypes::IdRefs(..) | XsdDatatypes::Entities(..) => {
            let n = u.int_in_range(1usize..=3usize).unwrap_or(1);
            (0..n).map(|_| gen_ncname(u)).collect::<Vec<_>>().join(" ")
        }
    }
}

/// Generate a short printable ASCII string (letters, digits, space).
pub fn gen_short_ascii(u: &mut Unstructured) -> String {
    const CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789";
    let len = u.int_in_range(1usize..=8usize).unwrap_or(4);
    (0..len)
        .map(|_| *u.choose(CHARS).unwrap_or(&b'a') as char)
        .collect()
}

/// Generate a valid XML NCName.
pub fn gen_ncname(u: &mut Unstructured) -> String {
    const STARTS: &[u8] = b"abcdefghijklmnopqrstuvwxyz_";
    const CONTINUES: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789_";
    let first = *u.choose(STARTS).unwrap_or(&b'n') as char;
    let len = u.int_in_range(0usize..=4usize).unwrap_or(0);
    let rest: String = (0..len)
        .map(|_| *u.choose(CONTINUES).unwrap_or(&b'a') as char)
        .collect();
    format!("{}{}", first, rest)
}
