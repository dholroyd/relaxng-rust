use relaxng_model::datatype;
use relaxng_model::model::NameClass;

#[derive(Copy, Clone, Debug)]
pub(crate) struct QualifiedName<'a> {
    pub(crate) namespace_uri: Option<&'a str>,
    /// Local name as raw bytes. For `Named` matches, byte equality against the
    /// schema's UTF-8 name is sufficient. For wildcard matches (`AnyName`/`NsName`),
    /// `contains()` validates that this is a legal XML NCName.
    pub(crate) local_name: &'a [u8],
}

fn is_ns_match(namespace_uri: &str, target_namespace: Option<&str>) -> bool {
    if let Some(target_namespace) = target_namespace {
        target_namespace == namespace_uri
    } else {
        namespace_uri.is_empty()
    }
}

/// Check if `bytes` is a valid XML NCName (Name without colons).
/// See <https://www.w3.org/TR/xml-names/#NT-NCName>.
pub(crate) fn is_ncname(bytes: &[u8]) -> bool {
    let s = match std::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => return false,
    };
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if is_name_start_char(c) => {}
        _ => return false,
    }
    chars.all(is_name_char)
}

/// XML NameStartChar (excluding ':'): Letter | '_'
/// See <https://www.w3.org/TR/xml/#NT-NameStartChar>.
fn is_name_start_char(c: char) -> bool {
    matches!(c,
        'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}'
    )
}

/// XML NameChar (excluding ':'): NameStartChar | '-' | '.' | digit | combining/extender
/// See <https://www.w3.org/TR/xml/#NT-NameChar>.
fn is_name_char(c: char) -> bool {
    is_name_start_char(c)
        || matches!(c,
            '-'
            | '.'
            | '0'..='9'
            | '\u{B7}'
            | '\u{0300}'..='\u{036F}'
            | '\u{203F}'..='\u{2040}'
        )
}

pub(crate) fn contains(nc: &NameClass, target_name: &QualifiedName) -> bool {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => {
            // Byte equality: if the schema name is a valid NCName (guaranteed by
            // the schema compiler), matching bytes implies a valid name.
            if let Some(target_namespace) = target_name.namespace_uri {
                target_namespace == namespace_uri && target_name.local_name == name.as_bytes()
            } else {
                namespace_uri.is_empty() && target_name.local_name == name.as_bytes()
            }
        }
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            if !is_ncname(target_name.local_name) {
                return false;
            }
            if is_ns_match(namespace_uri, target_name.namespace_uri) {
                if let Some(except_nameclass) = except {
                    !contains(except_nameclass, target_name)
                } else {
                    true
                }
            } else {
                false
            }
        }
        NameClass::AnyName { except } => {
            if !is_ncname(target_name.local_name) {
                return false;
            }
            match except {
                None => true,
                Some(nc) => !contains(nc, target_name),
            }
        }
        NameClass::Alt { a, b } => contains(a, target_name) || contains(b, target_name),
    }
}

pub(crate) fn describe_nameclass(nc: &NameClass, desc: &mut String) {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => {
            if !namespace_uri.is_empty() {
                desc.push('{');
                desc.push_str(namespace_uri);
                desc.push('}');
            }
            desc.push_str(name);
        }
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            desc.push_str(namespace_uri);
            desc.push_str(":*");
            if let Some(except) = except {
                desc.push('-');
                describe_nameclass(except, desc);
            }
        }
        NameClass::AnyName { except } => {
            desc.push('*');
            if let Some(except) = except {
                desc.push('-');
                describe_nameclass(except, desc);
            }
        }
        NameClass::Alt { a, b } => {
            describe_nameclass(a, desc);
            desc.push('|');
            describe_nameclass(b, desc);
        }
    }
}

pub(crate) fn describe_datatype(dt: &datatype::Datatypes) -> String {
    match dt {
        datatype::Datatypes::Relax(r) => match r {
            datatype::relax::BuiltinDatatype::Token => "token".to_string(),
            datatype::relax::BuiltinDatatype::String => "string".to_string(),
        },
        datatype::Datatypes::Xsd(x) => {
            use datatype::xsd::XsdDatatypes::*;
            let name = match x {
                String(_) => "xsd:string",
                NormalizedString(_) => "xsd:normalizedString",
                Token(_) => "xsd:token",
                Byte(..) => "xsd:byte",
                Short(..) => "xsd:short",
                UnsignedByte(..) => "xsd:unsignedByte",
                UnsignedShort(..) => "xsd:unsignedShort",
                Int(..) => "xsd:int",
                Integer(..) => "xsd:integer",
                Long(..) => "xsd:long",
                UnsignedInt(..) => "xsd:unsignedInt",
                UnsignedLong(..) => "xsd:unsignedLong",
                PositiveInteger(..) => "xsd:positiveInteger",
                NonNegativeInteger(..) => "xsd:nonNegativeInteger",
                NonPositiveInteger(..) => "xsd:nonPositiveInteger",
                NegativeInteger(..) => "xsd:negativeInteger",
                Decimal { .. } => "xsd:decimal",
                Float(..) => "xsd:float",
                Double(..) => "xsd:double",
                NmTokens(..) => "xsd:NMTOKENS",
                NmToken(_) => "xsd:NMTOKEN",
                NcName(_) => "xsd:NCName",
                Name(_) => "xsd:Name",
                Duration(_) => "xsd:duration",
                Date(_) => "xsd:date",
                Datetime(_) => "xsd:dateTime",
                Time(_) => "xsd:time",
                GYearMonth(_) => "xsd:gYearMonth",
                GYear(_) => "xsd:gYear",
                GMonthDay(_) => "xsd:gMonthDay",
                GDay(_) => "xsd:gDay",
                GMonth(_) => "xsd:gMonth",
                HexBinary(..) => "xsd:hexBinary",
                Base64Binary(..) => "xsd:base64Binary",
                AnyURI(_) => "xsd:anyURI",
                Language(_) => "xsd:language",
                Boolean(_) => "xsd:boolean",
                Id(_) => "xsd:ID",
                IdRef(_) => "xsd:IDREF",
                IdRefs(..) => "xsd:IDREFS",
                Entity(_) => "xsd:ENTITY",
                Entities(..) => "xsd:ENTITIES",
            };
            name.to_string()
        }
    }
}

pub(crate) fn describe_datatype_value(dt: &datatype::DatatypeValues) -> String {
    match dt {
        datatype::DatatypeValues::Relax(r) => match r {
            datatype::relax::BuiltinDatatypeValue::TokenValue(s)
            | datatype::relax::BuiltinDatatypeValue::StringValue(s) => {
                format!("\"{}\"", s)
            }
        },
        datatype::DatatypeValues::Xsd(x) => {
            use datatype::xsd::XsdDatatypeValues::*;
            match x {
                String(s) | Token(s) | Name(s) => format!("\"{}\"", s),
                QName(q) => format!("{:?}", q),
            }
        }
    }
}
