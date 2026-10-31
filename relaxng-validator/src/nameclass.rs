use relaxng_model::datatype;
use relaxng_model::model::NameClass;

use crate::infoset::{ExpandedName, NameTable};

/// Id of a local name the schema mentions.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NameId(u32);

/// Id of a namespace URI the schema mentions, with the empty URI standing for no namespace.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NsId(u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum LocalKey {
    Known(NameId),
    /// A valid NCName that the schema does not mention.
    Other,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum NsKey {
    Known(NsId),
    /// A namespace that the schema does not mention.
    Other,
}

/// A document's element or attribute name, as far as matching against the schema is concerned.
///
/// Every name class compares a name with the schema's names only for equality, so all the names
/// that a schema does not mention match exactly the same name classes, and share a key. The keys
/// a document can produce are therefore bounded by the schema, however many distinct names the
/// document uses.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NameKey {
    // Private, so that only `SchemaNames::key` can make one, and every name that reaches a
    // derivation has been checked to be a valid NCName
    local: LocalKey,
    ns: NsKey,
}

/// The local names and namespace URIs that a schema's name classes mention.
#[derive(Default)]
pub(crate) struct SchemaNames {
    local_names: NameTable,
    /// Indexed by `NsId`. Schemas mention few namespaces, so a linear search is cheaper than
    /// hashing the whole of a document's URI, and it can only be as long as the schema makes it.
    namespaces: Vec<Box<str>>,
}

impl SchemaNames {
    fn find_namespace(&self, uri: &str) -> Option<NsId> {
        let pos = self.namespaces.iter().position(|u| **u == *uri)?;
        Some(NsId(pos as u32))
    }

    fn namespace(&mut self, uri: &str) -> NsId {
        self.find_namespace(uri).unwrap_or_else(|| {
            self.namespaces.push(uri.into());
            NsId(self.namespaces.len() as u32 - 1)
        })
    }

    /// The key for `name`, or `None` if its local name is not a valid NCName, so that no name
    /// class can match it.
    pub(crate) fn key(&self, name: &ExpandedName<'_>) -> Option<NameKey> {
        let local = match name.local_name.lookup(&self.local_names) {
            Some(id) => LocalKey::Known(NameId(id)),
            None if name.local_name.is_valid() => LocalKey::Other,
            None => return None,
        };
        let ns = match self.find_namespace(name.namespace_uri.unwrap_or("")) {
            Some(id) => NsKey::Known(id),
            None => NsKey::Other,
        };
        Some(NameKey { local, ns })
    }
}

/// A [`NameClass`] with its names replaced by ids, for matching against a [`NameKey`].
pub(crate) enum NameMatcher {
    Named {
        ns: NsId,
        local: NameId,
    },
    NsName {
        ns: NsId,
        except: Option<Box<NameMatcher>>,
    },
    AnyName {
        except: Option<Box<NameMatcher>>,
    },
    Alt(Box<NameMatcher>, Box<NameMatcher>),
}

impl NameMatcher {
    /// Compiles `nc`, adding the names it mentions to `names`.
    ///
    /// # Panics
    ///
    /// If `nc` names an element or attribute with something that is not a valid NCName, which
    /// the schema parsers never produce. See [`NameTable::insert`].
    pub(crate) fn compile(nc: &NameClass, names: &mut SchemaNames) -> NameMatcher {
        let except = |except: &Option<Box<NameClass>>, names: &mut SchemaNames| {
            except
                .as_ref()
                .map(|nc| Box::new(NameMatcher::compile(nc, names)))
        };
        match nc {
            NameClass::Named {
                namespace_uri,
                name,
            } => NameMatcher::Named {
                ns: names.namespace(namespace_uri),
                local: NameId(names.local_names.insert(name)),
            },
            NameClass::NsName {
                namespace_uri,
                except: e,
            } => NameMatcher::NsName {
                ns: names.namespace(namespace_uri),
                except: except(e, names),
            },
            NameClass::AnyName { except: e } => NameMatcher::AnyName {
                except: except(e, names),
            },
            NameClass::Alt { a, b } => NameMatcher::Alt(
                Box::new(NameMatcher::compile(a, names)),
                Box::new(NameMatcher::compile(b, names)),
            ),
        }
    }

    pub(crate) fn matches(&self, name: NameKey) -> bool {
        let excluded =
            |except: &Option<Box<NameMatcher>>| except.as_ref().is_some_and(|nc| nc.matches(name));
        match self {
            NameMatcher::Named { ns, local } => {
                name.ns == NsKey::Known(*ns) && name.local == LocalKey::Known(*local)
            }
            NameMatcher::NsName { ns, except } => name.ns == NsKey::Known(*ns) && !excluded(except),
            NameMatcher::AnyName { except } => !excluded(except),
            NameMatcher::Alt(a, b) => a.matches(name) || b.matches(name),
        }
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
                QName(_) => "xsd:QName",
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
