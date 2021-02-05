//! The RELAX NG built-in datatype library

//use relaxng_model::model::{DatatypeName, Param, Span};
use crate::Context;
use relaxng_syntax::types::NamespacedName;
use relaxng_syntax::types::{DatatypeName, NcName, Param, QName};

// TODO: return Cow
pub fn normalize_whitespace(val: &str) -> String {
    // TODO: return Cow to optimise the case when input does not required modification?
    let mut last_space = false;
    let mut out = String::new();
    for c in val.chars().skip_while(|c| c.is_ascii_whitespace()) {
        if c.is_ascii_whitespace() {
            last_space = true;
        } else {
            if last_space {
                out.push(' ');
            }
            out.push(c);
            last_space = false;
        }
    }
    out
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum BuiltinDatatypeValue {
    TokenValue(String),
    StringValue(String),
}
impl super::Datatype for BuiltinDatatypeValue {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            BuiltinDatatypeValue::TokenValue(val) => val == &normalize_whitespace(value),
            BuiltinDatatypeValue::StringValue(val) => val == value,
        }
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum BuiltinDatatype {
    // TODO: do we actually need to distinguish 'Token' and 'String' cases?
    Token,
    String,
}
impl super::Datatype for BuiltinDatatype {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            BuiltinDatatype::Token => {
                // TODO: assert valid token
                true
            }
            BuiltinDatatype::String => true,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ParamNotAllowed { span: codemap::Span, name: String },
    DatataypeNameUnknown { span: codemap::Span, name: String },
}

#[derive(Default)]
pub struct Compiler;
impl super::DatatypeCompiler for Compiler {
    type DT = BuiltinDatatype;
    type DTValue = BuiltinDatatypeValue;
    type Error = Error;

    fn datatype_value(
        &self,
        ctx: &Context,
        name: &DatatypeName,
        value: &str,
    ) -> Result<Self::DTValue, Self::Error> {
        Ok(match name {
            DatatypeName::String => BuiltinDatatypeValue::StringValue(value.to_string()),
            DatatypeName::Token => BuiltinDatatypeValue::TokenValue(normalize_whitespace(value)),
            DatatypeName::CName(QName(_, NcName(_, name))) if name == "string" => {
                BuiltinDatatypeValue::StringValue(value.to_string())
            }
            DatatypeName::CName(QName(_, NcName(_, name))) if name == "token" => {
                BuiltinDatatypeValue::TokenValue(normalize_whitespace(value))
            }
            DatatypeName::CName(QName(prefix, localname)) => {
                return Err(Error::DatataypeNameUnknown {
                    span: ctx.convert_span(&localname.0),
                    name: localname.1.clone(),
                })
            }
            DatatypeName::NamespacedName(NamespacedName { localname, .. }) => {
                return Err(Error::DatataypeNameUnknown {
                    span: ctx.convert_span(&localname.0),
                    name: localname.1.clone(),
                })
            }
        })
    }

    fn datatype_name(
        &self,
        ctx: &Context,
        name: &DatatypeName,
        params: &[Param],
    ) -> Result<Self::DT, Self::Error> {
        if !params.is_empty() {
            return Err(Error::ParamNotAllowed {
                span: ctx.convert_span(&params[0].0),
                name: params[0].2.to_string(),
            });
        }
        Ok(match name {
            DatatypeName::String => BuiltinDatatype::String,
            DatatypeName::Token => BuiltinDatatype::Token,
            DatatypeName::CName(QName(_, NcName(_, name))) if name == "string" => {
                BuiltinDatatype::String
            }
            DatatypeName::CName(QName(_, NcName(_, name))) if name == "token" => {
                BuiltinDatatype::Token
            }
            DatatypeName::CName(QName(prefix, localname)) => {
                return Err(Error::DatataypeNameUnknown {
                    span: ctx.convert_span(&localname.0),
                    name: localname.1.clone(),
                })
            }
            DatatypeName::NamespacedName(NamespacedName { localname, .. }) => {
                return Err(Error::DatataypeNameUnknown {
                    span: ctx.convert_span(&localname.0),
                    name: localname.1.clone(),
                })
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_normalize() {
        assert_eq!(normalize_whitespace(""), "");
        assert_eq!(normalize_whitespace(" "), "");
        assert_eq!(normalize_whitespace("  "), "");
        assert_eq!(normalize_whitespace("a "), "a");
        assert_eq!(normalize_whitespace(" a"), "a");
        assert_eq!(normalize_whitespace("a a"), "a a");
        assert_eq!(normalize_whitespace("\na\t a\r"), "a a");
    }
}
