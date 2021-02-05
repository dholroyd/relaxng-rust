use crate::Context;
use relaxng_syntax::types;
use relaxng_syntax::types::{DatatypeName, NamespacedName};

pub mod relax;
pub mod xsd;

pub trait Namespaces {
    fn resolve(&self, prefix: &str) -> Option<&str>;
}

pub trait Datatype {
    fn is_valid(&self, value: &str) -> bool;
    // TODO: support producing values in the value-space of the type (e.g. produce the parsed
    //       integer for integer datatypes)
}

pub(crate) trait DatatypeCompiler {
    type DT: Datatype;
    type DTValue: Datatype;
    type Error;

    // TODO: accept type that provides a Span for value, rather than just &str
    fn datatype_value(
        &self,
        ctx: &Context,
        name: &types::DatatypeName,
        value: &str,
    ) -> Result<Self::DTValue, Self::Error>;
    fn datatype_name(
        &self,
        ctx: &Context,
        name: &types::DatatypeName,
        params: &[types::Param],
    ) -> Result<Self::DT, Self::Error>;
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum DatatypeValues {
    Relax(relax::BuiltinDatatypeValue),
    Xsd(xsd::XsdDatatypeValues),
}
impl Datatype for DatatypeValues {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            DatatypeValues::Relax(relax) => relax.is_valid(value),
            DatatypeValues::Xsd(xsd) => xsd.is_valid(value),
        }
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Datatypes {
    Relax(relax::BuiltinDatatype),
    Xsd(xsd::XsdDatatypes),
}
impl Datatype for Datatypes {
    fn is_valid(&self, value: &str) -> bool {
        match self {
            Datatypes::Relax(relax) => relax.is_valid(value),
            Datatypes::Xsd(xsd) => xsd.is_valid(value),
        }
    }
}
#[derive(Debug)]
pub enum Errors {
    UnsupportedDatatypeLibrary {
        span: codemap::Span,
        namespace: String,
    },
    Relax(relax::Error),
    Xsd(xsd::XsdDatatypeError),
}

#[derive(Default)]
pub struct Compiler {
    relax: relax::Compiler,
    xsd: xsd::Compiler,
}

impl DatatypeCompiler for Compiler {
    type DT = Datatypes;
    type DTValue = DatatypeValues;
    type Error = Errors;

    fn datatype_value(
        &self,
        ctx: &Context,
        datatype_name: &types::DatatypeName,
        value: &str,
    ) -> Result<Self::DTValue, Self::Error> {
        match datatype_name {
            types::DatatypeName::String | types::DatatypeName::Token => self
                .relax
                .datatype_value(ctx, datatype_name, value)
                .map(DatatypeValues::Relax)
                .map_err(Errors::Relax),
            types::DatatypeName::CName(types::QName(ref namespace_uri, _)) => self.dt_value(
                ctx,
                datatype_name,
                value,
                &namespace_uri.0,
                &namespace_uri.1,
            ),
            DatatypeName::NamespacedName(NamespacedName {
                ref namespace_uri, ..
            }) => {
                let ns = &namespace_uri.as_string_value()[..];
                self.dt_value(ctx, datatype_name, value, &namespace_uri.0, ns)
            }
        }
    }

    fn datatype_name(
        &self,
        ctx: &Context,
        datatype_name: &types::DatatypeName,
        params: &[types::Param],
    ) -> Result<Self::DT, Self::Error> {
        match datatype_name {
            types::DatatypeName::String | types::DatatypeName::Token => self
                .relax
                .datatype_name(ctx, datatype_name, params)
                .map(Datatypes::Relax)
                .map_err(Errors::Relax),
            DatatypeName::CName(types::QName(types::NcName(span, namespace_uri), _)) => {
                self.dt_name(ctx, datatype_name, params, &span, namespace_uri)
            }
            DatatypeName::NamespacedName(NamespacedName { namespace_uri, .. }) => {
                let ns = &namespace_uri.as_string_value()[..];
                self.dt_name(ctx, datatype_name, params, &namespace_uri.0, ns)
            }
        }
    }
}

impl Compiler {
    fn dt_name(
        &self,
        ctx: &Context,
        datatype_name: &DatatypeName,
        params: &[types::Param],
        ns_span: &types::Span,
        ns: &str,
    ) -> Result<Datatypes, Errors> {
        match ns {
            "" => self
                .relax
                .datatype_name(ctx, datatype_name, params)
                .map(Datatypes::Relax)
                .map_err(Errors::Relax),
            xsd::NAMESPACE_URI => self
                .xsd
                .datatype_name(ctx, datatype_name, params)
                .map(Datatypes::Xsd)
                .map_err(Errors::Xsd),
            _ => Err(Errors::UnsupportedDatatypeLibrary {
                span: ctx.convert_span(ns_span),
                namespace: ns.to_string(),
            }),
        }
    }

    fn dt_value(
        &self,
        ctx: &Context,
        datatype_name: &DatatypeName,
        value: &str,
        ns_span: &types::Span,
        ns: &str,
    ) -> Result<DatatypeValues, Errors> {
        match ns {
            "" => self
                .relax
                .datatype_value(ctx, datatype_name, value)
                .map(DatatypeValues::Relax)
                .map_err(Errors::Relax),
            xsd::NAMESPACE_URI => self
                .xsd
                .datatype_value(ctx, datatype_name, value)
                .map(DatatypeValues::Xsd)
                .map_err(Errors::Xsd),
            _ => Err(Errors::UnsupportedDatatypeLibrary {
                span: ctx.convert_span(ns_span),
                namespace: ns.to_string(),
            }),
        }
    }
}
