use crate::model::Pattern;
use crate::model::datatype::xsd::{FacetError, XsdDatatypeError};
use crate::model::datatype::{DatatypeCompiler, Errors};
use crate::model::{self, datatype, restrictions};
use codemap::CodeMap;

use crate::syntax::types::{
    DatatypeName, Name, NamespaceUriLiteral, NamespacedName, NcName, QName, Schema,
};
use crate::syntax::{compact, types};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

/// Information about a named definition in a compiled schema.
/// Key for a definition within a grammar.  The start pattern has its own key, so that it can't
/// collide with a pattern named `start`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum RefKey {
    Start,
    Named(String),
}

impl RefKey {
    fn named(id: &types::Identifier) -> RefKey {
        RefKey::Named(id.1.clone())
    }
}

impl From<&types::DefineName> for RefKey {
    fn from(name: &types::DefineName) -> RefKey {
        match name {
            types::DefineName::Start(_) => RefKey::Start,
            types::DefineName::Named(id) => RefKey::named(id),
        }
    }
}

impl fmt::Display for RefKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RefKey::Start => f.write_str("start"),
            RefKey::Named(name) => f.write_str(name),
        }
    }
}

pub struct DefinitionInfo {
    pub name: String,
    pub span: codemap::Span,
    pub pat_ref: Rc<RefCell<Option<model::DefineRule>>>,
}

/// Result of compiling a schema, including the start rule and metadata.
pub struct CompileResult {
    pub start: Rc<RefCell<Option<model::DefineRule>>>,
    pub definitions: Vec<DefinitionInfo>,
    pub reachable: HashSet<usize>,
}

impl CompileResult {
    /// Returns definitions that are not reachable from the start rule.
    pub fn unreachable_definitions(&self) -> Vec<&DefinitionInfo> {
        self.definitions
            .iter()
            .filter(|d| {
                !Rc::ptr_eq(&d.pat_ref, &self.start)
                    && !self.reachable.contains(&(d.pat_ref.as_ptr() as usize))
            })
            .collect()
    }
}

// TODO:
//  - Detect ambiguous grammars per https://www.kohsuke.org/relaxng/ambiguity/AmbiguousGrammarDetection.pdf
//  - Enforce constraints, e.g.
//    - An except element that is a child of an anyName element must not have any anyName descendant
//      elements. An except element that is a child of an nsName element must not have any nsName or
//      anyName descendant elements.
//    - A name element that occurs as the first child of an attribute element or as the descendant
//      of the first child of an attribute element and that has an ns attribute with value equal to
//      the empty string must not have content equal to xmlns.
//    - A name or nsName element that occurs as the first child of an attribute element or as the
//      descendant of the first child of an attribute element must not have an ns attribute with
//      value http://www.w3.org/2000/xmlns.
//  - Provide a way to support diagnostic warnings for definitions that are unused

/// Specifies the concrete RelaxNG syntax to be used when loading schema files
pub enum Syntax {
    /// Use the RelaxNG XML-based schema syntax
    Xml,
    /// Use the RelaxNG 'compact' schema syntax
    Compact,
    /// Auto-detect syntax from the file extension (`.rng` → XML, otherwise compact)
    Auto,
}

impl Syntax {
    fn resolve(&self, name: &Path) -> Syntax {
        match self {
            Syntax::Auto => {
                if name.extension().and_then(|e| e.to_str()) == Some("rng") {
                    Syntax::Xml
                } else {
                    Syntax::Compact
                }
            }
            Syntax::Xml => Syntax::Xml,
            Syntax::Compact => Syntax::Compact,
        }
    }

    fn parse(&self, file: &Arc<codemap::File>) -> Result<Schema, RelaxError> {
        match self {
            Syntax::Xml => crate::syntax::xml::parse(file.source()).map_err(|e| {
                let span = match &e {
                    crate::syntax::xml::Error::Expected(span, _) => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    crate::syntax::xml::Error::Unexpected(span, _)
                    | crate::syntax::xml::Error::UnexpectedChar { span, .. } => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    crate::syntax::xml::Error::Xml(span, _) => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    crate::syntax::xml::Error::Todo(_) => file.span.subspan(0, 0),
                };
                RelaxError::XmlParse(span, e)
            }),
            Syntax::Compact => {
                let subspan = |span: std::ops::Range<usize>| {
                    file.span.subspan(span.start as _, span.end as _)
                };
                let schema = compact::parse(file.source()).map_err(|e| match e {
                    compact::CompactError::Input(e) => RelaxError::Input(subspan(e.span()), e),
                    compact::CompactError::Syntax { span, kind } => {
                        RelaxError::Parse(subspan(span), kind)
                    }
                    compact::CompactError::ExceptNeedsParentheses { span } => {
                        RelaxError::ExceptNeedsParentheses(subspan(span))
                    }
                    compact::CompactError::TopLevelFollowAnnotation { span } => {
                        RelaxError::TopLevelFollowAnnotation(subspan(span))
                    }
                })?;
                Ok(schema)
            }
            Syntax::Auto => unreachable!("Auto syntax must be resolved before parsing"),
        }
    }
}

#[derive(Debug)]
pub enum RelaxError {
    Io(PathBuf, io::Error),
    IncludeError(codemap::Span, Box<RelaxError>),
    // TODO: remove nom type from public API
    Parse(codemap::Span, nom::error::ErrorKind),
    /// In the compact syntax, an operator applied to a `-` (except) expression without
    /// parentheses; the span is that of the operator
    ExceptNeedsParentheses(codemap::Span),
    /// In the compact syntax, an annotation that would translate to a second top-level element: a
    /// follow annotation (`>>`) on the top-level pattern, or an annotation element on a top-level
    /// value
    TopLevelFollowAnnotation(codemap::Span),
    XmlParse(codemap::Span, crate::syntax::xml::Error),
    DuplicateDefinition {
        name: String,
        duplicate: codemap::Span,
        original: codemap::Span,
    },
    UndefinedNamespacePrefix {
        span: codemap::Span,
        prefix: String,
    },
    UndefinedReference {
        span: codemap::Span,
        identifier: String,
    },
    UndefinedDatatypePrefix {
        span: codemap::Span,
        prefix: String,
    },
    NamespacePrefixAlreadyDefined {
        span: codemap::Span,
        prefix: String,
    },
    DatatypePrefixAlreadyDefined {
        span: codemap::Span,
        prefix: String,
    },
    /// a 'grammar' section failed to specify a 'start = ...' rule
    StartRuleNotDefined {
        span: codemap::Span,
    },
    /// the grammar has both 'foo |= ...' and 'foo &= ...' constructs
    DefineRulesMixesChoiceAndInterleave {
        this_span: codemap::Span,
        that_span: codemap::Span,
    },
    /// a 'parent ...' pattern was used in a context where no parent grammar is available
    NoParentAvailable(codemap::Span),
    DatatypeError(datatype::Errors),
    RecursiveReference {
        ref_id: String,
        ref_span: codemap::Span,
        def_id: String,
        def_span: codemap::Span,
    },
    /// An element pattern may not appear as the child of an attribute pattern
    ElementAsChildOfAttribute {
        attribute_span: codemap::Span,
        element_span: codemap::Span,
    },
    RestrictionViolation(restrictions::Violation),
    RecursiveInclude {
        name: String,
        span: codemap::Span,
    },
    IncludedFileMustBeGrammar {
        span: codemap::Span,
    },
    /// The name is specified as an override for an include, but the included grammar did not
    /// have a definition of that name to be overriden
    OverrideMissingFromInclude {
        override_span: codemap::Span,
        include_span: codemap::Span,
        name: String,
    },
    /// The compact-syntax source text was rejected before tokenisation
    Input(codemap::Span, compact::InputError),
    InvalidAnnotationName {
        span: codemap::Span,
        reason: AnnotationNameError,
        declaration: Option<codemap::Span>,
    },
    DuplicateAnnotationAttribute {
        duplicate: codemap::Span,
        original: codemap::Span,
        namespace_uri: String,
        local_name: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationNameError {
    UnqualifiedAttribute,
    RelaxNgNamespace,
    XmlnsNamespace,
    XmlnsName,
    InheritedNamespace { prefix: String },
}

impl AnnotationNameError {
    fn declaration_label(&self) -> &'static str {
        match self {
            AnnotationNameError::UnqualifiedAttribute => "prefix bound to no namespace here",
            AnnotationNameError::RelaxNgNamespace => "prefix bound to the RELAX NG namespace here",
            AnnotationNameError::XmlnsNamespace => "prefix bound to the xmlns namespace here",
            AnnotationNameError::XmlnsName => "attribute named `xmlns` here",
            AnnotationNameError::InheritedNamespace { .. } => "prefix declared as `inherit` here",
        }
    }
}

impl std::fmt::Display for AnnotationNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotationNameError::UnqualifiedAttribute => {
                write!(f, "An annotation attribute must be in a namespace")
            }
            AnnotationNameError::RelaxNgNamespace => {
                write!(f, "An annotation can't be in the RELAX NG namespace")
            }
            AnnotationNameError::XmlnsNamespace => {
                write!(f, "An annotation attribute can't be in the xmlns namespace")
            }
            AnnotationNameError::XmlnsName => {
                write!(f, "An annotation attribute can't be named `xmlns`")
            }
            AnnotationNameError::InheritedNamespace { prefix } => write!(
                f,
                "Prefix {prefix:?} is declared as `inherit`, which an annotation can't use"
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum NamespaceBinding {
    Uri(String),
    Inherit,
}

/// A namespace prefix's binding, and where it was declared
#[derive(Clone, Debug)]
pub(crate) struct NamespaceDecl {
    binding: NamespaceBinding,
    /// The `namespace` declaration; `None` for the predeclared `xml` prefix
    span: Option<codemap::Span>,
}

pub(crate) enum Context<'a> {
    Root {
        file: Arc<codemap::File>,
        refs: RefCell<HashMap<RefKey, Rc<RefCell<Option<model::DefineRule>>>>>,
        namespaces: HashMap<String, NamespaceDecl>,
        default_namespace: String,
        inherited_namespace: String,
        datatypes: HashMap<String, String>,
        //start: Option<DefineRule>,
    },
    Include {
        parent: &'a Context<'a>,
        file: Arc<codemap::File>,
        // we track what names are overiden for the included grammar, and also weather that
        // grammar actually had such a definition (it is an error to provide an override for
        // a name that's not defined in the include)
        overrides: RefCell<HashMap<RefKey, (codemap::Span, bool)>>,
        namespaces: HashMap<String, NamespaceDecl>,
        default_namespace: String,
        inherited_namespace: String,
        datatypes: HashMap<String, String>,
    },
    IncludeOverrides {
        parent: &'a Context<'a>,
    },
    Grammar {
        parent: &'a Context<'a>,
        refs: RefCell<HashMap<RefKey, Rc<RefCell<Option<model::DefineRule>>>>>,
    },
    // We track the context of a definition since the body of that definition must not reference
    // the identifier being defined, unless the reference is in the body of an element definition
    Define {
        parent: &'a Context<'a>,
        id: RefKey,
    },
    // We track context of an Element because when an Element appears within the body of a Define,
    // the Elements body my recursively refer to the ancestor definition, while other parts of
    // the Define body may not refer to itself recursively
    Element {
        parent: &'a Context<'a>,
    },
    // We track attributes as another context so that we can prevent Element patterns from appearing
    // as children of attributes
    Attribute {
        parent: &'a Context<'a>,
        span: codemap::Span,
    },
}
impl<'a> Context<'a> {
    pub fn new(file: Arc<codemap::File>) -> Context<'a> {
        let mut namespaces = HashMap::new();
        namespaces.insert(
            "xml".to_string(),
            NamespaceDecl {
                binding: NamespaceBinding::Uri("http://www.w3.org/XML/1998/namespace".to_string()),
                span: None,
            },
        );
        let mut datatypes = HashMap::new();
        datatypes.insert(
            "xsd".to_string(),
            "http://www.w3.org/2001/XMLSchema-datatypes".to_string(),
        );
        Context::Root {
            file,
            refs: RefCell::new(HashMap::new()),
            namespaces,
            default_namespace: "".to_string(),
            inherited_namespace: "".to_string(),
            datatypes,
            //start: None,
        }
    }

    /// Creates a new context for an included file
    fn new_include(
        &self,
        span: codemap::Span,
        file: Arc<codemap::File>,
        ns: Option<&str>,
    ) -> Result<Context<'_>, RelaxError> {
        self.check_include(span, file.clone())?;
        let mut namespaces = HashMap::new();
        namespaces.insert(
            "xml".to_string(),
            NamespaceDecl {
                binding: NamespaceBinding::Uri("http://www.w3.org/XML/1998/namespace".to_string()),
                span: None,
            },
        );
        let mut datatypes = HashMap::new();
        datatypes.insert(
            "xsd".to_string(),
            "http://www.w3.org/2001/XMLSchema-datatypes".to_string(),
        );
        Ok(Context::Include {
            parent: self,
            file,
            overrides: RefCell::new(HashMap::new()),
            namespaces,
            default_namespace: ns.unwrap_or("").to_string(),
            inherited_namespace: ns.unwrap_or("").to_string(),
            datatypes,
        })
    }

    /// Creates a new context for the body of an include-overrides block
    fn new_inc_overrides(&self) -> Context<'_> {
        match self {
            Context::Include { .. } => Context::IncludeOverrides { parent: self },
            _ => {
                unreachable!("Parent was not a Context::Include");
            }
        }
    }

    /// Creates a new context for the body of a grammar
    fn new_grammar(&self) -> Context<'_> {
        Context::Grammar {
            parent: self,
            refs: RefCell::new(HashMap::new()),
        }
    }

    /// Creates a new context for the body of a definition
    fn new_define(&self, id: RefKey) -> Context<'_> {
        Context::Define { parent: self, id }
    }

    /// Creates a new context for the body of an element
    fn new_element(&self, element_span: codemap::Span) -> Result<Context<'_>, RelaxError> {
        if let Some(attribute_span) = self.parent_attribute() {
            return Err(RelaxError::ElementAsChildOfAttribute {
                attribute_span,
                element_span,
            });
        }
        Ok(Context::Element { parent: self })
    }

    /// Creates a new context for the body of an element
    fn new_attribute(&self, span: codemap::Span) -> Context<'_> {
        Context::Attribute { parent: self, span }
    }

    /// For an include-overrides block, the context of the file containing the `include`.
    ///
    /// The overriding definitions are written in the including file, so names, prefixes and
    /// spans within them resolve against that file rather than against the included one.
    fn includer(&self) -> &Context<'a> {
        match self {
            Context::IncludeOverrides {
                parent: Context::Include { parent, .. },
            } => parent,
            _ => unreachable!("includer() is only valid on an IncludeOverrides context"),
        }
    }

    fn file(&self) -> Arc<codemap::File> {
        match self {
            Context::Root { file, .. } | Context::Include { file, .. } => file.clone(),
            Context::IncludeOverrides { .. } => self.includer().file(),
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.file(),
        }
    }

    /// Utility to convert a simple Span from the relaxng-compact-syntax model into the specialised
    /// Span type that we use for diagnostics
    pub(super) fn convert_span(&self, span: &super::pattern::Span) -> codemap::Span {
        self.file().span.subspan(span.start as u64, span.end as u64)
    }

    // TODO: an enum to indicate either 'no namespace', or a uri
    pub(super) fn default_namespace_uri(&self) -> &str {
        match self {
            Context::Root {
                default_namespace, ..
            }
            | Context::Include {
                default_namespace, ..
            } => &default_namespace[..],
            Context::IncludeOverrides { .. } => self.includer().default_namespace_uri(),
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.default_namespace_uri(),
        }
    }

    fn set_default_namespace(&mut self, uri: String) {
        match self {
            Context::Root {
                default_namespace, ..
            }
            | Context::Include {
                default_namespace, ..
            } => {
                *default_namespace = uri;
            }
            _ => {
                unreachable!("Not expecting to see namespace declarations in this context")
            }
        }
    }

    fn inherited_namespace_uri(&self) -> &str {
        match self {
            Context::Root {
                inherited_namespace,
                ..
            }
            | Context::Include {
                inherited_namespace,
                ..
            } => &inherited_namespace[..],
            Context::IncludeOverrides { .. } => self.includer().inherited_namespace_uri(),
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.inherited_namespace_uri(),
        }
    }

    fn declare_namespace(
        &mut self,
        prefix: String,
        binding: NamespaceBinding,
        span: codemap::Span,
    ) -> Result<(), RelaxError> {
        match self {
            Context::Root { namespaces, .. } | Context::Include { namespaces, .. } => {
                if let Some(_old_uri) = namespaces.get(&prefix) {
                    //Err(RelaxError::NamespacePrefixAlreadyDefined { span: self.convert_span(), prefix })
                    Ok(())
                } else {
                    namespaces.insert(
                        prefix,
                        NamespaceDecl {
                            binding,
                            span: Some(span),
                        },
                    );
                    Ok(())
                }
            }
            Context::IncludeOverrides { .. }
            | Context::Grammar { .. }
            | Context::Define { .. }
            | Context::Element { .. }
            | Context::Attribute { .. } => {
                unreachable!("Not expecting to see namespace declarations in this context")
            }
        }
    }

    fn namespace_uri_for_prefix(&self, prefix: &types::NcName) -> Result<&str, RelaxError> {
        // TODO: return Option, and have caller work out RelaxError value
        match self.namespace_uri_for_prefix_str(&prefix.1) {
            None => Err(RelaxError::UndefinedNamespacePrefix {
                span: self.convert_span(&prefix.0),
                prefix: prefix.1.to_string(),
            }),
            Some(ns) => Ok(ns),
        }
    }
    pub(super) fn namespace_uri_for_prefix_str(&self, prefix: &str) -> Option<&str> {
        self.namespace_decl(prefix).map(|decl| match &decl.binding {
            NamespaceBinding::Uri(uri) => &uri[..],
            NamespaceBinding::Inherit => self.inherited_namespace_uri(),
        })
    }

    fn namespace_decl(&self, prefix: &str) -> Option<&NamespaceDecl> {
        match self {
            Context::Root { namespaces, .. } | Context::Include { namespaces, .. } => {
                namespaces.get(prefix)
            }
            Context::IncludeOverrides { .. } => self.includer().namespace_decl(prefix),
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.namespace_decl(prefix),
        }
    }

    fn declare_datatype(
        &mut self,
        prefix: String,
        uri: String,
        span: codemap::Span,
    ) -> Result<(), RelaxError> {
        match self {
            Context::Root { datatypes, .. } | Context::Include { datatypes, .. } => {
                match datatypes.entry(prefix) {
                    Entry::Occupied(e) => Err(RelaxError::DatatypePrefixAlreadyDefined {
                        span,
                        prefix: e.key().clone(),
                    }),
                    Entry::Vacant(e) => {
                        e.insert(uri);
                        Ok(())
                    }
                }
            }
            Context::IncludeOverrides { .. }
            | Context::Grammar { .. }
            | Context::Define { .. }
            | Context::Element { .. }
            | Context::Attribute { .. } => {
                unreachable!("Not expecting to see datatype declarations in this context")
            }
        }
    }

    fn try_datatype_uri_for_prefix(&self, prefix: &types::NcName) -> Result<&str, RelaxError> {
        self.datatype_uri_for_prefix(&prefix.1)
            .ok_or_else(|| RelaxError::UndefinedDatatypePrefix {
                span: self.convert_span(&prefix.0),
                prefix: prefix.1.clone(),
            })
    }

    fn datatype_uri_for_prefix(&self, prefix: &str) -> Option<&str> {
        match self {
            // datatype prefixes are scoped to the file that declares them, and are not visible
            // to included files
            Context::Root { datatypes, .. } | Context::Include { datatypes, .. } => {
                datatypes.get(prefix).map(|s| &s[..])
            }
            Context::IncludeOverrides { .. } => self.includer().datatype_uri_for_prefix(prefix),
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.datatype_uri_for_prefix(prefix),
        }
    }

    fn define(&self, id: &RefKey, rule: model::DefineRule) -> Result<(), RelaxError> {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                let mut refs = refs.borrow_mut();
                if refs.contains_key(id) {
                    let a = refs.get(id);
                    let mut rule_ref = a.unwrap().borrow_mut();
                    *rule_ref =
                        Compiler::<FsFiles>::merge(&id.to_string(), rule_ref.take(), Some(rule))?
                } else {
                    refs.insert(id.clone(), Rc::new(RefCell::new(Some(rule))));
                }
                Ok(())
            }
            Context::Include {
                parent, overrides, ..
            } => {
                let mut overrides = overrides.borrow_mut();
                if overrides.contains_key(id) {
                    overrides.get_mut(id).unwrap().1 = true;
                    Ok(())
                } else {
                    parent.define(id, rule)
                }
            }
            Context::IncludeOverrides { parent } => {
                if let Context::Include {
                    overrides,
                    parent: parant_of_include,
                    ..
                } = parent
                {
                    let mut overrides = overrides.borrow_mut();
                    if overrides.contains_key(id) {
                        return Err(RelaxError::DuplicateDefinition {
                            name: id.to_string(),
                            duplicate: *rule.span(),
                            original: overrides[id].0,
                        });
                    } else {
                        overrides.insert(id.clone(), (*rule.span(), false));
                        parant_of_include.define(id, rule)?;
                    };
                    Ok(())
                } else {
                    unreachable!(
                        "Should not be possible for Context::IncludeOverrides to have parent other than Context::Include"
                    )
                }
            }
            Context::Define { .. } => {
                unreachable!("Not expecting definition within another definition")
            }
            Context::Element { .. } => {
                unreachable!("Not expecting definition within an element")
            }
            Context::Attribute { .. } => {
                unreachable!("Not expecting definition within an element")
            }
        }
    }

    fn get_ref(
        &self,
        ref_id: &RefKey,
    ) -> std::option::Option<std::rc::Rc<std::cell::RefCell<std::option::Option<model::DefineRule>>>>
    {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                refs.borrow().get(ref_id).map(Clone::clone)
            }
            Context::Include { parent, .. }
            | Context::IncludeOverrides { parent }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.get_ref(ref_id),
            Context::Define { parent, id } => {
                if id == ref_id {
                    panic!("unexpected ref lookup of {id:?} within its own definition");
                }
                parent.get_ref(ref_id)
            }
        }
    }

    fn ref_iter(
        &self,
    ) -> impl Iterator<
        Item = (
            RefKey,
            std::rc::Rc<std::cell::RefCell<std::option::Option<model::DefineRule>>>,
        ),
    > + '_ {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                refs.borrow().clone().into_iter()
            }
            _ => panic!("ref_iter() only valid for root context"),
        }
    }

    fn acquire_ref(&self, ref_id: &types::Identifier) -> Result<model::PatRef, RelaxError> {
        self.acquire_ref_impl(ref_id)
    }
    fn acquire_ref_impl(&self, ref_id: &types::Identifier) -> Result<model::PatRef, RelaxError> {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                let mut refs = refs.borrow_mut();
                let key = RefKey::named(ref_id);
                if let Some(r) = refs.get(&key) {
                    Ok(model::PatRef(r.clone()))
                } else {
                    let r = Rc::new(RefCell::new(None));
                    refs.insert(key, r.clone());
                    Ok(model::PatRef(r))
                }
            }
            Context::Include { parent, .. }
            | Context::IncludeOverrides { parent }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.acquire_ref_impl(ref_id),
        }
    }

    fn acquire_parent_ref(&self, id: &types::Identifier) -> Result<model::PatRef, RelaxError> {
        match self {
            Context::Root { .. } => {
                let span = self.file().span.subspan(id.0.start as u64, id.0.end as u64);
                Err(RelaxError::NoParentAvailable(span))
            }
            Context::Include { parent, .. }
            | Context::IncludeOverrides { parent }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.acquire_parent_ref(id),
            Context::Grammar { parent, .. } => parent.acquire_ref(id),
        }
    }
    // Returns Some if the parent definition for which this is the context is defining an
    // attribute, and None otherwise
    fn parent_attribute(&self) -> Option<codemap::Span> {
        match self {
            Context::Root { .. } | Context::Grammar { .. } => None,
            Context::Attribute { span, .. } => Some(*span),
            Context::Include { parent, .. }
            | Context::IncludeOverrides { parent }
            | Context::Define { parent, .. }
            | Context::Element { parent } => parent.parent_attribute(),
        }
    }

    fn check_include(
        &self,
        span: codemap::Span,
        this_file: Arc<codemap::File>,
    ) -> Result<(), RelaxError> {
        match self {
            Context::Root { .. } => Ok(()),
            Context::Grammar { parent, .. }
            | Context::IncludeOverrides { parent }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.check_include(span, this_file),
            Context::Include { parent, file, .. } => {
                if this_file.name() == file.name() {
                    return Err(RelaxError::RecursiveInclude {
                        name: this_file.name().to_string(),
                        span,
                    });
                }
                parent.check_include(span, this_file)
            }
        }
    }
}

pub trait Files {
    fn load(&self, name: &Path) -> Result<String, RelaxError>;
}
pub struct FsFiles;
impl Files for FsFiles {
    fn load(&self, name: &Path) -> Result<String, RelaxError> {
        let mut io = File::open(name).map_err(|e| RelaxError::Io(name.to_path_buf(), e))?;
        let mut data = String::new();
        io.read_to_string(&mut data)
            .map_err(|e| RelaxError::Io(name.to_path_buf(), e))?;
        Ok(data)
    }
}

pub struct Compiler<FS: Files> {
    loaded: HashMap<PathBuf, (Arc<codemap::File>, Rc<Schema>)>,
    codemap: CodeMap,
    fs: FS,
    syntax: Syntax,
    datatype_compiler: datatype::Compiler,
}
impl Default for Compiler<FsFiles> {
    fn default() -> Self {
        Self::new(FsFiles, Syntax::Auto)
    }
}
/// The namespace that a file referenced by `include` or `external` inherits as its default.
///
/// An XML-syntax reference carries this as its (possibly inherited) `ns` attribute.  A
/// compact-syntax reference passes on the namespace bound to its `inherit = prefix` clause, or
/// the including file's default namespace when there is no such clause.
fn inherited_namespace(
    ctx: &Context,
    ns: Option<&str>,
    inherit: Option<&types::Inherit>,
) -> Result<String, RelaxError> {
    if let Some(ns) = ns {
        return Ok(ns.to_string());
    }
    match inherit {
        Some(inherit) => {
            let prefix = inherit.0.to_string();
            ctx.namespace_uri_for_prefix_str(&prefix)
                .map(str::to_string)
                .ok_or_else(|| RelaxError::UndefinedNamespacePrefix {
                    span: ctx.convert_span(&inherit.0.span()),
                    prefix,
                })
        }
        None => Ok(ctx.default_namespace_uri().to_string()),
    }
}

impl<FS: Files> Compiler<FS> {
    pub fn new(fs: FS, syntax: Syntax) -> Compiler<FS> {
        Compiler {
            loaded: HashMap::default(),
            codemap: CodeMap::default(),
            fs,
            syntax,
            datatype_compiler: datatype::Compiler::default(),
        }
    }

    // Returns an iterator over the loaded schemas.
    pub fn loaded(&self) -> impl Iterator<Item = &PathBuf> {
        self.loaded.keys()
    }

    /// Access the underlying CodeMap for resolving spans to source locations.
    pub fn code_map(&self) -> &CodeMap {
        &self.codemap
    }

    // TODO: does this need to support URLs?
    pub fn compile(&mut self, name: &Path) -> Result<CompileResult, RelaxError> {
        let (file, schema) = self.get_schema(name)?;
        let mut ctx = Context::new(file.clone());
        self.compile_schema(&mut ctx, schema)?;

        // Collect definitions before checking for undefined refs
        let mut definitions = Vec::new();
        for (def_name, r) in ctx.ref_iter() {
            if r.borrow().is_none() {
                return Err(RelaxError::UndefinedReference {
                    span: file.span.subspan(0, 0),
                    identifier: def_name.to_string(),
                });
            }
            let def_span = r.borrow().as_ref().unwrap().span().to_owned();
            definitions.push(DefinitionInfo {
                name: def_name.to_string(),
                span: def_span,
                pat_ref: r,
            });
        }

        if let Some(start) = ctx.get_ref(&RefKey::Start) {
            let mut seen = HashSet::new();
            let mut expanding = HashMap::new();
            // Check for undefined references and recursive references without element
            self.check(
                &mut seen,
                &mut expanding,
                false,
                start.borrow().as_ref().unwrap().pattern(),
            )?;
            // Section 7 restrictions (must run after simplification/normalization)
            restrictions::check_restrictions(start.borrow().as_ref().unwrap().pattern())
                .map_err(RelaxError::RestrictionViolation)?;
            Ok(CompileResult {
                start,
                definitions,
                reachable: seen,
            })
        } else {
            Err(RelaxError::StartRuleNotDefined { span: file.span })
        }
    }

    /// Walk the pattern tree from start, checking for undefined references and
    /// recursive references that don't pass through an element.
    /// - `seen`: refs that have been fully checked (skip on re-encounter)
    /// - `expanding`: refs currently being expanded in the call chain, mapped
    ///   to whether an element has been crossed since that ref started expanding
    /// - `inside_element`: whether we've entered an element since the nearest
    ///   enclosing ref expansion started
    #[allow(clippy::only_used_in_recursion)]
    fn check(
        &self,
        seen: &mut HashSet<usize>,
        expanding: &mut HashMap<usize, bool>,
        inside_element: bool,
        patt: &model::Pattern,
    ) -> Result<(), RelaxError> {
        match patt {
            Pattern::Choice(v, _) | Pattern::Interleave(v, _) | Pattern::Group(v, _) => {
                for p in v {
                    self.check(seen, expanding, inside_element, p)?
                }
            }
            Pattern::Mixed(p, _)
            | Pattern::Optional(p, _)
            | Pattern::ZeroOrMore(p, _)
            | Pattern::OneOrMore(p, _)
            | Pattern::Attribute(_, p, _, _)
            | Pattern::List(p, _) => self.check(seen, expanding, inside_element, p)?,
            Pattern::Element(_, p, _, _) => {
                // Mark all currently-expanding refs as having crossed an element,
                // but save old values so siblings outside this element aren't affected
                let saved: Vec<(usize, bool)> = expanding.iter().map(|(&k, &v)| (k, v)).collect();
                for v in expanding.values_mut() {
                    *v = true;
                }
                self.check(seen, expanding, true, p)?;
                // Restore the crossed-element flags for refs still in expanding
                for (k, v) in saved {
                    if let Some(entry) = expanding.get_mut(&k) {
                        *entry = v;
                    }
                }
            }
            Pattern::Empty(_)
            | Pattern::Text(_)
            | Pattern::NotAllowed(_)
            | Pattern::DatatypeValue { .. } => {}
            Pattern::Ref(span, name, def) => {
                let ptr = def.0.as_ptr() as usize;
                if let Some(&crossed_element) = expanding.get(&ptr) {
                    if !crossed_element {
                        let borrowed = def.0.borrow();
                        let def_span = borrowed.as_ref().map(|r| *r.span());
                        return Err(RelaxError::RecursiveReference {
                            ref_id: name.clone(),
                            ref_span: *span,
                            def_id: name.clone(),
                            def_span: def_span.unwrap_or(*span),
                        });
                    }
                    // Valid recursion through element — don't recurse further
                } else if !seen.contains(&ptr) {
                    expanding.insert(ptr, false);
                    if let Some(rule) = def.0.borrow().as_ref() {
                        self.check(seen, expanding, false, rule.pattern())?
                    } else {
                        return Err(RelaxError::UndefinedReference {
                            span: *span,
                            identifier: name.to_string(),
                        });
                    }
                    expanding.remove(&ptr);
                    seen.insert(ptr);
                }
            }
            Pattern::DatatypeName { except, .. } => {
                if let Some(e) = except.as_ref() {
                    self.check(seen, expanding, inside_element, e)?;
                }
            }
        }
        Ok(())
    }

    pub fn dump_diagnostic(&self, err: &RelaxError) {
        let mut emitter = codemap_diagnostic::Emitter::stderr(
            codemap_diagnostic::ColorConfig::Always,
            Some(&self.codemap),
        );
        let d = self.diagnostic(err);
        emitter.emit(&[d]);
    }

    #[allow(clippy::only_used_in_recursion)]
    fn diagnostic(&self, err: &RelaxError) -> codemap_diagnostic::Diagnostic {
        match err {
            RelaxError::IncludeError(span, err) => {
                let mut d = self.diagnostic(err);
                let style = if d.spans.is_empty() {
                    codemap_diagnostic::SpanStyle::Primary
                } else {
                    codemap_diagnostic::SpanStyle::Secondary
                };
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style,
                    label: Some("as included here".to_owned()),
                };
                d.spans.push(label);
                d
            }
            RelaxError::Io(path, err) => codemap_diagnostic::Diagnostic {
                level: codemap_diagnostic::Level::Error,
                message: format!("Problem loading {:?}: {:?}", path, err.kind()),
                code: None,
                spans: vec![],
            },
            RelaxError::UndefinedReference { span, identifier } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("name is referenced here, but is not defined".to_owned()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Undefined: {identifier:?}"),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::NoParentAvailable(span) => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("No parent context in which to look-up this name".to_owned()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "parent is not available in this context".to_string(),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::XmlParse(span, e) => match e {
                crate::syntax::xml::Error::Expected(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("Expected: {msg}"),
                        code: None,
                        spans: vec![label],
                    }
                }
                crate::syntax::xml::Error::Unexpected(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: msg.to_string(),
                        code: None,
                        spans: vec![label],
                    }
                }
                crate::syntax::xml::Error::UnexpectedChar {
                    ch, at_start, what, ..
                } => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: crate::syntax::xml::Error::unexpected_char_message(
                            *ch, *at_start, what,
                        ),
                        code: None,
                        spans: vec![label],
                    }
                }
                crate::syntax::xml::Error::Xml(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("XML parsing error: {msg}"),
                        code: None,
                        spans: vec![label],
                    }
                }
                _ => panic!("{err:?}"),
            },
            RelaxError::InvalidAnnotationName {
                span,
                reason,
                declaration,
            } => {
                let mut spans = vec![codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                }];
                if let Some(declaration) = declaration {
                    spans.push(codemap_diagnostic::SpanLabel {
                        span: *declaration,
                        style: codemap_diagnostic::SpanStyle::Secondary,
                        label: Some(reason.declaration_label().to_string()),
                    });
                }
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: reason.to_string(),
                    code: None,
                    spans,
                }
            }
            RelaxError::DuplicateAnnotationAttribute {
                duplicate,
                original,
                namespace_uri,
                local_name,
            } => codemap_diagnostic::Diagnostic {
                level: codemap_diagnostic::Level::Error,
                message: if namespace_uri.is_empty() {
                    format!("Duplicate annotation attribute {local_name:?}")
                } else {
                    format!(
                        "Duplicate annotation attribute {local_name:?} in namespace {namespace_uri:?}"
                    )
                },
                code: None,
                spans: vec![
                    codemap_diagnostic::SpanLabel {
                        span: *duplicate,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: Some("duplicate".to_string()),
                    },
                    codemap_diagnostic::SpanLabel {
                        span: *original,
                        style: codemap_diagnostic::SpanStyle::Secondary,
                        label: Some("first given here".to_string()),
                    },
                ],
            },
            RelaxError::UndefinedNamespacePrefix { span, prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Undefined namespace prefix {prefix:?}"),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::DatatypeError(e) => match e {
                Errors::UnsupportedDatatypeLibrary { span, namespace } => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("Unsupported datatype library namespace {namespace:?}"),
                        code: None,
                        spans: vec![label],
                    }
                }
                Errors::Relax(e) => match e {
                    datatype::relax::Error::ParamNotAllowed { span, name: _ } => {
                        let label = codemap_diagnostic::SpanLabel {
                            span: *span,
                            style: codemap_diagnostic::SpanStyle::Primary,
                            label: Some("remove this parameter".to_string()),
                        };
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: "relaxng built-in datatype supports no parameters".to_string(),
                            code: None,
                            spans: vec![label],
                        }
                    }
                    datatype::relax::Error::DatataypeNameUnknown { span, name } => {
                        let label = codemap_diagnostic::SpanLabel {
                            span: *span,
                            style: codemap_diagnostic::SpanStyle::Primary,
                            label: Some("Only 'string' and 'token' supported".to_string()),
                        };
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: format!(
                                "The relaxng build-in datatype bibrary does not support the type {name:?}"
                            ),
                            code: None,
                            spans: vec![label],
                        }
                    }
                },
                Errors::Xsd(e) => match e {
                    XsdDatatypeError::Facet { type_name, facet } => {
                        let mut labels = vec![];
                        match facet {
                            FacetError::ConflictingFacet(_name) => {
                                unimplemented!("Support for ConflictingFacet")
                            }
                            FacetError::InvalidInt(span, msg) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid integer value: {msg}")),
                                })
                            }
                            FacetError::InvalidFloat(span, msg) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid floating-point value: {msg}")),
                                })
                            }
                            FacetError::InvalidPattern(span, err) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid pattern value: {err}")),
                                })
                            }
                            FacetError::InvalidFacet(span, name) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid facet for type: {name}")),
                                })
                            }
                            FacetError::InvalidValue(span, msg) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid facet value: {msg}")),
                                })
                            }
                        }
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: format!("Problematic facet for {type_name} type"),
                            code: None,
                            spans: labels,
                        }
                    }
                    XsdDatatypeError::UnsupportedDatatype { span, name } => {
                        let label = codemap_diagnostic::SpanLabel {
                            span: *span,
                            style: codemap_diagnostic::SpanStyle::Primary,
                            label: None,
                        };
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: format!(
                                "Unsupported datatype {name:?} for XML Schema datatype library"
                            ),
                            code: None,
                            spans: vec![label],
                        }
                    }
                    XsdDatatypeError::InvalidValueOfType { span, type_name } => {
                        let label = codemap_diagnostic::SpanLabel {
                            span: *span,
                            style: codemap_diagnostic::SpanStyle::Primary,
                            label: None,
                        };
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: format!("Invalid value of type {type_name:?}"),
                            code: None,
                            spans: vec![label],
                        }
                    }
                },
            },
            RelaxError::RecursiveReference {
                ref_id,
                ref_span,
                def_id: _,
                def_span,
            } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *ref_span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                let definition = codemap_diagnostic::SpanLabel {
                    span: *def_span,
                    style: codemap_diagnostic::SpanStyle::Secondary,
                    label: Some("Definition is here".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Recursive reference to {ref_id:?}"),
                    code: None,
                    spans: vec![label, definition],
                }
            }
            RelaxError::UndefinedDatatypePrefix { span, prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("undefined".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The datatype prefix {prefix:?} is not defined"),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::DuplicateDefinition {
                name,
                duplicate,
                original,
            } => {
                let dup = codemap_diagnostic::SpanLabel {
                    span: *duplicate,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Duplicated here".to_string()),
                };
                let orig = codemap_diagnostic::SpanLabel {
                    span: *original,
                    style: codemap_diagnostic::SpanStyle::Secondary,
                    label: Some("First seen here".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Duplicate definition of {name:?}"),
                    code: None,
                    spans: vec![dup, orig],
                }
            }
            RelaxError::DefineRulesMixesChoiceAndInterleave {
                this_span,
                that_span,
            } => {
                let dup = codemap_diagnostic::SpanLabel {
                    span: *this_span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Defined here with one define-rule".to_string()),
                };
                let orig = codemap_diagnostic::SpanLabel {
                    span: *that_span,
                    style: codemap_diagnostic::SpanStyle::Secondary,
                    label: Some(
                        "Previously defined here with a different combine-rule".to_string(),
                    ),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Definition mixes 'combine' and 'interleave' combine-rules"
                        .to_string(),
                    code: None,
                    spans: vec![dup, orig],
                }
            }
            RelaxError::StartRuleNotDefined { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("needs a 'start' rule".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Grammar is missing a start rule".to_string(),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::ElementAsChildOfAttribute {
                attribute_span,
                element_span,
            } => {
                let elem = codemap_diagnostic::SpanLabel {
                    span: *element_span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Remove this element".to_string()),
                };
                let attr = codemap_diagnostic::SpanLabel {
                    span: *attribute_span,
                    style: codemap_diagnostic::SpanStyle::Secondary,
                    label: Some("In this attribute's body".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "An Element may not appear within an attribute definition".to_string(),
                    code: None,
                    spans: vec![elem, attr],
                }
            }
            RelaxError::RecursiveInclude { name, span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Remove recursive include reference".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The resource {name:?} refers to itself recursively"),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::IncludedFileMustBeGrammar { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Add a 'grammar' declaration at the top level".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Included files must use a top-level 'grammar', not any other pattern element".to_string(),
                    code: None,
                    spans: vec![label]
                }
            }
            RelaxError::OverrideMissingFromInclude {
                override_span,
                include_span,
                name,
            } => {
                let override_label = codemap_diagnostic::SpanLabel {
                    span: *override_span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Either remove this override...".to_string()),
                };
                let include_label = codemap_diagnostic::SpanLabel {
                    span: *include_span,
                    style: codemap_diagnostic::SpanStyle::Secondary,
                    label: Some(format!(
                        "...or add a definition for {name:?} to this grammar"
                    )),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!(
                        "Override {name:?} has no corresponding definition in included grammar"
                    ),
                    code: None,
                    spans: vec![override_label, include_label],
                }
            }
            RelaxError::RestrictionViolation(violation) => {
                let mut spans = vec![codemap_diagnostic::SpanLabel {
                    span: violation.span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                }];
                if let Some(span) = violation.other_span {
                    spans.push(codemap_diagnostic::SpanLabel {
                        span,
                        style: codemap_diagnostic::SpanStyle::Secondary,
                        label: Some("overlaps with this".to_string()),
                    });
                }
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Schema restriction violated: {}", violation.kind),
                    code: None,
                    spans,
                }
            }
            RelaxError::TopLevelFollowAnnotation(span) => codemap_diagnostic::Diagnostic {
                level: codemap_diagnostic::Level::Error,
                message: "The top-level pattern can't have follow annotations".to_string(),
                code: None,
                spans: vec![codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Remove this annotation".to_string()),
                }],
            },
            RelaxError::ExceptNeedsParentheses(span) => codemap_diagnostic::Diagnostic {
                level: codemap_diagnostic::Level::Error,
                message: "Parentheses required around \"-\" expression".to_string(),
                code: None,
                spans: vec![codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("Applies to a \"-\" expression".to_string()),
                }],
            },
            RelaxError::Parse(span, kind) => {
                let message = match kind {
                    nom::error::ErrorKind::TooLarge => {
                        "Expression is too deeply nested".to_string()
                    }
                    _ => format!("Parse error: {kind:?}"),
                };
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message,
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::Input(span, err) => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: err.to_string(),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::NamespacePrefixAlreadyDefined { span, prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("already defined".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Namespace prefix {prefix:?} is already defined"),
                    code: None,
                    spans: vec![label],
                }
            }
            RelaxError::DatatypePrefixAlreadyDefined { span, prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: Some("already defined".to_string()),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Datatype prefix {prefix:?} is already defined"),
                    code: None,
                    spans: vec![label],
                }
            }
        }
    }

    #[inline(never)]
    fn get_schema(&mut self, name: &Path) -> Result<(Arc<codemap::File>, Rc<Schema>), RelaxError> {
        if let Some((f, s)) = self.loaded.get(name) {
            return Ok((f.clone(), s.clone()));
        }
        let data = self.fs.load(name)?;
        let file = self
            .codemap
            .add_file(name.to_string_lossy().to_string(), data);
        let schema = self.syntax.resolve(name).parse(&file)?;
        let schema = Rc::new(schema);
        self.loaded
            .insert(name.to_path_buf(), (file.clone(), schema.clone()));
        Ok((file, schema))
    }

    fn compile_schema(&mut self, ctx: &mut Context, schema: Rc<Schema>) -> Result<(), RelaxError> {
        for dec in schema.decls.iter() {
            self.compile_declaration(ctx, dec)?;
        }
        match schema.pattern_or_grammar {
            types::PatternOrGrammar::Pattern(ref p) => {
                if let Context::Include { .. } = ctx {
                    // look through leading annotations, as in `[ xml:lang = "en" ] grammar { … }`
                    let mut unannotated = p;
                    while let types::Pattern::Annotated(annos, inner) = unannotated {
                        Self::check_annotations(ctx, Some(annos))?;
                        unannotated = inner;
                    }
                    match unannotated {
                        types::Pattern::Grammar(grammar) => {
                            // when including a file containing a grammar definition, we don't require
                            // that the grammar has a 'start' rule, as compile_grammar_pattern()
                            self.compile_grammar_contents(ctx, &grammar.content[..])?;
                        }
                        _ => {
                            // files that are included into other files must have a top level 'grammar',
                            // rather than just a pattern
                            return Err(RelaxError::IncludedFileMustBeGrammar {
                                // TODO: use the span of the pattern, once we can determine it reliably
                                span: ctx.convert_span(&(0..0)),
                            });
                        }
                    }
                } else {
                    let rule = self.compile_pattern(ctx, p)?;
                    let span = ctx.convert_span(&(0..0)); // FIXME: synthesize a more useful span
                    ctx.define(
                        &RefKey::Start,
                        model::DefineRule::AssignCombine(span, None, rule),
                    )?;
                }
            }
            types::PatternOrGrammar::Grammar(types::GrammarPattern {
                span: _,
                ref content,
            }) => {
                self.compile_grammar_contents(ctx, &content[..])?;
            }
        }
        Ok(())
    }
    fn append_choice(choice: &mut Pattern, c: Pattern) {
        if let Pattern::Choice(this, _) = choice {
            if let Pattern::Choice(mut other, _) = c {
                this.append(&mut other)
            } else {
                this.push(c)
            }
        } else {
            panic!("Not a Choice pattern {choice:?}");
        }
    }
    fn append_interleave(interleave: &mut Pattern, c: Pattern) {
        if let Pattern::Interleave(this, _) = interleave {
            if let Pattern::Interleave(mut other, _) = c {
                this.append(&mut other)
            } else {
                this.push(c)
            }
        } else {
            panic!("Not an Interleave pattern {interleave:?}");
        }
    }
    fn compile_define(
        &mut self,
        ctx: &mut Context,
        define: &types::Define,
    ) -> Result<(), RelaxError> {
        Self::check_annotations(ctx, define.annotations.as_ref())?;
        let id = RefKey::from(&define.name);
        let mut def_ctx = ctx.new_define(id.clone());
        let rule = self.compile_pattern(&mut def_ctx, &define.pattern)?;
        let span = ctx.convert_span(define.name.span());
        let new_rule = match define.assign_method {
            types::AssignMethod::Assign => model::DefineRule::AssignCombine(span, None, rule),
            types::AssignMethod::Choice => {
                model::DefineRule::CombineOnly(span, model::CombineRule::Choice, rule)
            }
            types::AssignMethod::Interleave => {
                model::DefineRule::CombineOnly(span, model::CombineRule::Interleave, rule)
            }
        };
        ctx.define(&id, new_rule)?;
        Ok(())
    }
    fn compile_grammar_div(
        &mut self,
        ctx: &mut Context,
        div: &[types::GrammarContent],
    ) -> Result<(), RelaxError> {
        for item in div {
            self.compile_grammar_content_item(ctx, item)?;
        }
        Ok(())
    }
    /// Compile the overriding definitions in an include body, or in a `div` within one
    fn compile_include_overrides(
        &mut self,
        ctx: &mut Context,
        content: &[types::IncludeContent],
    ) -> Result<(), RelaxError> {
        for item in content {
            match item {
                types::IncludeContent::Define(d) => self.compile_define(ctx, d)?,
                types::IncludeContent::Div(d) => {
                    Self::check_annotations(ctx, d.annotations.as_ref())?;
                    self.compile_include_overrides(ctx, &d.content)?
                }
                types::IncludeContent::Annotation(e) => {
                    Self::compile_annotation_element(ctx, AnnotationNameRole::ForeignElement, e)?;
                }
            }
        }
        Ok(())
    }
    fn compile_include(
        &mut self,
        ctx: &mut Context,
        inc: &types::Include,
    ) -> Result<(), RelaxError> {
        Self::check_annotations(ctx, inc.annotations.as_ref())?;
        let file = ctx.file();
        let parent_dir = Path::new(file.name()).parent().unwrap_or(Path::new("."));
        let path = parent_dir.join(inc.uri.as_string_value());
        let span = ctx
            .file()
            .span
            .subspan(inc.uri.0.start as u64, inc.uri.0.end as u64);
        let (file, s) = self
            .get_schema(&path)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;
        // TODO: get the span of the grammar in the file, rather than the span of the whole file
        let include_span = file.span;

        let ns = inherited_namespace(ctx, inc.ns.as_deref(), inc.inherit.as_ref())?;
        let mut inc_ctx = ctx.new_include(span, file, Some(&ns))?;

        // replace any definitions for which the including file provides an override
        if let Some(ref overrides) = inc.content {
            let mut override_ctx = inc_ctx.new_inc_overrides();
            self.compile_include_overrides(&mut override_ctx, overrides)
                .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;
        }

        self.compile_schema(&mut inc_ctx, s)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;

        if let Context::Include { overrides, .. } = inc_ctx {
            for (name, (override_span, defined)) in overrides.borrow().iter() {
                if !defined {
                    return Err(RelaxError::OverrideMissingFromInclude {
                        name: name.to_string(),
                        override_span: *override_span,
                        include_span,
                    });
                }
            }
        } else {
            panic!()
        }

        Ok(())
    }

    // TODO: make this a method of PatRef or something like that
    fn merge(
        name: &str,
        a: Option<model::DefineRule>,
        b: Option<model::DefineRule>,
    ) -> Result<Option<model::DefineRule>, RelaxError> {
        match (a, b) {
            (None, None) => Ok(None),
            (Some(a), None) => Ok(Some(a)),
            (None, Some(b)) => Ok(Some(b)),
            (Some(a), Some(b)) => {
                // TODO: does this really work if other areas of the grammar retain references to the unmodified 'b'?
                Self::merge1(name, a, b).map(Some)
            }
        }
    }
    fn merge1(
        name: &str,
        a: model::DefineRule,
        b: model::DefineRule,
    ) -> Result<model::DefineRule, RelaxError> {
        use crate::model::{CombineRule, DefineRule};
        match (a, b) {
            (DefineRule::AssignCombine(this, _, _), DefineRule::AssignCombine(that, _, _)) => {
                Err(RelaxError::DuplicateDefinition {
                    name: name.to_string(),
                    duplicate: this,
                    original: that,
                })
            }
            // choice-related cases,
            (
                DefineRule::CombineOnly(this, CombineRule::Choice, mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Choice, patt_b),
            ) => {
                let result = if let Pattern::Choice(_, _) = patt_a {
                    Self::append_choice(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Choice(vec![patt_a, patt_b], that)
                };
                Ok(DefineRule::CombineOnly(this, CombineRule::Choice, result))
            }
            (
                DefineRule::AssignCombine(this, Some(CombineRule::Choice), mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Choice, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Choice, mut patt_a),
                DefineRule::AssignCombine(that, Some(CombineRule::Choice), patt_b),
            )
            | (
                DefineRule::AssignCombine(this, None, mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Choice, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Choice, mut patt_a),
                DefineRule::AssignCombine(that, None, patt_b),
            ) => {
                let result = if let Pattern::Choice(_, _) = patt_a {
                    Self::append_choice(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Choice(vec![patt_a, patt_b], that)
                };
                Ok(DefineRule::AssignCombine(
                    this,
                    Some(CombineRule::Choice),
                    result,
                ))
            }
            // interleave-related cases,
            (
                DefineRule::CombineOnly(this, CombineRule::Interleave, mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Interleave, patt_b),
            ) => {
                let result = if let Pattern::Interleave(_, _) = patt_a {
                    Self::append_interleave(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Interleave(vec![patt_a, patt_b], that)
                };
                Ok(DefineRule::CombineOnly(
                    this,
                    CombineRule::Interleave,
                    result,
                ))
            }
            (
                DefineRule::AssignCombine(this, Some(CombineRule::Interleave), mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Interleave, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Interleave, mut patt_a),
                DefineRule::AssignCombine(that, Some(CombineRule::Interleave), patt_b),
            )
            | (
                DefineRule::AssignCombine(this, None, mut patt_a),
                DefineRule::CombineOnly(that, CombineRule::Interleave, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Interleave, mut patt_a),
                DefineRule::AssignCombine(that, None, patt_b),
            ) => {
                let result = if let Pattern::Interleave(_, _) = patt_a {
                    Self::append_interleave(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Interleave(vec![patt_a, patt_b], that)
                };
                Ok(DefineRule::AssignCombine(
                    this,
                    Some(CombineRule::Interleave),
                    result,
                ))
            }
            // invalid combinations,
            (
                DefineRule::CombineOnly(this_span, CombineRule::Choice, _),
                DefineRule::CombineOnly(that_span, CombineRule::Interleave, _),
            )
            | (
                DefineRule::CombineOnly(this_span, CombineRule::Interleave, _),
                DefineRule::CombineOnly(that_span, CombineRule::Choice, _),
            )
            | (
                DefineRule::AssignCombine(this_span, Some(CombineRule::Choice), _),
                DefineRule::CombineOnly(that_span, CombineRule::Interleave, _),
            )
            | (
                DefineRule::AssignCombine(this_span, Some(CombineRule::Interleave), _),
                DefineRule::CombineOnly(that_span, CombineRule::Choice, _),
            )
            | (
                DefineRule::CombineOnly(this_span, CombineRule::Choice, _),
                DefineRule::AssignCombine(that_span, Some(CombineRule::Interleave), _),
            )
            | (
                DefineRule::CombineOnly(this_span, CombineRule::Interleave, _),
                DefineRule::AssignCombine(that_span, Some(CombineRule::Choice), _),
            ) => Err(RelaxError::DefineRulesMixesChoiceAndInterleave {
                this_span,
                that_span,
            }),
        }
    }

    fn compile_pattern(
        &mut self,
        ctx: &mut Context,
        pattern: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        match pattern {
            types::Pattern::Element(e) => self.compile_element(ctx, e),
            types::Pattern::Attribute(e) => self.compile_attribute(ctx, e),
            types::Pattern::List(l) => self.compile_list(ctx, l),
            types::Pattern::Mixed(types::MixedPattern(span, p)) => Ok(model::Pattern::Mixed(
                Box::new(self.compile_pattern(ctx, p)?),
                ctx.convert_span(span),
            )),
            types::Pattern::Identifier(i) => self.compile_ref(ctx, i),
            types::Pattern::Parent(p) => self.compile_parent(ctx, p),
            types::Pattern::Empty(span) => Ok(model::Pattern::Empty(ctx.convert_span(span))),
            types::Pattern::Text(span) => Ok(model::Pattern::Text(ctx.convert_span(span))),
            types::Pattern::NotAllowed(span) => {
                Ok(model::Pattern::NotAllowed(ctx.convert_span(span)))
            }
            types::Pattern::External(e) => self.compile_external(ctx, e),
            types::Pattern::Grammar(g) => self.compile_grammar_pattern(ctx, g),
            types::Pattern::Group(_, g) => self.compile_pattern(ctx, g),
            types::Pattern::ListPair(span, a, b) => self.compile_sequence(ctx, span, a, b),
            types::Pattern::InterleavePair(span, a, b) => self.compile_interleave(ctx, span, a, b),
            types::Pattern::ChoicePair(span, a, b) => self.compile_choice(ctx, span, a, b),
            types::Pattern::Optional(span, p) => Ok(model::Pattern::Optional(
                Box::new(self.compile_pattern(ctx, p)?),
                ctx.convert_span(span),
            )),
            types::Pattern::ZeroOrMore(span, p) => Ok(model::Pattern::ZeroOrMore(
                Box::new(self.compile_pattern(ctx, p)?),
                ctx.convert_span(span),
            )),
            types::Pattern::OneOrMore(span, p) => Ok(model::Pattern::OneOrMore(
                Box::new(self.compile_pattern(ctx, p)?),
                ctx.convert_span(span),
            )),
            types::Pattern::DatatypeValue(d) => self.compile_datatype_value_pattern(ctx, d),
            types::Pattern::DatatypeName(d) => self.compile_datatype_name_pattern(ctx, d),
            types::Pattern::Annotated(annos, inner) => {
                let compiled_annos = Self::compile_annotations(ctx, annos)?;
                let mut result = self.compile_pattern(ctx, inner)?;
                // Attach annotations to Element/Attribute patterns if possible
                match &mut result {
                    model::Pattern::Element(_, _, _, a) | model::Pattern::Attribute(_, _, _, a) => {
                        *a = Some(compiled_annos);
                    }
                    _ => {
                        // For other patterns, annotations are metadata-only; we note them but
                        // they don't change the pattern structure
                    }
                }
                Ok(result)
            }
        }
    }
    fn compile_element(
        &mut self,
        ctx: &mut Context,
        element: &types::ElementPattern,
    ) -> Result<model::Pattern, RelaxError> {
        let name_class = self.compile_nameclass(ctx, ElemAttr::Element, &element.name_class)?;
        let span = ctx.convert_span(&element.span);
        restrictions::check_name_class_restrictions(&name_class, span)
            .map_err(RelaxError::RestrictionViolation)?;
        let mut el_ctx = ctx.new_element(span)?;
        Ok(model::Pattern::Element(
            name_class,
            Box::new(self.compile_pattern(&mut el_ctx, &element.pattern)?),
            span,
            None,
        ))
    }
    fn compile_attribute(
        &mut self,
        ctx: &mut Context,
        attribute: &types::AttributePattern,
    ) -> Result<model::Pattern, RelaxError> {
        let name_class = self.compile_nameclass(ctx, ElemAttr::Attribute, &attribute.name_class)?;
        let span = ctx.convert_span(&attribute.span);
        restrictions::check_name_class_restrictions(&name_class, span)
            .map_err(RelaxError::RestrictionViolation)?;
        let mut att_ctx = ctx.new_attribute(span);
        Ok(model::Pattern::Attribute(
            name_class,
            Box::new(self.compile_pattern(&mut att_ctx, &attribute.pattern)?),
            span,
            None,
        ))
    }
    fn compile_list(
        &mut self,
        ctx: &mut Context,
        list: &types::ListPattern,
    ) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::List(
            Box::new(self.compile_pattern(ctx, &list.1)?),
            ctx.convert_span(&list.0),
        ))
    }
    fn compile_ref(
        &mut self,
        ctx: &mut Context,
        reference: &types::Identifier,
    ) -> Result<model::Pattern, RelaxError> {
        let span = ctx
            .file()
            .span
            .subspan((reference.0).start as u64, (reference.0).end as u64);
        Ok(Pattern::Ref(
            span,
            reference.1.clone(),
            ctx.acquire_ref(reference)?,
        ))
    }
    fn compile_parent(
        &mut self,
        ctx: &mut Context,
        reference: &types::Identifier,
    ) -> Result<model::Pattern, RelaxError> {
        let span = ctx
            .file()
            .span
            .subspan((reference.0).start as u64, (reference.0).end as u64);
        Ok(Pattern::Ref(
            span,
            reference.1.clone(),
            ctx.acquire_parent_ref(reference)?,
        ))
    }
    fn compile_external(
        &mut self,
        ctx: &mut Context,
        external: &types::ExternalPattern,
    ) -> Result<model::Pattern, RelaxError> {
        let ns = inherited_namespace(ctx, external.2.as_deref(), external.1.as_ref())?;
        let file = ctx.file();
        let parent_dir = Path::new(file.name()).parent().unwrap_or(Path::new("."));
        let path = parent_dir.join(external.0.as_string_value());
        let span = ctx.convert_span(&(external.0).0);
        let (file, s) = self
            .get_schema(&path)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;
        let file_span = file.span;
        let mut inc_ctx = ctx.new_include(span, file, Some(&ns))?;

        match &s.pattern_or_grammar {
            types::PatternOrGrammar::Pattern(pat) => self
                .compile_pattern(&mut inc_ctx, pat)
                .map_err(|e| RelaxError::IncludeError(span, Box::new(e))),
            types::PatternOrGrammar::Grammar(types::GrammarPattern { span: _, content }) => {
                let mut child_ctx = inc_ctx.new_grammar();
                for g in content {
                    self.compile_grammar_content_item(&mut child_ctx, g)?;
                }
                for (name, r) in child_ctx.ref_iter() {
                    if r.borrow().is_none() {
                        // TODO: retain span information for references so that we don't report bogus data
                        return Err(RelaxError::UndefinedReference {
                            span: child_ctx.convert_span(&(0..0)),
                            identifier: name.to_string(),
                        });
                    }
                }
                if let Some(r) = child_ctx.get_ref(&RefKey::Start) {
                    r.borrow_mut()
                        .take()
                        .map(|d| match d {
                            model::DefineRule::AssignCombine(_, _, p)
                            | model::DefineRule::CombineOnly(_, _, p) => p,
                        })
                        .ok_or(RelaxError::StartRuleNotDefined { span: file_span })
                } else {
                    Err(RelaxError::StartRuleNotDefined { span: file_span })
                }
            }
        }
    }

    fn compile_grammar_pattern(
        &mut self,
        ctx: &mut Context,
        grammar: &types::GrammarPattern,
    ) -> Result<model::Pattern, RelaxError> {
        let mut child_ctx = ctx.new_grammar();

        let content = &grammar.content[..];
        self.compile_grammar_contents(&mut child_ctx, content)?;
        if let Some(r) = child_ctx.get_ref(&RefKey::Start) {
            r.borrow_mut()
                .take()
                .map(|d| match d {
                    model::DefineRule::AssignCombine(_, _, p)
                    | model::DefineRule::CombineOnly(_, _, p) => p,
                })
                .ok_or_else(|| RelaxError::StartRuleNotDefined {
                    span: ctx.convert_span(&grammar.span),
                })
        } else {
            Err(RelaxError::StartRuleNotDefined {
                span: ctx.convert_span(&grammar.span),
            })
        }
    }

    fn compile_grammar_contents(
        &mut self,
        child_ctx: &mut Context,
        content: &[types::GrammarContent],
    ) -> Result<(), RelaxError> {
        for g in content {
            self.compile_grammar_content_item(child_ctx, g)?;
        }
        Ok(())
    }

    fn compile_grammar_content_item(
        &mut self,
        child_ctx: &mut Context,
        g: &types::GrammarContent,
    ) -> Result<(), RelaxError> {
        match g {
            types::GrammarContent::Define(d) => self.compile_define(child_ctx, d),
            types::GrammarContent::Div(d) => {
                Self::check_annotations(child_ctx, d.annotations.as_ref())?;
                self.compile_grammar_div(child_ctx, &d.content)
            }
            types::GrammarContent::Include(i) => self.compile_include(child_ctx, i),
            types::GrammarContent::Annotation(e) => {
                Self::compile_annotation_element(child_ctx, AnnotationNameRole::ForeignElement, e)
                    .map(|_| ())
            }
        }
    }

    fn compile_sequence(
        &mut self,
        ctx: &mut Context,
        span: &types::Span,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Group(mut v, _) => i.append(&mut v),
            _ => i.push(a),
        }
        match b {
            Pattern::Group(mut v, _) => i.append(&mut v),
            _ => i.push(b),
        }
        Ok(model::Pattern::Group(i, ctx.convert_span(span)))
    }
    fn compile_interleave(
        &mut self,
        ctx: &mut Context,
        span: &types::Span,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Interleave(mut v, _) => i.append(&mut v),
            _ => i.push(a),
        }
        match b {
            Pattern::Interleave(mut v, _) => i.append(&mut v),
            _ => i.push(b),
        }
        Ok(model::Pattern::Interleave(i, ctx.convert_span(span)))
    }
    fn compile_choice(
        &mut self,
        ctx: &mut Context,
        span: &types::Span,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut c = vec![];
        match a {
            Pattern::Choice(mut v, _) => c.append(&mut v),
            _ => c.push(a),
        }
        match b {
            Pattern::Choice(mut v, _) => c.append(&mut v),
            _ => c.push(b),
        }
        Ok(model::Pattern::Choice(c, ctx.convert_span(span)))
    }
    fn compile_datatype_value_pattern(
        &mut self,
        ctx: &mut Context,
        datatype_value: &types::DatatypeValuePattern,
    ) -> Result<model::Pattern, RelaxError> {
        // the default datatype if none is explicitly specified is 'token'
        let name = datatype_value
            .1
            .as_ref()
            .unwrap_or(&types::DatatypeName::Token);
        let name = match name {
            DatatypeName::String => DatatypeName::String,
            DatatypeName::Token => DatatypeName::Token,
            DatatypeName::CName(QName(namespace, name)) => DatatypeName::CName(QName(
                NcName(
                    namespace.0.clone(),
                    ctx.try_datatype_uri_for_prefix(namespace)?.to_string(),
                ),
                NcName(name.0.clone(), name.1.clone()),
            )),
            DatatypeName::NamespacedName(NamespacedName {
                namespace_uri,
                localname,
            }) => DatatypeName::CName(QName(
                NcName(namespace_uri.0.clone(), namespace_uri.as_string_value()),
                NcName(localname.0.clone(), localname.1.clone()),
            )),
        };
        let span = ctx.convert_span(&datatype_value.0);
        let datatype = self
            .datatype_compiler
            .datatype_value(
                ctx,
                &name,
                &datatype_value.2.as_string_value(),
                datatype_value.3.as_deref(),
                &datatype_value.4,
            )
            .map_err(RelaxError::DatatypeError)?;
        Ok(Pattern::DatatypeValue { datatype, span })
    }
    fn compile_datatype_name_pattern(
        &mut self,
        ctx: &mut Context,
        datatype_name: &types::DatatypeNamePattern,
    ) -> Result<model::Pattern, RelaxError> {
        let tmp = [];
        let params = if let Some(ref params) = datatype_name.2 {
            &params[..]
        } else {
            &tmp
        };
        for param in params {
            Self::check_annotations(ctx, param.annotations.as_ref())?;
        }
        let name = match &datatype_name.1 {
            DatatypeName::String => DatatypeName::String,
            DatatypeName::Token => DatatypeName::Token,
            DatatypeName::CName(QName(namespace, name)) => DatatypeName::CName(QName(
                NcName(
                    namespace.0.clone(),
                    ctx.try_datatype_uri_for_prefix(namespace)?.to_string(),
                ),
                NcName(name.0.clone(), name.1.clone()),
            )),
            DatatypeName::NamespacedName(NamespacedName {
                namespace_uri,
                localname,
            }) => DatatypeName::CName(QName(
                NcName(namespace_uri.0.clone(), namespace_uri.as_string_value()),
                NcName(localname.0.clone(), localname.1.clone()),
            )),
        };
        let span = ctx.convert_span(&datatype_name.0);
        let datatype = self
            .datatype_compiler
            .datatype_name(ctx, &name, params)
            .map_err(RelaxError::DatatypeError)?;
        Ok(model::Pattern::DatatypeName {
            datatype,
            except: if let Some(ref except) = datatype_name.3 {
                Some(Box::new(self.compile_pattern(ctx, except)?))
            } else {
                None
            },
            span,
        })
    }

    fn compile_declaration(
        &mut self,
        ctx: &mut Context,
        dec: &types::Decl,
    ) -> Result<(), RelaxError> {
        match dec {
            types::Decl::DefaultNamespace(types::DefaultNamespaceDeclaration {
                span,
                prefix,
                uri,
            }) => {
                let (default, binding) = match uri {
                    NamespaceUriLiteral::Inherit => (
                        ctx.inherited_namespace_uri().to_string(),
                        NamespaceBinding::Inherit,
                    ),
                    NamespaceUriLiteral::Uri(uri) => {
                        let uri = uri.as_string_value();
                        (uri.clone(), NamespaceBinding::Uri(uri))
                    }
                };
                ctx.set_default_namespace(default);
                if let Some(p) = prefix {
                    let span = ctx.convert_span(span);
                    ctx.declare_namespace(p.clone(), binding, span)
                } else {
                    Ok(())
                }
            }
            types::Decl::Namespace(types::NamespaceDeclaration { span, prefix, uri }) => {
                let binding = match uri {
                    NamespaceUriLiteral::Inherit => NamespaceBinding::Inherit,
                    NamespaceUriLiteral::Uri(uri) => NamespaceBinding::Uri(uri.as_string_value()),
                };
                let span = ctx.convert_span(span);
                ctx.declare_namespace(prefix.clone(), binding, span)
            }
            types::Decl::Datatypes(types::DatatypesDeclaration { prefix, uri }) => {
                let span = ctx.convert_span(&uri.0);
                ctx.declare_datatype(prefix.clone(), uri.as_string_value(), span)
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn compile_nameclass(
        &mut self,
        ctx: &mut Context,
        elem_attr: ElemAttr,
        name_class: &types::NameClass,
    ) -> Result<model::NameClass, RelaxError> {
        // TODO: wrap namespace-prefix resolution failure with source-span information from the AST
        let result = match name_class {
            types::NameClass::Name(name) => match name {
                types::Name::Identifier(id) => Ok(model::NameClass::named(
                    if elem_attr == ElemAttr::Element {
                        ctx.default_namespace_uri().to_string()
                    } else {
                        "".to_string()
                    },
                    id.to_string(),
                )),
                types::Name::CName(cname) => Ok(model::NameClass::named(
                    ctx.namespace_uri_for_prefix(&cname.0)?.to_string(),
                    (cname.1).1.clone(),
                )),
                Name::NamespacedName(NamespacedName {
                    namespace_uri,
                    localname,
                }) => {
                    let ns = namespace_uri.as_string_value();
                    let ns = if ns.is_empty() && elem_attr == ElemAttr::Element {
                        let default = ctx.default_namespace_uri();
                        if default.is_empty() {
                            ns
                        } else {
                            default.to_string()
                        }
                    } else {
                        ns
                    };
                    Ok(model::NameClass::named(ns, localname.1.clone()))
                }
            },
            types::NameClass::NsName(types::NsName { name, except }) => {
                let except = if let Some(except) = except {
                    Some(self.compile_nameclass(ctx, elem_attr, except)?)
                } else {
                    None
                };
                let uri = match name {
                    types::NamespaceOrPrefix::NamespaceUri(uri) => uri.as_string_value(),
                    types::NamespaceOrPrefix::Prefix(prefix) => {
                        ctx.namespace_uri_for_prefix(prefix)?.to_string()
                    }
                };
                Ok(model::NameClass::ns_name(uri, except))
            }
            types::NameClass::AnyName(types::AnyName(except)) => {
                Ok(model::NameClass::any_name(if let Some(except) = except {
                    Some(self.compile_nameclass(ctx, elem_attr, except)?)
                } else {
                    None
                }))
            }
            types::NameClass::Alt(types::AltName(a, b)) => {
                let a = self.compile_nameclass(ctx, elem_attr, a)?;
                let b = self.compile_nameclass(ctx, elem_attr, b)?;
                Ok(model::NameClass::alt(a, b))
            }
            types::NameClass::Paren(types::ParenName(n)) => {
                self.compile_nameclass(ctx, elem_attr, n)
            }
            types::NameClass::Annotated(annos, inner) => {
                // Annotations on name classes are metadata-only; compile the inner name class
                Self::check_annotations(ctx, Some(annos))?;
                self.compile_nameclass(ctx, elem_attr, inner)
            }
        }?;
        Ok(result)
    }

    fn check_annotations(
        ctx: &Context,
        annos: Option<&types::Annotations>,
    ) -> Result<(), RelaxError> {
        if let Some(annos) = annos {
            Self::compile_annotations(ctx, annos)?;
        }
        Ok(())
    }

    fn compile_annotations(
        ctx: &Context,
        annos: &types::Annotations,
    ) -> Result<model::Annotation, RelaxError> {
        let documentation = annos
            .documentation
            .iter()
            .map(|d| model::AnnotationDocumentation {
                content: d.content.clone(),
                span: Some(ctx.convert_span(&d.span)),
            })
            .collect();

        let (attributes, mut elements) = match annos.initial {
            Some(ref initial) => (
                Self::compile_annotation_attributes(
                    ctx,
                    AnnotationNameRole::ForeignAttribute,
                    &initial.attribute_annotations,
                )?,
                initial
                    .element_annotations
                    .iter()
                    .map(|e| {
                        Self::compile_annotation_element(ctx, AnnotationNameRole::ForeignElement, e)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            None => (vec![], vec![]),
        };

        for e in &annos.follow_elements {
            elements.push(Self::compile_annotation_element(
                ctx,
                AnnotationNameRole::ForeignElement,
                e,
            )?);
        }

        Ok(model::Annotation {
            documentation,
            attributes,
            elements,
        })
    }

    fn compile_annotation_name(
        ctx: &Context,
        role: AnnotationNameRole,
        name: &types::Name,
    ) -> Result<(String, String), RelaxError> {
        let span = ctx.convert_span(&name.span());
        let (ns, local, declaration) = match name {
            types::Name::CName(QName(prefix, local)) => {
                let decl = ctx.namespace_decl(&prefix.1);
                let declaration = decl.and_then(|d| d.span);
                if decl.is_some_and(|d| d.binding == NamespaceBinding::Inherit) {
                    return Err(RelaxError::InvalidAnnotationName {
                        span,
                        reason: AnnotationNameError::InheritedNamespace {
                            prefix: prefix.1.clone(),
                        },
                        declaration,
                    });
                }
                let ns = ctx.namespace_uri_for_prefix(prefix)?;
                (ns.to_string(), local.1.clone(), declaration)
            }
            types::Name::Identifier(id) => ("".to_string(), id.to_string(), None),
            types::Name::NamespacedName(nn) => (
                nn.namespace_uri.as_string_value(),
                nn.localname.1.clone(),
                None,
            ),
        };
        let reason = match role {
            AnnotationNameRole::ForeignAttribute if ns.is_empty() => {
                Some(AnnotationNameError::UnqualifiedAttribute)
            }
            AnnotationNameRole::ForeignAttribute | AnnotationNameRole::ForeignElement
                if ns == RELAXNG_NS =>
            {
                Some(AnnotationNameError::RelaxNgNamespace)
            }
            AnnotationNameRole::ForeignAttribute | AnnotationNameRole::AnyAttribute
                if XMLNS_NS.contains(&&ns[..]) =>
            {
                Some(AnnotationNameError::XmlnsNamespace)
            }
            AnnotationNameRole::AnyAttribute if ns.is_empty() && local == "xmlns" => {
                Some(AnnotationNameError::XmlnsName)
            }
            _ => None,
        };
        match reason {
            Some(reason) => Err(RelaxError::InvalidAnnotationName {
                span,
                reason,
                declaration,
            }),
            None => Ok((ns, local)),
        }
    }

    fn compile_annotation_attributes(
        ctx: &Context,
        role: AnnotationNameRole,
        attrs: &[types::AnnotationAttribute],
    ) -> Result<Vec<model::AnnotationAttribute>, RelaxError> {
        let mut result: Vec<model::AnnotationAttribute> = Vec::with_capacity(attrs.len());
        for a in attrs {
            let (ns, local) = Self::compile_annotation_name(ctx, role, &a.name)?;
            let span = ctx.convert_span(&a.span);
            if let Some(original) = result
                .iter()
                .find(|o| o.namespace_uri == ns && o.local_name == local)
            {
                return Err(RelaxError::DuplicateAnnotationAttribute {
                    duplicate: span,
                    original: original.span.expect("always set when pushed"),
                    namespace_uri: ns,
                    local_name: local,
                });
            }
            result.push(model::AnnotationAttribute {
                namespace_uri: ns,
                local_name: local,
                value: a.value.as_string_value(),
                span: Some(span),
            });
        }
        Ok(result)
    }

    fn compile_annotation_element(
        ctx: &Context,
        role: AnnotationNameRole,
        elem: &types::AnnotationElement,
    ) -> Result<model::AnnotationElement, RelaxError> {
        let (ns, local) = Self::compile_annotation_name(ctx, role, &elem.name)?;
        let attributes = Self::compile_annotation_attributes(
            ctx,
            AnnotationNameRole::AnyAttribute,
            &elem.annotation_attributes,
        )?;
        let children = elem
            .annotation_elements_or_literals
            .iter()
            .map(|c| match c {
                types::AnnotationElementOrLiteral::Element(e) => {
                    Ok(model::AnnotationContent::Element(
                        Self::compile_annotation_element(ctx, AnnotationNameRole::AnyElement, e)?,
                    ))
                }
                types::AnnotationElementOrLiteral::Literal(l) => {
                    Ok(model::AnnotationContent::Text(l.as_string_value()))
                }
            })
            .collect::<Result<_, RelaxError>>()?;
        Ok(model::AnnotationElement {
            namespace_uri: ns,
            local_name: local,
            attributes,
            children,
            span: Some(ctx.convert_span(&elem.span)),
        })
    }
}

const RELAXNG_NS: &str = "http://relaxng.org/ns/structure/1.0";
/// The namespace of `xmlns` attributes.  The compact syntax spec gives it without the trailing
/// `/`, but the namespace's real name has one, so both are rejected.
const XMLNS_NS: [&str; 2] = [
    "http://www.w3.org/2000/xmlns/",
    "http://www.w3.org/2000/xmlns",
];

#[derive(Copy, Clone, PartialEq, Debug)]
enum AnnotationNameRole {
    ForeignElement,
    ForeignAttribute,
    AnyElement,
    AnyAttribute,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum ElemAttr {
    Element,
    Attribute,
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::*;
    // TODO: maybe take tests from https://github.com/relaxng/jing-trang/blob/master/mod/rng-validate/test/spectest.xml

    #[test]
    fn refs() {
        struct FS;
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "test.rnc" => {
                        "
                        start = foo
                        foo = element x { foo | empty }
                    "
                    }
                    other => panic!("No {other:?}"),
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let input = Path::new("test.rnc");
        let result = match c.compile(input) {
            Err(e) => {
                c.dump_diagnostic(&e);
                std::thread::sleep(std::time::Duration::from_secs(1));
                panic!("{e:?}");
            }
            Ok(s) => s,
        };

        let s = result.start.borrow();
        let start = s.as_ref().unwrap().pattern();
        assert_matches!(start, Pattern::Ref(_whence, _name, model::PatRef(ref1)) => {
            assert_matches!(ref1.borrow().as_ref(), Some(model::DefineRule::AssignCombine(_, _, patt)) => {
                assert_matches!(patt, Pattern::Element(_name, content_patt, _, _) => {
                    assert_matches!(**content_patt, Pattern::Choice(ref seq, _) => {
                        assert_matches!(seq[0], Pattern::Ref(_, _, model::PatRef(ref ref2)) => {
                            assert!(Rc::ptr_eq(ref1, ref2));
                        })
                    })
                })
            })
        })
    }

    #[test]
    fn include_self_ref() {
        struct FS;
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "main.rnc" => "start = grammar { start = blocks  include 'elements.rnc' }",
                    "elements.rnc" => "blocks = element container { blocks? }",
                    _ => {
                        return Err(RelaxError::Io(
                            name.to_path_buf(),
                            io::Error::from(io::ErrorKind::NotFound),
                        ));
                    }
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let input = Path::new("main.rnc");
        let result = match c.compile(input) {
            Ok(s) => s,
            Err(e) => {
                c.dump_diagnostic(&e);
                panic!("{e:?}");
            }
        };
        let s = result.start.borrow();
        let start = s.as_ref().unwrap().pattern();
        assert_matches!(start, Pattern::Ref(_whence, _name, model::PatRef(ref1)) => {
            assert_matches!(ref1.borrow().as_ref(), Some(model::DefineRule::AssignCombine(_, _, patt)) => {
                assert_matches!(patt, Pattern::Element(_name, content_patt, _, _) => {
                    assert_matches!(**content_patt, Pattern::Optional(ref patt, _) => {
                        assert_matches!(**patt, Pattern::Ref(_, _, model::PatRef(ref ref2)) => {
                            assert!(Rc::ptr_eq(ref1, ref2));
                        })
                    })
                })
            })
        })
    }

    #[test]
    fn unreachable_definition_detected() {
        struct FS;
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "test.rnc" => {
                        "
                        start = element root { empty }
                        orphan = element unused { text }
                    "
                    }
                    other => panic!("No {other:?}"),
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let result = c.compile(Path::new("test.rnc")).unwrap();
        let unreachable = result.unreachable_definitions();
        assert_eq!(unreachable.len(), 1);
        assert_eq!(unreachable[0].name, "orphan");
    }

    #[test]
    fn ref_to_attributes_grouped_with_data() {
        // Regression test: a ref to an attributes-only definition grouped with
        // a data type should not trigger the section 7.2 StringSequence restriction.
        // This mirrors the Atom RFC 4287 pattern: atomDateConstruct = atomCommonAttributes, xsd:dateTime
        struct FS;
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "test.rnc" => {
                        "
                        start = element root { commonAttrs, xsd:dateTime }
                        commonAttrs = attribute id { text }?, attribute class { text }?
                        "
                    }
                    other => panic!("No {other:?}"),
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        c.compile(Path::new("test.rnc")).expect(
            "schema with ref to attributes grouped with data type should compile without error",
        );
    }

    #[test]
    fn all_definitions_reachable() {
        struct FS;
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "test.rnc" => {
                        "
                        start = foo
                        foo = element x { bar }
                        bar = text
                    "
                    }
                    other => panic!("No {other:?}"),
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let result = c.compile(Path::new("test.rnc")).unwrap();
        let unreachable = result.unreachable_definitions();
        assert!(unreachable.is_empty());
    }

    /// Schema files held in memory, as (name, text) pairs
    struct MemFiles(Vec<(&'static str, &'static str)>);
    impl Files for MemFiles {
        fn load(&self, name: &Path) -> Result<String, RelaxError> {
            let name = name.to_str().unwrap();
            let (_, text) = self.0.iter().find(|(n, _)| *n == name).expect(name);
            Ok(text.to_string())
        }
    }

    #[test]
    fn include_annotated_grammar() {
        // issue #26
        for inc in [
            "[ xml:lang = \"en\" ] grammar { start = element a { empty } }",
            "## doc\ngrammar { start = element a { empty } }",
        ] {
            let mut c = Compiler::new(
                MemFiles(vec![("main.rnc", "include \"inc.rnc\""), ("inc.rnc", inc)]),
                Syntax::Compact,
            );
            if let Err(e) = c.compile(Path::new("main.rnc")) {
                panic!("including {inc:?} failed: {e:?}");
            }
        }
    }

    #[test]
    fn included_file_inherits_default_namespace() {
        for (main, expected) in [
            (
                "default namespace = \"urn:a\"\ninclude \"inc.rnc\"",
                "urn:a",
            ),
            (
                "default namespace = \"urn:a\"\nnamespace b = \"urn:b\"\ninclude \"inc.rnc\" inherit = b",
                "urn:b",
            ),
            (
                "default namespace = \"urn:a\"\nstart = external \"ext.rnc\"",
                "urn:a",
            ),
            (
                "namespace b = \"urn:b\"\nstart = external \"ext.rnc\" inherit = b",
                "urn:b",
            ),
        ] {
            let mut c = Compiler::new(
                MemFiles(vec![
                    ("main.rnc", main),
                    ("inc.rnc", "start = element foo { empty }"),
                    ("ext.rnc", "element foo { empty }"),
                ]),
                Syntax::Compact,
            );
            let result = c
                .compile(Path::new("main.rnc"))
                .unwrap_or_else(|e| panic!("{main:?} failed to compile: {e:?}"));
            let start = result.start.borrow();
            let Some(model::Pattern::Element(model::NameClass::Named { namespace_uri, .. }, ..)) =
                start.as_ref().map(|d| d.pattern())
            else {
                panic!("{main:?}: expected named element as start pattern");
            };
            assert_eq!(namespace_uri, expected, "{main:?}");
        }
    }

    #[test]
    fn namespace_declared_as_inherit() {
        for (main, inc, expected) in [
            ("namespace f = inherit\nelement f:x { empty }", "", ""),
            ("default namespace = inherit\nelement x { empty }", "", ""),
            (
                "default namespace = \"urn:a\"\ninclude \"inc.rnc\"",
                "default namespace = \"urn:d\"\nnamespace f = inherit\nstart = element f:x { empty }",
                "urn:a",
            ),
            (
                "default namespace = \"urn:a\"\ninclude \"inc.rnc\"",
                "default namespace = inherit\nnamespace g = \"urn:g\"\nstart = element x { empty }",
                "urn:a",
            ),
            (
                "namespace b = \"urn:b\"\ninclude \"inc.rnc\" inherit = b",
                "default namespace f = inherit\nstart = element f:x { empty }",
                "urn:b",
            ),
            (
                "default namespace = \"urn:a\"\nnamespace f = inherit\ninclude \"inc.rnc\" inherit = f",
                "start = element x { empty }",
                "",
            ),
        ] {
            let start = compile_include_case(main, inc)
                .unwrap_or_else(|e| panic!("{main:?} / {inc:?} failed to compile: {e:?}"));
            let start = start.borrow();
            let Some(model::Pattern::Element(model::NameClass::Named { namespace_uri, .. }, ..)) =
                start.as_ref().map(|d| d.pattern())
            else {
                panic!("{main:?} / {inc:?}: expected named element as start pattern");
            };
            assert_eq!(namespace_uri, expected, "{main:?} / {inc:?}");
        }
    }

    fn compile_compact(schema: &'static str) -> Result<CompileResult, RelaxError> {
        Compiler::new(MemFiles(vec![("main.rnc", schema)]), Syntax::Compact)
            .compile(Path::new("main.rnc"))
    }

    #[test]
    fn annotation_name_constraints() {
        const DECLS: &str = "namespace f = \"urn:f\"\n\
            namespace g = \"urn:f\"\n\
            namespace e = \"\"\n\
            namespace r = \"http://relaxng.org/ns/structure/1.0\"\n\
            namespace n = \"http://www.w3.org/2000/xmlns/\"\n\
            namespace i = inherit\n";
        let invalid = |reason| move |e: &RelaxError| matches!(e, RelaxError::InvalidAnnotationName { reason: r, .. } if *r == reason);
        let inherited = invalid(AnnotationNameError::InheritedNamespace {
            prefix: "i".to_string(),
        });
        let rng = invalid(AnnotationNameError::RelaxNgNamespace);
        let xmlns = invalid(AnnotationNameError::XmlnsNamespace);
        let xmlns_name = invalid(AnnotationNameError::XmlnsName);
        let unqualified = invalid(AnnotationNameError::UnqualifiedAttribute);
        let duplicate =
            |e: &RelaxError| matches!(e, RelaxError::DuplicateAnnotationAttribute { .. });
        let undefined = |e: &RelaxError| matches!(e, RelaxError::UndefinedNamespacePrefix { .. });
        type Check<'a> = &'a dyn Fn(&RelaxError) -> bool;
        let cases: &[(&str, Check)] = &[
            ("element x { [ e:a = \"1\" ] empty }", &unqualified),
            ("element x { [ r:a = \"1\" ] empty }", &rng),
            ("element x { [ r:e [] ] empty }", &rng),
            ("element x { empty >> r:e [] }", &rng),
            ("element [ r:a = \"1\" ] x { empty }", &rng),
            ("[ r:a = \"1\" ] start = element x { empty }", &rng),
            ("start = element x { empty }\nr:e []", &rng),
            ("element x { [ n:a = \"1\" ] empty }", &xmlns),
            ("element x { [ f:e [ n:a = \"1\" ] ] empty }", &xmlns),
            ("element x { [ f:e [ xmlns = \"1\" ] ] empty }", &xmlns_name),
            (
                "element x { [ f:e [ \\xmlns = \"1\" ] ] empty }",
                &xmlns_name,
            ),
            (
                "element x { [ f:a = \"1\" f:a = \"2\" ] empty }",
                &duplicate,
            ),
            (
                "element x { [ f:a = \"1\" g:a = \"2\" ] empty }",
                &duplicate,
            ),
            (
                "element x { [ f:e [ a = \"1\" a = \"2\" ] ] empty }",
                &duplicate,
            ),
            (
                "[ f:a = \"1\" f:a = \"2\" ] div { start = element x { empty } }",
                &duplicate,
            ),
            ("element x { [ i:a = \"1\" ] empty }", &inherited),
            ("element x { [ i:e [] ] empty }", &inherited),
            ("element x { [ f:e [ i:a = \"1\" ] ] empty }", &inherited),
            ("element x { [ q:a = \"1\" ] empty }", &undefined),
            ("element x { [ q:e [] ] empty }", &undefined),
            (
                "element x { [ f:e [ q:a = \"1\" q:e [] ] ] empty }",
                &undefined,
            ),
            (
                "element x { xsd:string { [ q:a = \"1\" ] length = \"1\" } }",
                &undefined,
            ),
        ];
        for (body, expected) in cases {
            let schema: &'static str = format!("{DECLS}{body}").leak();
            match compile_compact(schema) {
                Ok(_) => panic!("{body:?} should not compile"),
                Err(e) => assert!(expected(&e), "{body:?}: unexpected {e:?}"),
            }
        }

        for body in [
            "element x { [ f:a = \"1\" f:b = \"1\" g:c = \"1\" ] empty }",
            "element x { [ f:e [ a = \"1\" f:a = \"2\" r:b = \"3\" r:c [] d [] ] ] empty >> f:z [] }",
            "default namespace = \"http://relaxng.org/ns/structure/1.0\"\n\
             element x { [ e [] ] empty }",
            "element i:x { attribute i:a { text } }",
        ] {
            let schema: &'static str = format!("{DECLS}{body}").leak();
            if let Err(e) = compile_compact(schema) {
                panic!("{body:?} failed to compile: {e:?}");
            }
        }
    }

    #[test]
    fn annotation_name_error_points_at_namespace_declaration() {
        const SCHEMA: &str = "namespace f = \"urn:f\"\n\
            namespace r = \"http://relaxng.org/ns/structure/1.0\"\n\
            element x { [ r:a = \"1\" ] empty }";
        let mut c = Compiler::new(MemFiles(vec![("main.rnc", SCHEMA)]), Syntax::Compact);
        let Err(RelaxError::InvalidAnnotationName {
            declaration: Some(declaration),
            ..
        }) = c.compile(Path::new("main.rnc"))
        else {
            panic!("expected an annotation name error with a declaration span");
        };
        assert_eq!(
            c.code_map()
                .look_up_span(declaration)
                .file
                .source_slice(declaration),
            "namespace r = \"http://relaxng.org/ns/structure/1.0\""
        );
    }

    /// Compiles `main.rnc`, which includes `inc.rnc`, and returns the start rule, or the error
    /// with any `IncludeError` wrappers removed
    fn compile_include_case(
        main: &'static str,
        inc: &'static str,
    ) -> Result<Rc<RefCell<Option<model::DefineRule>>>, RelaxError> {
        let mut c = Compiler::new(
            MemFiles(vec![("main.rnc", main), ("inc.rnc", inc)]),
            Syntax::Compact,
        );
        match c.compile(Path::new("main.rnc")) {
            Ok(s) => Ok(s.start),
            Err(mut e) => {
                while let RelaxError::IncludeError(_, inner) = e {
                    e = *inner;
                }
                Err(e)
            }
        }
    }

    #[test]
    fn include_override_uses_includers_namespace_prefixes() {
        // issue #49
        let start = compile_include_case(
            "namespace foo = \"urn:foo\"\n\
             include \"inc.rnc\" {\n  start = element doc { attribute foo:a { text }? }\n}",
            "start = element doc { empty }",
        )
        .unwrap_or_else(|e| panic!("failed to compile: {e:?}"));
        let start = start.borrow();
        let model::Pattern::Element(_, content, ..) = start.as_ref().unwrap().pattern() else {
            panic!("expected element as start pattern");
        };
        let model::Pattern::Optional(ref attr, _) = **content else {
            panic!("expected optional content, got {content:?}");
        };
        let model::Pattern::Attribute(
            model::NameClass::Named {
                ref namespace_uri, ..
            },
            ..,
        ) = **attr
        else {
            panic!("expected named attribute, got {attr:?}");
        };
        assert_eq!(namespace_uri, "urn:foo");
    }

    #[test]
    fn include_override_uses_includers_default_namespace() {
        // the `inherit` namespace applies to the included file, not to the overrides
        let start = compile_include_case(
            "default namespace = \"urn:a\"\nnamespace b = \"urn:b\"\n\
             include \"inc.rnc\" inherit = b {\n  start = element doc { empty }\n}",
            "start = element doc { empty }",
        )
        .unwrap_or_else(|e| panic!("failed to compile: {e:?}"));
        let start = start.borrow();
        let model::Pattern::Element(model::NameClass::Named { namespace_uri, .. }, ..) =
            start.as_ref().unwrap().pattern()
        else {
            panic!("expected named element as start pattern");
        };
        assert_eq!(namespace_uri, "urn:a");
    }

    #[test]
    fn include_override_uses_includers_datatype_prefixes() {
        let start = compile_include_case(
            "datatypes x = \"http://www.w3.org/2001/XMLSchema-datatypes\"\n\
             include \"inc.rnc\" {\n  start = element doc { x:int }\n}",
            "start = element doc { empty }",
        )
        .unwrap_or_else(|e| panic!("failed to compile: {e:?}"));
        let start = start.borrow();
        let model::Pattern::Element(_, content, ..) = start.as_ref().unwrap().pattern() else {
            panic!("expected element as start pattern");
        };
        assert_matches!(**content, model::Pattern::DatatypeName { .. });
    }

    #[test]
    fn include_override_cannot_use_included_files_namespace_prefixes() {
        let Err(err) = compile_include_case(
            "include \"inc.rnc\" {\n  start = element bar:doc { empty }\n}",
            "namespace bar = \"urn:bar\"\nstart = element bar:doc { empty }",
        ) else {
            panic!("override should not see the included file's prefix");
        };
        assert_matches!(err, RelaxError::UndefinedNamespacePrefix { ref prefix, .. } if prefix == "bar");
    }

    #[test]
    fn included_file_cannot_use_includers_datatype_prefixes() {
        let Err(err) = compile_include_case(
            "datatypes x = \"http://www.w3.org/2001/XMLSchema-datatypes\"\n\
             include \"inc.rnc\"",
            "start = element doc { x:string }",
        ) else {
            panic!("included file should not see the includer's datatype prefix");
        };
        assert_matches!(err, RelaxError::UndefinedDatatypePrefix { ref prefix, .. } if prefix == "x");
    }

    #[test]
    fn external_file_cannot_use_referencers_datatype_prefixes() {
        let mut c = Compiler::new(
            MemFiles(vec![
                (
                    "main.rnc",
                    "datatypes x = \"http://www.w3.org/2001/XMLSchema-datatypes\"\n\
                     start = external \"ext.rnc\"",
                ),
                ("ext.rnc", "element doc { x:string }"),
            ]),
            Syntax::Compact,
        );
        let Err(mut err) = c.compile(Path::new("main.rnc")) else {
            panic!("external file should not see the referencer's datatype prefix");
        };
        while let RelaxError::IncludeError(_, inner) = err {
            err = *inner;
        }
        assert_matches!(err, RelaxError::UndefinedDatatypePrefix { ref prefix, .. } if prefix == "x");
    }

    #[test]
    fn pattern_named_start_is_not_the_start_pattern() {
        // as in TEI ODD schemas
        for (name, syntax, text) in [
            (
                "test.rnc",
                Syntax::Compact,
                "start = element a { \\start }\n\\start = element b { empty }",
            ),
            (
                "test.rng",
                Syntax::Xml,
                r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                     <start><element name="a"><ref name="start"/></element></start>
                     <define name="start"><element name="b"><empty/></element></define>
                   </grammar>"#,
            ),
        ] {
            let mut c = Compiler::new(MemFiles(vec![(name, text)]), syntax);
            let result = c
                .compile(Path::new(name))
                .unwrap_or_else(|e| panic!("{name} failed to compile: {e:?}"));
            assert!(result.unreachable_definitions().is_empty(), "{name}");
            let start = result.start.borrow();
            let Some(model::Pattern::Element(_, content, _, _)) =
                start.as_ref().map(|d| d.pattern())
            else {
                panic!("{name}: expected element as start pattern");
            };
            assert_matches!(**content, model::Pattern::Ref(_, ref n, _) if n == "start");
        }
    }

    #[test]
    fn attribute_overlap_ignores_not_allowed_branch() {
        // The validator.nu HTML5 schema disables one definition of cols/rows by interleaving
        // it with notAllowed, then adds another; section 7.3 applies after simplification
        // has removed the disabled branch.
        let mut c = Compiler::new(
            MemFiles(vec![(
                "test.rnc",
                "start = element textarea { textarea.attrs }
                 textarea.attrs = wf1
                 wf1 = inner
                 inner = attribute cols { text } & attribute rows { text }
                 inner &= notAllowed
                 wf1 |= empty
                 textarea.attrs &= attribute rows { text }? & attribute cols { text }?",
            )]),
            Syntax::Compact,
        );
        if let Err(e) = c.compile(Path::new("test.rnc")) {
            panic!("{e:?}");
        }
    }

    #[test]
    fn optional_not_allowed_does_not_hide_attribute_overlap() {
        // `notAllowed?` simplifies to `empty`, so the group around it is still checked
        let text = "start = element e {\n  attribute b { notAllowed }?,\n  attribute a { text },\n  attribute a { text }\n}";
        let mut c = Compiler::new(MemFiles(vec![("test.rnc", text)]), Syntax::Compact);
        let Err(RelaxError::RestrictionViolation(v)) = c.compile(Path::new("test.rnc")) else {
            panic!("expected a restriction violation");
        };
        assert_matches!(v.kind, restrictions::RestrictionKind::AttributeNameOverlap);
        let line_of = |span: codemap::Span| c.codemap.look_up_span(span).begin.line;
        // 0-based lines of the second and first `attribute a`
        assert_eq!(line_of(v.span), 3);
        assert_eq!(line_of(v.other_span.expect("other span")), 2);
    }

    #[test]
    fn unreferenced_pattern_named_start_is_unreachable() {
        let mut c = Compiler::new(
            MemFiles(vec![(
                "test.rnc",
                "start = element a { empty }\n\\start = empty",
            )]),
            Syntax::Compact,
        );
        let result = c.compile(Path::new("test.rnc")).unwrap();
        let unreachable = result.unreachable_definitions();
        assert_eq!(unreachable.len(), 1);
        assert_eq!(unreachable[0].name, "start");
    }
}
