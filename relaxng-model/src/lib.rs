use crate::datatype::xsd::{FacetError, XsdDatatypeError};
use crate::datatype::{DatatypeCompiler, Errors};
use crate::model::Pattern;
use codemap::CodeMap;

use nom::error::Error;
use nom_locate::LocatedSpan;
use relaxng_syntax::types::{
    DatatypeName, Name, NamespaceUriLiteral, NamespacedName, NcName, QName, Schema,
};
use relaxng_syntax::{compact, types};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io;
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

pub mod datatype;
pub mod model;

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
}

impl Syntax {
    fn parse(&self, file: &Arc<codemap::File>) -> Result<Schema, RelaxError> {
        match self {
            Syntax::Xml => relaxng_syntax::xml::parse(file.source()).map_err(|e| {
                let span = match &e {
                    relaxng_syntax::xml::Error::Expected(span, _) => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    relaxng_syntax::xml::Error::Unexpected(span, _) => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    relaxng_syntax::xml::Error::Xml(span, _) => {
                        file.span.subspan(span.start as _, span.end as _)
                    }
                    relaxng_syntax::xml::Error::Todo(_) => file.span.subspan(0, 0),
                };
                RelaxError::XmlParse(span, e)
            }),
            Syntax::Compact => {
                let input = LocatedSpan::new(file.source());
                let schema = compact::schema(input).map_err(|e| match e {
                    nom::Err::Error(Error { input, code }) => RelaxError::Parse(
                        file.span.subspan(
                            input.location_offset() as _,
                            (input.location_offset() + input.fragment().len()) as _,
                        ),
                        code,
                    ),
                    nom::Err::Failure(Error { input, code }) => RelaxError::Parse(
                        file.span.subspan(
                            input.location_offset() as _,
                            (input.location_offset() + input.fragment().len()) as _,
                        ),
                        code,
                    ),
                    nom::Err::Incomplete(_n) => unimplemented!("{:?}", e),
                })?;
                Ok(schema)
            }
        }
    }
}

type Span = Range<usize>;

#[derive(Debug)]
pub enum RelaxError {
    Io(PathBuf, io::Error),
    IncludeError(codemap::Span, Box<RelaxError>),
    // TODO: remove nom type from public API
    Parse(codemap::Span, nom::error::ErrorKind),
    XmlParse(codemap::Span, relaxng_syntax::xml::Error),
    DuplicateDefinition {
        name: String,
        duplicate: codemap::Span,
        original: codemap::Span,
    },
    IncompatibleCombination {
        file: PathBuf,
        combine: Span,
        original: Span,
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
    DatatypePrefixAlreadyDefined(String),
    DuplicateParameterName(String),
    /// the grammar has 'start = ...' more than once
    StartRuleRedefined,
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
}

enum Context<'a> {
    Root {
        file: Arc<codemap::File>,
        refs: RefCell<HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>>,
        namespaces: HashMap<String, String>,
        default_namespace: String,
        datatypes: HashMap<String, String>,
        //start: Option<DefineRule>,
    },
    Include {
        parent: &'a Context<'a>,
        file: Arc<codemap::File>,
        // we track what names are overiden for the included grammar, and also weather that
        // grammar actually had such a definition (it is an error to provide an override for
        // a name that's not defined in the include)
        overrides: RefCell<HashMap<String, (codemap::Span, bool)>>,
        namespaces: HashMap<String, String>,
        default_namespace: String,
        datatypes: HashMap<String, String>,
    },
    IncludeOverrides {
        parent: &'a Context<'a>,
    },
    Grammar {
        parent: &'a Context<'a>,
        refs: RefCell<HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>>,
    },
    // We track the context of a definition since the body of that definition must not reference
    // the identifier being defined, unless the reference is in the body of an element definition
    Define {
        parent: &'a Context<'a>,
        id: types::Identifier,
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
            "http://www.w3.org/XML/1998/namespace".to_string(),
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
            datatypes,
            //start: None,
        }
    }

    /// Creates a new context for an included file
    fn new_include(
        &self,
        span: codemap::Span,
        file: Arc<codemap::File>,
    ) -> Result<Context, RelaxError> {
        self.check_include(span, file.clone())?;
        let mut namespaces = HashMap::new();
        namespaces.insert(
            "xml".to_string(),
            "http://www.w3.org/XML/1998/namespace".to_string(),
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
            default_namespace: "".to_string(),
            datatypes,
        })
    }

    /// Creates a new context for the body of an include-overrides block
    fn new_inc_overrides(&self) -> Context {
        match self {
            Context::Include { .. } => Context::IncludeOverrides { parent: self },
            _ => {
                unreachable!("Parent was not a Context::Include");
            }
        }
    }

    /// Creates a new context for the body of a grammar
    fn new_grammar(&self) -> Context {
        Context::Grammar {
            parent: self,
            refs: RefCell::new(HashMap::new()),
        }
    }

    /// Creates a new context for the body of a definition
    fn new_define(&self, id: &types::Identifier) -> Context {
        Context::Define {
            parent: self,
            id: id.clone(),
        }
    }

    /// Creates a new context for the body of an element
    fn new_element(&self, element_span: codemap::Span) -> Result<Context, RelaxError> {
        if let Some(attribute_span) = self.parent_attribute() {
            return Err(RelaxError::ElementAsChildOfAttribute {
                attribute_span,
                element_span,
            });
        }
        Ok(Context::Element { parent: self })
    }

    /// Creates a new context for the body of an element
    fn new_attribute(&self, span: codemap::Span) -> Context {
        Context::Attribute { parent: self, span }
    }

    fn file(&self) -> Arc<codemap::File> {
        match self {
            Context::Root { file, .. } | Context::Include { file, .. } => file.clone(),
            Context::IncludeOverrides { parent, .. } => {
                if let Context::Include { parent, .. } = parent {
                    parent.file()
                } else {
                    unreachable!("Should not be possible for Context::IncludeOverrides to have parent other than Context::Include")
                }
            }
            Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.file(),
        }
    }

    /// Utility to convert a simple Span from the relaxng-compact-syntax model into the specialised
    /// Span type that we use for diagnostics
    fn convert_span(&self, span: &model::Span) -> codemap::Span {
        self.file().span.subspan(span.start as u64, span.end as u64)
    }

    // TODO: an enum to indicate either 'no namespace', or a uri
    fn default_namespace_uri(&self) -> &str {
        match self {
            Context::Root {
                default_namespace, ..
            }
            | Context::Include {
                default_namespace, ..
            } => &default_namespace[..],
            Context::IncludeOverrides { parent, .. }
            | Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.default_namespace_uri(),
        }
    }

    fn declare_namespace(&mut self, prefix: String, uri: String) -> Result<(), RelaxError> {
        match self {
            Context::Root { namespaces, .. } | Context::Include { namespaces, .. } => {
                if let Some(_old_uri) = namespaces.get(&prefix) {
                    //Err(RelaxError::NamespacePrefixAlreadyDefined { span: self.convert_span(), prefix })
                    Ok(())
                } else {
                    namespaces.insert(prefix, uri);
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
    fn namespace_uri_for_prefix_str(&self, prefix: &str) -> Option<&str> {
        match self {
            Context::Root { namespaces, .. } | Context::Include { namespaces, .. } => {
                namespaces.get(prefix).map(|s| &s[..])
            }
            Context::IncludeOverrides { parent, .. }
            | Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.namespace_uri_for_prefix_str(prefix),
        }
    }

    fn declare_datatype(&mut self, prefix: String, uri: String) -> Result<(), RelaxError> {
        match self {
            Context::Root { datatypes, .. } | Context::Include { datatypes, .. } => {
                match datatypes.entry(prefix) {
                    Entry::Occupied(e) => {
                        Err(RelaxError::DatatypePrefixAlreadyDefined(e.key().clone()))
                    }
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
            Context::Root { datatypes, .. } => {
                // TODO: return Option, and have caller work out RelaxError value
                datatypes.get(prefix).map(|s| &s[..])
            }
            Context::Include {
                parent, datatypes, ..
            } => {
                if let Some(uri) = datatypes.get(prefix).map(|s| &s[..]) {
                    Some(uri)
                } else {
                    parent.datatype_uri_for_prefix(prefix)
                }
            }
            Context::IncludeOverrides { parent }
            | Context::Grammar { parent, .. }
            | Context::Define { parent, .. }
            | Context::Element { parent }
            | Context::Attribute { parent, .. } => parent.datatype_uri_for_prefix(prefix),
        }
    }

    fn define(&self, id: &str, rule: model::DefineRule) -> Result<(), RelaxError> {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                let mut refs = refs.borrow_mut();
                if refs.contains_key(id) {
                    let a = refs.get(id);
                    let mut rule_ref = a.unwrap().borrow_mut();
                    *rule_ref = Compiler::<FsFiles>::merge(id, rule_ref.take(), Some(rule))?
                } else {
                    refs.insert(id.to_string(), Rc::new(RefCell::new(Some(rule))));
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
                        // TODO: return Err, not panic
                        panic!("TODO: redefined override {:?}", id)
                    } else {
                        overrides.insert(id.to_string(), (*rule.span(), false));
                        parant_of_include.define(id, rule)?;
                    };
                    Ok(())
                } else {
                    unreachable!("Should not be possible for Context::IncludeOverrides to have parent other than Context::Include")
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
        ref_id: &str,
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
                if id.1 == ref_id {
                    panic!(
                        "unexpected ref lookup of {:?} within its own definition",
                        id.1
                    );
                }
                parent.get_ref(ref_id)
            }
        }
    }

    fn ref_iter(
        &self,
    ) -> impl Iterator<
        Item = (
            std::string::String,
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
        self.check_ref_recursion(ref_id)?;
        self.acquire_ref_impl(ref_id)
    }
    fn acquire_ref_impl(&self, ref_id: &types::Identifier) -> Result<model::PatRef, RelaxError> {
        match self {
            Context::Root { refs, .. } | Context::Grammar { refs, .. } => {
                let mut refs = refs.borrow_mut();
                if let Some(r) = refs.get(&ref_id.1) {
                    Ok(model::PatRef(r.clone()))
                } else {
                    let r = Rc::new(RefCell::new(None));
                    refs.insert(ref_id.1.to_string(), r.clone());
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
    /// Given the identifier if a reference, checks that the current context is not actually the
    /// definition of that same name.
    ///
    /// Such recursive references are allowed if the definition body includes an element, and the
    /// recursive reference is within the body of that element.
    fn check_ref_recursion(&self, ref_id: &types::Identifier) -> Result<(), RelaxError> {
        match self {
            Context::Root { .. } => Ok(()),
            Context::Include { .. } => Ok(()),
            Context::IncludeOverrides { .. } => Ok(()),
            Context::Grammar { .. } => Ok(()),
            Context::Define { id, .. } => {
                if id.1 == ref_id.1 {
                    Err(RelaxError::RecursiveReference {
                        ref_id: ref_id.1.clone(),
                        ref_span: self.convert_span(&ref_id.0),
                        def_id: id.1.clone(),
                        def_span: self.convert_span(&id.0),
                    })
                } else {
                    Ok(())
                }
            }
            Context::Element { .. } => Ok(()),
            Context::Attribute { parent, .. } => parent.check_ref_recursion(ref_id),
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
        Self::new(FsFiles, Syntax::Compact)
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
    // TODO: provide a simpler return-type
    // TODO: does this need to support URLs?
    pub fn compile(
        &mut self,
        name: &Path,
    ) -> Result<Rc<RefCell<Option<model::DefineRule>>>, RelaxError> {
        let (file, schema) = self.get_schema(name)?;
        let mut ctx = Context::new(file.clone());
        self.compile_schema(&mut ctx, schema)?;
        for (name, r) in ctx.ref_iter() {
            if r.borrow().is_none() {
                //println!("Undefined {:?} :(", name);
                return Err(RelaxError::UndefinedReference {
                    span: file.span.subspan(0, 0),
                    identifier: name,
                });
            } else {
                //println!("Defined {:?} => :)", name);
            }
        }
        if let Some(start) = ctx.get_ref("start") {
            let mut seen = HashSet::new();
            // TODO: this is a temporary hack to detect bad references; do this in a better way?
            self.check(&mut seen, start.borrow().as_ref().unwrap().pattern())?;
            Ok(start)
        } else {
            Err(RelaxError::StartRuleNotDefined { span: file.span })
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check(&self, seen: &mut HashSet<usize>, patt: &model::Pattern) -> Result<(), RelaxError> {
        match patt {
            Pattern::Choice(v) | Pattern::Interleave(v) | Pattern::Group(v) => {
                for p in v {
                    self.check(seen, p)?
                }
            }
            Pattern::Mixed(p)
            | Pattern::Optional(p)
            | Pattern::ZeroOrMore(p)
            | Pattern::OneOrMore(p)
            | Pattern::Attribute(_, p)
            | Pattern::Element(_, p)
            | Pattern::List(p) => self.check(seen, p)?,
            Pattern::Empty
            | Pattern::Text
            | Pattern::NotAllowed
            | Pattern::DatatypeValue { .. } => {}
            Pattern::Ref(span, name, def) => {
                let ptr = def.0.as_ptr() as usize;
                if !seen.contains(&ptr) {
                    seen.insert(ptr);
                    if let Some(rule) = def.0.borrow().as_ref() {
                        self.check(seen, rule.pattern())?
                    } else {
                        return Err(RelaxError::UndefinedReference {
                            span: *span,
                            identifier: name.to_string(),
                        });
                    }
                }
            }
            Pattern::DatatypeName { except, .. } => {
                if let Some(e) = except.as_ref() {
                    self.check(seen, e)?;
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
                    message: format!("Undefined: {:?}", identifier),
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
                relaxng_syntax::xml::Error::Expected(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("Expected: {}", msg),
                        code: None,
                        spans: vec![label],
                    }
                }
                relaxng_syntax::xml::Error::Unexpected(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("Unexpected {}", msg),
                        code: None,
                        spans: vec![label],
                    }
                }
                relaxng_syntax::xml::Error::Xml(_, msg) => {
                    let label = codemap_diagnostic::SpanLabel {
                        span: *span,
                        style: codemap_diagnostic::SpanStyle::Primary,
                        label: None,
                    };
                    codemap_diagnostic::Diagnostic {
                        level: codemap_diagnostic::Level::Error,
                        message: format!("XML parsing error: {}", msg),
                        code: None,
                        spans: vec![label],
                    }
                }
                _ => panic!("{:?}", err),
            },
            RelaxError::UndefinedNamespacePrefix { span, prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("Undefined namespace prefix {:?}", prefix),
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
                        message: format!("Unsupported datatype library namespace {:?}", namespace),
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
                                message: format!("The relaxng build-in datatype bibrary does not support the type {:?}", name),
                                code: None,
                                spans: vec![label]
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
                                    label: Some(format!("Invalid integer value: {}", msg)),
                                })
                            }
                            FacetError::InvalidFloat(span, msg) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid floating-point value: {}", msg)),
                                })
                            }
                            FacetError::InvalidPattern(span, err) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid pattern value: {}", err)),
                                })
                            }
                            FacetError::InvalidFacet(span, name) => {
                                labels.push(codemap_diagnostic::SpanLabel {
                                    span: *span,
                                    style: codemap_diagnostic::SpanStyle::Primary,
                                    label: Some(format!("Invalid facet for type: {}", name)),
                                })
                            }
                        }
                        codemap_diagnostic::Diagnostic {
                            level: codemap_diagnostic::Level::Error,
                            message: format!("Problematic facet for {} type", type_name),
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
                                "Unsupported datatype {:?} for XML Schema datatype library",
                                name
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
                            message: format!("Invalid value of type {:?}", type_name),
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
                    message: format!("Recursive reference to {:?}", ref_id),
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
                    message: format!("The datatype prefix {:?} is not defined", prefix),
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
                    message: format!("Duplicate definition of {:?}", name),
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
                    message: format!("The resource {:?} refers to itself recursively", name),
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
                        "...or add a definition for {:?} to this grammar",
                        name
                    )),
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!(
                        "Override {:?} has no corresponding definition in included grammar",
                        name
                    ),
                    code: None,
                    spans: vec![override_label, include_label],
                }
            }
            RelaxError::Parse(span, _kind) => {
                let label = codemap_diagnostic::SpanLabel {
                    span: *span,
                    style: codemap_diagnostic::SpanStyle::Primary,
                    label: None,
                };
                codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Parse error".to_string(),
                    code: None,
                    spans: vec![label],
                }
            }
            _ => panic!("{:?}", err),
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
        let schema = self.syntax.parse(&file)?;
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
                    match p {
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
                    ctx.define("start", model::DefineRule::AssignCombine(span, None, rule))?;
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
        if let Pattern::Choice(ref mut this) = choice {
            if let Pattern::Choice(mut other) = c {
                this.append(&mut other)
            } else {
                this.push(c)
            }
        } else {
            panic!("Not a Choice pattern {:?}", choice);
        }
    }
    fn append_interleave(interleave: &mut Pattern, c: Pattern) {
        if let Pattern::Interleave(ref mut this) = interleave {
            if let Pattern::Interleave(mut other) = c {
                this.append(&mut other)
            } else {
                this.push(c)
            }
        } else {
            panic!("Not an Interleave pattern {:?}", interleave);
        }
    }
    fn compile_define(
        &mut self,
        ctx: &mut Context,
        define: &types::Define,
    ) -> Result<(), RelaxError> {
        let mut def_ctx = ctx.new_define(&define.1);
        let id = (define.1).1.clone();
        let rule = self.compile_pattern(&mut def_ctx, &define.3)?;
        let span = ctx.convert_span(&(define.1).0);
        let new_rule = match define.2 {
            types::AssignMethod::Assign => model::DefineRule::AssignCombine(span, None, rule),
            types::AssignMethod::Choice => model::DefineRule::CombineOnly(
                span,
                model::CombineRule::Choice,
                Pattern::Choice(vec![rule]),
            ),
            types::AssignMethod::Interleave => model::DefineRule::CombineOnly(
                span,
                model::CombineRule::Interleave,
                Pattern::Interleave(vec![rule]),
            ),
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
    fn compile_include_div(
        &mut self,
        _ctx: &mut Context,
        _div: &[types::IncludeContent],
    ) -> Result<(), RelaxError> {
        unimplemented!()
    }
    fn compile_include(
        &mut self,
        ctx: &mut Context,
        inc: &types::Include,
    ) -> Result<(), RelaxError> {
        let path = Path::new(ctx.file().name())
            .parent()
            .expect("TODO: no parent?")
            .join(inc.0.as_string_value());
        let span = ctx
            .file()
            .span
            .subspan((inc.0).0.start as u64, (inc.0).0.end as u64);
        let (file, s) = self
            .get_schema(&path)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;
        // TODO: get the span of the grammar in the file, rather than the span of the whole file
        let include_span = file.span;

        let mut inc_ctx = ctx.new_include(span, file)?;

        if let Some(ref inherit) = inc.1 {
            let prefix = inherit.0.to_string();
            let namespace_uri = ctx
                .namespace_uri_for_prefix_str(&prefix)
                .ok_or_else(|| RelaxError::UndefinedNamespacePrefix {
                    span: ctx.convert_span(&inherit.0.span()),
                    prefix: prefix.clone(),
                })?
                .to_string();
            inc_ctx.declare_namespace(prefix, namespace_uri)?;
        }

        // replace any definitions for which the including file provides an override
        if let Some(ref overrides) = inc.2 {
            let mut override_ctx = inc_ctx.new_inc_overrides();
            for o in overrides.iter() {
                match o {
                    types::IncludeContent::Define(d) => {
                        self.compile_define(&mut override_ctx, d)
                            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?
                    }
                    types::IncludeContent::Div(d) => self
                        .compile_include_div(&mut override_ctx, &d[..])
                        .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?,
                    types::IncludeContent::Annotation(_) => (),
                }
            }
        }

        self.compile_schema(&mut inc_ctx, s)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;

        if let Context::Include { overrides, .. } = inc_ctx {
            for (name, (override_span, defined)) in overrides.borrow().iter() {
                if !defined {
                    return Err(RelaxError::OverrideMissingFromInclude {
                        name: name.clone(),
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
        use model::{CombineRule, DefineRule};
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
                DefineRule::CombineOnly(_, CombineRule::Choice, patt_b),
            ) => {
                Self::append_choice(&mut patt_a, patt_b);
                Ok(DefineRule::CombineOnly(this, CombineRule::Choice, patt_a))
            }
            (
                DefineRule::AssignCombine(this, Some(CombineRule::Choice), mut patt_a),
                DefineRule::CombineOnly(_, CombineRule::Choice, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Choice, mut patt_a),
                DefineRule::AssignCombine(_, Some(CombineRule::Choice), patt_b),
            )
            | (
                DefineRule::AssignCombine(this, None, mut patt_a),
                DefineRule::CombineOnly(_, CombineRule::Choice, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Choice, mut patt_a),
                DefineRule::AssignCombine(_, None, patt_b),
            ) => {
                let result = if let Pattern::Choice(_) = patt_a {
                    Self::append_choice(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Choice(vec![patt_a, patt_b])
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
                DefineRule::CombineOnly(_, CombineRule::Interleave, patt_b),
            ) => {
                Self::append_interleave(&mut patt_a, patt_b);
                Ok(DefineRule::CombineOnly(
                    this,
                    CombineRule::Interleave,
                    patt_a,
                ))
            }
            (
                DefineRule::AssignCombine(this, Some(CombineRule::Interleave), mut patt_a),
                DefineRule::CombineOnly(_, CombineRule::Interleave, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Interleave, mut patt_a),
                DefineRule::AssignCombine(_, Some(CombineRule::Interleave), patt_b),
            )
            | (
                DefineRule::AssignCombine(this, None, mut patt_a),
                DefineRule::CombineOnly(_, CombineRule::Interleave, patt_b),
            )
            | (
                DefineRule::CombineOnly(this, CombineRule::Interleave, mut patt_a),
                DefineRule::AssignCombine(_, None, patt_b),
            ) => {
                let result = if let Pattern::Interleave(_) = patt_a {
                    Self::append_interleave(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Interleave(vec![patt_a, patt_b])
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
            types::Pattern::Mixed(p) => Ok(model::Pattern::Mixed(Box::new(
                self.compile_pattern(ctx, &p.0)?,
            ))),
            types::Pattern::Identifier(i) => self.compile_ref(ctx, i),
            types::Pattern::Parent(p) => self.compile_parent(ctx, p),
            types::Pattern::Empty => Ok(model::Pattern::Empty),
            types::Pattern::Text => Ok(model::Pattern::Text),
            types::Pattern::NotAllowed => Ok(model::Pattern::NotAllowed),
            types::Pattern::External(e) => self.compile_external(ctx, e),
            types::Pattern::Grammar(g) => self.compile_grammar_pattern(ctx, g),
            types::Pattern::Group(g) => self.compile_pattern(ctx, g),
            types::Pattern::ListPair(a, b) => self.compile_sequence(ctx, a, b),
            types::Pattern::InterleavePair(a, b) => self.compile_interleave(ctx, a, b),
            types::Pattern::ChoicePair(a, b) => self.compile_choice(ctx, a, b),
            types::Pattern::Optional(p) => Ok(model::Pattern::Optional(Box::new(
                self.compile_pattern(ctx, p)?,
            ))),
            types::Pattern::ZeroOrMore(p) => Ok(model::Pattern::ZeroOrMore(Box::new(
                self.compile_pattern(ctx, p)?,
            ))),
            types::Pattern::OneOrMore(p) => Ok(model::Pattern::OneOrMore(Box::new(
                self.compile_pattern(ctx, p)?,
            ))),
            types::Pattern::DatatypeValue(d) => self.compile_datatype_value_pattern(ctx, d),
            types::Pattern::DatatypeName(d) => self.compile_datatype_name_pattern(ctx, d),
        }
    }
    fn compile_element(
        &mut self,
        ctx: &mut Context,
        element: &types::ElementPattern,
    ) -> Result<model::Pattern, RelaxError> {
        let name_class = self.compile_nameclass(ctx, ElemAttr::Element, &element.name_class)?;
        let mut el_ctx = ctx.new_element(ctx.convert_span(&element.span))?;
        Ok(model::Pattern::Element(
            name_class,
            Box::new(self.compile_pattern(&mut el_ctx, &element.pattern)?),
        ))
    }
    fn compile_attribute(
        &mut self,
        ctx: &mut Context,
        attribute: &types::AttributePattern,
    ) -> Result<model::Pattern, RelaxError> {
        let name_class = self.compile_nameclass(ctx, ElemAttr::Attribute, &attribute.name_class)?;
        let mut att_ctx = ctx.new_attribute(ctx.convert_span(&attribute.span));
        Ok(model::Pattern::Attribute(
            name_class,
            Box::new(self.compile_pattern(&mut att_ctx, &attribute.pattern)?),
        ))
    }
    fn compile_list(
        &mut self,
        ctx: &mut Context,
        list: &types::ListPattern,
    ) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::List(Box::new(
            self.compile_pattern(ctx, &list.0)?,
        )))
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
        if external.1.is_some() {
            unimplemented!("inherit");
        }
        let path = Path::new(ctx.file().name())
            .parent()
            .expect("TODO: no parent?")
            .join(external.0.as_string_value());
        let span = ctx.convert_span(&(external.0).0);
        let (file, s) = self
            .get_schema(&path)
            .map_err(|e| RelaxError::IncludeError(span, Box::new(e)))?;
        let file_span = file.span;
        let mut inc_ctx = ctx.new_include(span, file)?;

        match &s.pattern_or_grammar {
            types::PatternOrGrammar::Pattern(pat) => self
                .compile_pattern(&mut inc_ctx, pat)
                .map_err(|e| RelaxError::IncludeError(span, Box::new(e))),
            types::PatternOrGrammar::Grammar(types::GrammarPattern { span: _, content }) => {
                let mut child_ctx = ctx.new_grammar();
                for g in content {
                    self.compile_grammar_content_item(&mut child_ctx, g)?;
                }
                for (name, r) in child_ctx.ref_iter() {
                    if r.borrow().is_none() {
                        // TODO: retain span information for references so that we don't report bogus data
                        return Err(RelaxError::UndefinedReference {
                            span: child_ctx.convert_span(&(0..0)),
                            identifier: name,
                        });
                    }
                }
                if let Some(r) = child_ctx.get_ref("start") {
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
        if let Some(r) = child_ctx.get_ref("start") {
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
            types::GrammarContent::Define(ref d) => self.compile_define(child_ctx, d),
            types::GrammarContent::Div(ref d) => self.compile_grammar_div(child_ctx, d),
            types::GrammarContent::Include(ref i) => self.compile_include(child_ctx, i),
            types::GrammarContent::Annotation(_) => Ok(()),
        }
    }

    fn compile_sequence(
        &mut self,
        ctx: &mut Context,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Group(mut v) => i.append(&mut v),
            _ => i.push(a),
        }
        match b {
            Pattern::Group(mut v) => i.append(&mut v),
            _ => i.push(b),
        }
        Ok(model::Pattern::Group(i))
    }
    fn compile_interleave(
        &mut self,
        ctx: &mut Context,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Interleave(mut v) => i.append(&mut v),
            _ => i.push(a),
        }
        match b {
            Pattern::Interleave(mut v) => i.append(&mut v),
            _ => i.push(b),
        }
        Ok(model::Pattern::Interleave(i))
    }
    fn compile_choice(
        &mut self,
        ctx: &mut Context,
        a: &types::Pattern,
        b: &types::Pattern,
    ) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut c = vec![];
        match a {
            Pattern::Choice(mut v) => c.append(&mut v),
            _ => c.push(a),
        }
        match b {
            Pattern::Choice(mut v) => c.append(&mut v),
            _ => c.push(b),
        }
        Ok(model::Pattern::Choice(c))
    }
    fn compile_datatype_value_pattern(
        &mut self,
        ctx: &mut Context,
        datatype_value: &types::DatatypeValuePattern,
    ) -> Result<model::Pattern, RelaxError> {
        // the default datatype if none is explicitly specified is 'token'
        let name = datatype_value
            .0
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
        let datatype = self
            .datatype_compiler
            .datatype_value(ctx, &name, &datatype_value.1.as_string_value())
            .map_err(RelaxError::DatatypeError)?;
        Ok(Pattern::DatatypeValue { datatype })
    }
    fn compile_datatype_name_pattern(
        &mut self,
        ctx: &mut Context,
        datatype_name: &types::DatatypeNamePattern,
    ) -> Result<model::Pattern, RelaxError> {
        let tmp = [];
        let params = if let Some(ref params) = datatype_name.1 {
            &params[..]
        } else {
            &tmp
        };
        let name = match &datatype_name.0 {
            DatatypeName::String => DatatypeName::String,
            DatatypeName::Token => DatatypeName::Token,
            DatatypeName::CName(QName(namespace, name)) => DatatypeName::CName(QName(
                NcName(
                    0..0,
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
        let datatype = self
            .datatype_compiler
            .datatype_name(ctx, &name, params)
            .map_err(RelaxError::DatatypeError)?;
        Ok(model::Pattern::DatatypeName {
            datatype,
            except: if let Some(ref except) = datatype_name.2 {
                Some(Box::new(self.compile_pattern(ctx, except)?))
            } else {
                None
            },
        })
    }

    fn compile_declaration(
        &mut self,
        ctx: &mut Context,
        dec: &types::Decl,
    ) -> Result<(), RelaxError> {
        match dec {
            types::Decl::DefaultNamespace(types::DefaultNamespaceDeclaration { prefix, uri }) => {
                match uri {
                    NamespaceUriLiteral::Inherit => {
                        panic!(
                            "Can't inherit namespace {:?} at top level, I think?",
                            prefix
                        )
                    }
                    NamespaceUriLiteral::Uri(uri) => {
                        if let Some(ref p) = prefix {
                            ctx.declare_namespace(p.clone(), uri.as_string_value())
                        } else {
                            Ok(())
                        }
                    }
                }
            }
            types::Decl::Namespace(types::NamespaceDeclaration { prefix, uri }) => match uri {
                NamespaceUriLiteral::Inherit => {
                    panic!(
                        "Can't inherit namespace {:?} at top level, I think?",
                        prefix
                    )
                }
                NamespaceUriLiteral::Uri(uri) => {
                    ctx.declare_namespace(prefix.clone(), uri.as_string_value())
                }
            },
            types::Decl::Datatypes(types::DatatypesDeclaration { prefix, uri }) => {
                ctx.declare_datatype(prefix.clone(), uri.as_string_value())
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
        match name_class {
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
                }) => Ok(model::NameClass::named(
                    namespace_uri.as_string_value(),
                    localname.1.clone(),
                )),
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
        }
    }
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
                    other => panic!("No {:?}", other),
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let input = Path::new("test.rnc");
        let schema = match c.compile(input) {
            Err(e) => {
                c.dump_diagnostic(&e);
                std::thread::sleep(std::time::Duration::from_secs(1));
                panic!("{:?}", e);
            }
            Ok(s) => s,
        };

        let s = schema.borrow();
        let start = s.as_ref().unwrap().pattern();
        assert_matches!(start, Pattern::Ref(_whence, _name, model::PatRef(ref1)) => {
            assert_matches!(ref1.borrow().as_ref(), Some(model::DefineRule::AssignCombine(_, _, patt)) => {
                assert_matches!(patt, Pattern::Element(_name, content_patt) => {
                    assert_matches!(**content_patt, Pattern::Choice(ref seq) => {
                        assert_matches!(seq[0], Pattern::Ref(_, _, model::PatRef(ref ref2)) => {
                            assert!(Rc::ptr_eq(&ref1, &ref2));
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
                        ))
                    }
                };
                Ok(t.to_string())
            }
        }
        let mut c = Compiler::new(FS, Syntax::Compact);
        let input = Path::new("main.rnc");
        let schema = match c.compile(input) {
            Ok(s) => s,
            Err(e) => {
                c.dump_diagnostic(&e);
                panic!("{:?}", e);
            }
        };
        let s = schema.borrow();
        let start = s.as_ref().unwrap().pattern();
        assert_matches!(start, Pattern::Ref(_whence, _name, model::PatRef(ref1)) => {
            assert_matches!(ref1.borrow().as_ref(), Some(model::DefineRule::AssignCombine(_, _, patt)) => {
                assert_matches!(patt, Pattern::Element(_name, content_patt) => {
                    assert_matches!(**content_patt, Pattern::Optional(ref patt) => {
                        assert_matches!(**patt, Pattern::Ref(_, _, model::PatRef(ref ref2)) => {
                            assert!(Rc::ptr_eq(&ref1, &ref2));
                        })
                    })
                })
            })
        })
    }
}
