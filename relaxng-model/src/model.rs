use std::cell::RefCell;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;

pub type Span = Range<usize>;

#[derive(Debug)]
pub enum DefineRule {
    // the name has been defined with '=' operator, and might have also been defined with '&=' or '|='
    AssignCombine(codemap::Span, Option<CombineRule>, Pattern),
    // the name has been defined with either '&=' or '|=', but has not yet been seen with '='
    CombineOnly(codemap::Span, CombineRule, Pattern),
}
impl DefineRule {
    pub fn pattern(&self) -> &Pattern {
        match self {
            DefineRule::AssignCombine(_, _, p) | DefineRule::CombineOnly(_, _, p) => p,
        }
    }

    pub fn span(&self) -> &codemap::Span {
        match self {
            DefineRule::AssignCombine(s, _, _) | DefineRule::CombineOnly(s, _, _) => s,
        }
    }
}
#[derive(Debug)]
pub enum CombineRule {
    Choice,
    Interleave,
}

#[derive(Debug)]
pub enum Pattern {
    Choice(Vec<Pattern>),
    Interleave(Vec<Pattern>),
    /// An ordered sequence of patterns
    Group(Vec<Pattern>),
    Mixed(Box<Pattern>),
    Empty,
    Text(Option<codemap::Span>),
    NotAllowed,
    Optional(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    OneOrMore(Box<Pattern>),
    Attribute(
        NameClass,
        Box<Pattern>,
        Option<codemap::Span>,
        Option<Annotation>,
    ),
    Element(
        NameClass,
        Box<Pattern>,
        Option<codemap::Span>,
        Option<Annotation>,
    ),
    Ref(codemap::Span, String, PatRef),
    DatatypeValue {
        datatype: crate::datatype::DatatypeValues,
        span: Option<codemap::Span>,
    },
    DatatypeName {
        datatype: crate::datatype::Datatypes,
        except: Option<Box<Pattern>>,
        span: Option<codemap::Span>,
    },
    List(Box<Pattern>),
}

// Factored out from Pattern primarily to avoid infinite recursion in Debug impl
#[derive(Clone)]
pub struct PatRef(pub Rc<RefCell<Option<DefineRule>>>);
impl fmt::Debug for PatRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let mut d = f.debug_tuple("PatRef");
        if self.0.borrow().is_some() {
            d.field(&"Some(...)")
        } else {
            d.field(&"None")
        }
        .finish()
    }
}

// TODO: will users want to know the prefix which was specified in the source file, prior to
//       resolution into the uri?
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum NameClass {
    Named {
        namespace_uri: String,
        name: String,
    },
    NsName {
        namespace_uri: String,
        except: Option<Box<NameClass>>,
    },
    AnyName {
        except: Option<Box<NameClass>>,
    },
    Alt {
        a: Box<NameClass>,
        b: Box<NameClass>,
    },
}
impl NameClass {
    pub fn named(namespace_uri: String, name: String) -> NameClass {
        NameClass::Named {
            namespace_uri,
            name,
        }
    }
    pub fn ns_name(namespace_uri: String, except: Option<NameClass>) -> NameClass {
        NameClass::NsName {
            namespace_uri,
            except: except.map(Box::new),
        }
    }
    pub fn any_name(except: Option<NameClass>) -> NameClass {
        NameClass::AnyName {
            except: except.map(Box::new),
        }
    }
    pub fn alt(a: NameClass, b: NameClass) -> NameClass {
        NameClass::Alt {
            a: Box::new(a),
            b: Box::new(b),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Param {
    pub span: Span,
    pub name: String,
    pub value: String,
}

/// A fully-resolved annotation (namespace prefixes resolved to URIs)
#[derive(Debug)]
pub struct Annotation {
    pub documentation: Vec<AnnotationDocumentation>,
    pub attributes: Vec<AnnotationAttribute>,
    pub elements: Vec<AnnotationElement>,
}

#[derive(Debug)]
pub struct AnnotationDocumentation {
    pub content: String,
    pub span: Option<codemap::Span>,
}

#[derive(Debug)]
pub struct AnnotationAttribute {
    pub namespace_uri: String,
    pub local_name: String,
    pub value: String,
    pub span: Option<codemap::Span>,
}

#[derive(Debug)]
pub struct AnnotationElement {
    pub namespace_uri: String,
    pub local_name: String,
    pub attributes: Vec<AnnotationAttribute>,
    pub children: Vec<AnnotationContent>,
    pub span: Option<codemap::Span>,
}

#[derive(Debug)]
pub enum AnnotationContent {
    Element(AnnotationElement),
    Text(String),
}
