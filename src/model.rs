use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub enum DefineRule {
    // the name has been defined with '=' operator, and might have also been defined with '&=' or '|='
    AssignCombine(Option<CombineRule>, Pattern),
    // the name has been defined with either '&=' or '|=', but has not yet been seen with '='
    CombineOnly(CombineRule, Pattern),
}
#[derive(Debug)]
pub enum CombineRule {
    Choice,
    Interleave
}

#[derive(Debug)]
pub enum Pattern {
    Choice(Vec<Pattern>),
    Interleave(Vec<Pattern>),
    // AKA 'group' -- patterns that must appear in the given order
    Sequence(Vec<Pattern>),
    Mixed(Box<Pattern>),
    Empty,
    Text,
    NotAllowed,
    Optional(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    OneOrMore(Box<Pattern>),
    Attribute(NameClass, Box<Pattern>),
    Element(NameClass, Box<Pattern>),
    Ref(Rc<RefCell<Option<DefineRule>>>),
    DatatypeValue(Option<DatatypeName>, String),
    DatatypeName { name: DatatypeName, params: Vec<Param>, except: Option<Box<Pattern>> },
    List(Box<Pattern>),
}

// TODO: will users want to know the prefix which was specified in the source file, prior to
//       resolution into the uri?
#[derive(Debug)]
pub enum NameClass {
    Named {
        namespace_uri: String,
        name: String,
    },
    NsName {
        namespace_uri: String,
        except: Option<Box<NameClass>>
    },
    AnyName {
        except: Option<Box<NameClass>>
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
            except: except.map(Box::new)
        }
    }
    pub fn any_name(except: Option<NameClass>) -> NameClass {
        NameClass::AnyName {
            except: except.map(Box::new)
        }
    }
    pub fn alt(a: NameClass, b: NameClass) -> NameClass {
        NameClass::Alt {
            a: Box::new(a),
            b: Box::new(b),
        }
    }
}

#[derive(Debug)]
pub enum DatatypeName {
    String,
    Token,
    Name {
        namespace_uri: String,
        name: String,
    },
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub value: String,
}