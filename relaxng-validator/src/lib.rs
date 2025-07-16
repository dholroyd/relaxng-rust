use relaxng_model::datatype::Datatype;
use relaxng_model::model::NameClass;
use relaxng_model::{datatype, model};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::io;
use std::rc::Rc;
use xmlparser::{ElementEnd, EntityDefinition, StrSpan, Token, Tokenizer};

#[derive(Debug)]
pub enum ValidatorError<'a> {
    Xml(xmlparser::Error),
    NotAllowed(Token<'a>),
    UndefinedNamespacePrefix {
        prefix: StrSpan<'a>,
    },
    UndefinedEntity {
        name: &'a str,
        span: std::ops::Range<usize>,
    },
    InvalidOrUnclosedEntity {
        span: std::ops::Range<usize>,
    },
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct PatId(u16);

// TODO: separate representations?
//       1) includes 'Placeholder, but doesn't include nullability flags or 'After'
//       2) excludes 'Placeholder', and includes nullability flags and 'After'

// TODO: instances of Pat are currently 208 bytes, and are cloned regularly; shrink this for
//       better performance
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Pat {
    Choice(PatId, PatId, bool),
    Interleave(PatId, PatId, bool),
    Group(PatId, PatId, bool),
    OneOrMore(PatId, bool),
    Empty,
    Text,
    NotAllowed,
    Attribute(model::NameClass, PatId),
    Element(model::NameClass, PatId),
    // the Datatypes and DatatypeValues types are quite large, so we
    Datatype(datatype::Datatypes),
    DatatypeValue(datatype::DatatypeValues),
    DatatypeExcept(datatype::Datatypes, PatId),
    List(PatId),
    Placeholder(*const Option<relaxng_model::model::DefineRule>),
    After(PatId, PatId),
}

impl Pat {
    pub fn is_nullable(&self) -> bool {
        match self {
            Pat::Choice(_, _, nullable) => *nullable,
            Pat::Interleave(_, _, nullable) => *nullable,
            Pat::Group(_, _, nullable) => *nullable,
            Pat::OneOrMore(_, nullable) => *nullable,
            Pat::Empty => true,
            Pat::Text => true,
            Pat::NotAllowed => false,
            Pat::Attribute(_, _) => false,
            Pat::Element(_, _) => false,
            Pat::Datatype(_) => false,
            Pat::DatatypeValue(_) => false,
            Pat::DatatypeExcept(_, _) => false,
            Pat::List(_) => false,
            Pat::Placeholder(_name) => false, //unreachable!("Placeholder {:?}", name),
            Pat::After(_, _) => false,
        }
    }
}

#[derive(Default)]
struct Inner {
    memo: HashMap<Pat, PatId>,
    patterns: Vec<Pat>,
    refs: HashMap<*const Option<relaxng_model::model::DefineRule>, PatId>,
}
#[derive(Default)]
struct Schema {
    inner: RefCell<Inner>,
}
impl Schema {
    fn push(&self, p: Pat) -> PatId {
        let mut inner = self.inner.borrow_mut();
        if inner.patterns.len() > 0xffff {
            panic!("Only up to 2^16 rules supported in one schema")
        }
        if let Some(id) = inner.memo.get(&p) {
            *id
        } else {
            let id = PatId(inner.patterns.len() as u16);
            inner.memo.insert(p.clone(), id);
            inner.patterns.push(p);
            id
        }
    }
    pub fn choice(&self, left: PatId, right: PatId) -> PatId {
        // TODO:
        //       "In order to avoid exponential blowup with some patterns, it is essential
        //        for the choice function to eliminate redundant choices. Define the
        //        choice-leaves of a pattern to be the concatenation of the choice-leaves of
        //        its operands if the the pattern is a Choice pattern and the empty-list
        //        otherwise. Eliminating redundant choices means ensuring that the list of
        //        choice-leaves of the constructed pattern contains no duplicates. One way
        //        to do this is to for choice to walk the choice-leaves of one operand building
        //        a hash-table of the set of choice-leaves of that operand; then walk the other
        //        operand using this hash-table to eliminate any choice-leaf that has occurred
        //        in the other operand."
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => right,
            (_, Pat::NotAllowed) => left,
            (l, r) => {
                if left == right {
                    //println!("Choices are identical: left=right={:?} - {}", l, left.0);
                    return left;
                }
                if l.is_nullable() {
                    //println!("**redundant choice construction? l => nullable");
                }
                if r.is_nullable() {
                    //println!("**redundant choice construction? r => nullable");
                }
                self.push(Pat::Choice(left, right, l.is_nullable() || r.is_nullable()))
            }
        }
    }
    pub fn interleave(&self, left: PatId, right: PatId) -> PatId {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => self.not_allowed(),
            (_, Pat::NotAllowed) => self.not_allowed(),
            (Pat::Empty, _) => right,
            (_, Pat::Empty) => left,
            (l, r) => self.push(Pat::Interleave(
                left,
                right,
                l.is_nullable() && r.is_nullable(),
            )),
        }
    }
    pub fn group(&self, left: PatId, right: PatId) -> PatId {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => self.not_allowed(),
            (_, Pat::NotAllowed) => self.not_allowed(),
            (Pat::Empty, _) => right,
            (_, Pat::Empty) => left,
            (l, r) => self.push(Pat::Group(left, right, l.is_nullable() && r.is_nullable())),
        }
    }
    fn after(&self, p1: PatId, p2: PatId) -> PatId {
        match (self.patt(p1), self.patt(p1)) {
            (_, Pat::NotAllowed) => self.not_allowed(),
            (Pat::NotAllowed, _) => self.not_allowed(),
            (_, _) => self.push(Pat::After(p1, p2)),
        }
    }

    pub fn mixed(&self, pattern: PatId) -> PatId {
        self.interleave(pattern, self.text())
    }
    pub fn empty(&self) -> PatId {
        self.push(Pat::Empty)
    }
    pub fn text(&self) -> PatId {
        self.push(Pat::Text)
    }
    pub fn not_allowed(&self) -> PatId {
        self.push(Pat::NotAllowed)
    }
    pub fn one_or_more(&self, pattern: PatId) -> PatId {
        let p = self.patt(pattern);
        self.push(Pat::OneOrMore(pattern, p.is_nullable()))
    }
    fn attribute(&self, name: model::NameClass, p: PatId) -> PatId {
        self.push(Pat::Attribute(name, p))
    }
    fn element(&self, name: model::NameClass, p: PatId) -> PatId {
        self.push(Pat::Element(name, p))
    }
    fn datatype_value(&self, dt: datatype::DatatypeValues) -> PatId {
        self.push(Pat::DatatypeValue(dt))
    }
    fn datatype_name(&self, dt: datatype::Datatypes, except: Option<PatId>) -> PatId {
        if let Some(except) = except {
            self.push(Pat::DatatypeExcept(dt, except))
        } else {
            self.push(Pat::Datatype(dt))
        }
    }
    fn list(&self, p: PatId) -> PatId {
        self.push(Pat::List(p))
    }
    fn get_ref(&self, p: *const Option<relaxng_model::model::DefineRule>) -> Option<PatId> {
        let inner = self.inner.borrow_mut();
        inner.refs.get(&p).copied()
    }
    /*
    fn set_ref(&self, p: *const Option<relaxng_model::model::DefineRule>, id: PatId) {
        let mut inner = self.inner.borrow_mut();
        inner.refs.insert(p, id);
    }
    */
    fn ref_placeholder(
        &self,
        p: *const Option<relaxng_model::model::DefineRule>,
        _name: &str,
    ) -> PatId {
        let pl = Pat::Placeholder(p);
        let id = self.push(pl);
        let mut inner = self.inner.borrow_mut();
        inner.refs.insert(p, id);
        id
    }
    fn resolve_ref(&self, placeholder_id: PatId, id: PatId, name: &str) {
        if placeholder_id == id {
            // we already resolved this placeholder
            return;
        }
        let target = self.patt(id);
        if let Pat::Placeholder(_) = target {
            panic!(
                "can't resolve placeholder {} with another placeholder {}",
                placeholder_id.0, id.0
            );
        }
        let mut inner = self.inner.borrow_mut();
        match &inner.patterns[placeholder_id.0 as usize] {
            Pat::Placeholder(_) => (),
            p => panic!("expected placeholder but got {:?}, with id {} while trying to resolve it to {}, for definition {:?}", p, placeholder_id.0, id.0, name)
        }
        inner.patterns[placeholder_id.0 as usize] = target;
    }
    fn patt(&self, id: PatId) -> Pat {
        self.inner.borrow().patterns[id.0 as usize].clone()
    }

    fn check_choice(
        &self,
        id: PatId,
        seen: &mut Vec<PatId>,
        seen_choices: &mut Vec<PatId>,
    ) -> bool {
        if seen_choices.contains(&id) {
            println!(
                "Choice contains duplicate pattern {:?} {:?}",
                id,
                self.patt(id)
            );
        }
        seen.push(id);
        match self.patt(id) {
            Pat::Choice(l, r, _) => {
                self.check_choice(l, seen, seen_choices);
                self.check_choice(r, seen, seen_choices);
            }
            Pat::Interleave(l, r, _) | Pat::Group(l, r, _) => {
                self.check_choices(l, seen);
                self.check_choices(r, seen);
            }
            Pat::Empty => {}
            Pat::Text => {}
            Pat::NotAllowed => {}
            Pat::OneOrMore(p, _) | Pat::Attribute(_, p) | Pat::Element(_, p) | Pat::List(p) => {
                self.check_choices(p, seen)
            }
            Pat::Datatype(_) => {}
            Pat::DatatypeValue(_) => {}
            Pat::DatatypeExcept(_, _) => {}
            Pat::Placeholder(_) | Pat::After(_, _) => unreachable!(),
        }
        false
    }
    fn check_choices(&self, id: PatId, seen: &mut Vec<PatId>) {
        if seen.contains(&id) {
            return;
        }
        seen.push(id);
        match self.patt(id) {
            Pat::Choice(_, _, _) => {
                let mut seen_choices = vec![];
                self.check_choice(id, seen, &mut seen_choices);
            }
            Pat::Interleave(l, r, _) | Pat::Group(l, r, _) => {
                self.check_choices(l, seen);
                self.check_choices(r, seen);
            }
            Pat::OneOrMore(p, _) | Pat::Attribute(_, p) | Pat::Element(_, p) | Pat::List(p) => {
                self.check_choices(p, seen)
            }
            Pat::Empty | Pat::Text | Pat::NotAllowed | Pat::Datatype(_) | Pat::DatatypeValue(_) => {
            }
            Pat::DatatypeExcept(_, p) => self.check_choices(p, seen),
            Pat::Placeholder(_) | Pat::After(_, _) => unreachable!(),
        }
    }

    #[allow(unused)]
    pub fn d(&self, pat: PatId) -> Result<(), io::Error> {
        let mut o = io::stdout();
        self.dumpy(pat, &mut o)
    }

    pub fn dumpy<W: io::Write>(&self, pat: PatId, w: &mut W) -> Result<(), io::Error> {
        let mut seen = HashSet::new();
        self.dumpy_dump(0, pat, w, &mut seen)
    }
    pub fn dumpy_dump<W: io::Write>(
        &self,
        depth: usize,
        pat: PatId,
        w: &mut W,
        seen: &mut HashSet<PatId>,
    ) -> Result<(), io::Error> {
        for _ in 0..depth {
            w.write_all(b"  ")?;
        }
        if seen.insert(pat) {
            match self.patt(pat) {
                Pat::Choice(p1, p2, _) => {
                    writeln!(w, "Choice{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p1, w, seen)?;
                    self.dumpy_dump(depth + 1, p2, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Interleave(p1, p2, _) => {
                    writeln!(w, "Interleave{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p1, w, seen)?;
                    self.dumpy_dump(depth + 1, p2, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Group(p1, p2, _) => {
                    writeln!(w, "Group{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p1, w, seen)?;
                    self.dumpy_dump(depth + 1, p2, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::OneOrMore(p, _) => {
                    writeln!(w, "OneOrMany{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Empty => {
                    writeln!(w, "Empty{}", pat.0)
                }
                Pat::Text => {
                    writeln!(w, "Text{}", pat.0)
                }
                Pat::NotAllowed => {
                    writeln!(w, "NotAllowed{}", pat.0)
                }
                Pat::Attribute(nc, p) => {
                    writeln!(w, "Att{}({:?}=", pat.0, nc)?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Element(nc, p) => {
                    writeln!(w, "Elem{}({:?}=", pat.0, nc)?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Datatype(dt) => {
                    writeln!(w, "{:?}{}", dt, pat.0)
                }
                Pat::DatatypeValue(dt) => {
                    writeln!(w, "{:?}{}", dt, pat.0)
                }
                Pat::DatatypeExcept(dt, p) => {
                    writeln!(w, "{:?}{}-", dt, pat.0)?;
                    self.dumpy_dump(depth + 1, p, w, seen)
                }
                Pat::List(p) => {
                    writeln!(w, "List{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Placeholder(_) => {
                    writeln!(w, "Placeholder{}", pat.0)
                }
                Pat::After(p1, p2) => {
                    writeln!(w, "After{}(", pat.0)?;
                    self.dumpy_dump(depth + 1, p1, w, seen)?;
                    self.dumpy_dump(depth + 1, p2, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
            }
        } else {
            match self.patt(pat) {
                Pat::Choice(_p1, _p2, _) => {
                    writeln!(w, "Choice{}!", pat.0)
                }
                Pat::Interleave(_p1, _p2, _) => {
                    writeln!(w, "Interleave{}!", pat.0)
                }
                Pat::Group(_p1, _p2, _) => {
                    writeln!(w, "Group{}!", pat.0)
                }
                Pat::OneOrMore(_p, _) => {
                    writeln!(w, "OneOrMany{}!", pat.0)
                }
                Pat::Empty => {
                    writeln!(w, "Empty{}!", pat.0)
                }
                Pat::Text => {
                    writeln!(w, "Text{}!", pat.0)
                }
                Pat::NotAllowed => {
                    writeln!(w, "NotAllowed{}!", pat.0)
                }
                Pat::Attribute(_nc, _p) => {
                    writeln!(w, "Att{}!", pat.0)
                }
                Pat::Element(_nc, _p) => {
                    writeln!(w, "Elem{}!", pat.0)
                }
                Pat::Datatype(dt) => {
                    writeln!(w, "{:?}{}!", dt, pat.0)
                }
                Pat::DatatypeValue(dt) => {
                    writeln!(w, "{:?}{}!", dt, pat.0)
                }
                Pat::DatatypeExcept(dt, _p) => {
                    writeln!(w, "{:?}{}!", dt, pat.0)
                }
                Pat::List(_p) => {
                    writeln!(w, "List{}!", pat.0)
                }
                Pat::Placeholder(_) => unreachable!(),
                Pat::After(_p1, _p2) => {
                    writeln!(w, "After{}!", pat.0)
                }
            }
        }
    }
}

fn is_ns_match(namespace_uri: &str, target_namespace: Option<&StrSpan>) -> bool {
    if let Some(target_namespace) = target_namespace {
        target_namespace.as_str() == namespace_uri
    } else {
        namespace_uri.is_empty()
    }
}
fn contains(nc: &model::NameClass, target_name: QualifiedName) -> bool {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => {
            if let Some(ref target_namespace) = target_name.namespace_uri {
                target_namespace.as_str() == namespace_uri
                    && target_name.local_name.as_str() == name
            } else {
                namespace_uri.is_empty() && target_name.local_name.as_str() == name
            }
        }
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            if is_ns_match(namespace_uri, target_name.namespace_uri.as_ref()) {
                if let Some(except_nameclass) = except {
                    !contains(except_nameclass, target_name)
                } else {
                    true
                }
            } else {
                false
            }
        }
        NameClass::AnyName { except } => match except {
            None => true,
            Some(nc) => !contains(nc, target_name),
        },
        NameClass::Alt { a, b } => contains(a, target_name) || contains(b, target_name),
    }
}

#[derive(Copy, Clone, Debug)]
struct QualifiedName<'a> {
    namespace_uri: Option<StrSpan<'a>>,
    local_name: StrSpan<'a>,
}

#[derive(Copy, Clone, Debug)]
struct Attr<'a> {
    name: QualifiedName<'a>,
    value: StrSpan<'a>,
    span: StrSpan<'a>,
}

pub struct Validator<'a> {
    schema: Schema,
    tokenizer: Tokenizer<'a>,
    current_step: PatId,
    last_was_start_element: bool,
    stack: ElementStack<'a>,
    entity_definitions: HashMap<String, String>,
}

impl<'a> Validator<'a> {
    pub fn new(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        tokenizer: Tokenizer<'a>,
    ) -> Validator<'a> {
        let schema = Schema::default();
        let start = Self::compile(
            &schema,
            Rc::as_ref(&model).borrow().as_ref().unwrap().pattern(),
        );
        let mut entity_definitions = HashMap::default();
        entity_definitions.insert("lt".to_string(), "<".to_string());
        entity_definitions.insert("gt".to_string(), ">".to_string());
        entity_definitions.insert("amp".to_string(), "&".to_string());
        entity_definitions.insert("apos".to_string(), "'".to_string());
        entity_definitions.insert("quot".to_string(), "\"".to_string());
        Validator {
            schema,
            tokenizer,
            current_step: start,
            last_was_start_element: false,
            stack: ElementStack::default(),
            entity_definitions,
        }
    }
    fn compile(s: &Schema, p: &model::Pattern) -> PatId {
        match p {
            model::Pattern::Choice(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    right = s.choice(Self::compile(s, left), right)
                }
                right
            }
            model::Pattern::Interleave(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    right = s.interleave(Self::compile(s, left), right)
                }
                right
            }
            model::Pattern::Group(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    right = s.group(Self::compile(s, left), right)
                }
                right
            }
            model::Pattern::Mixed(p) => s.mixed(Self::compile(s, p)),
            model::Pattern::Empty => s.empty(),
            model::Pattern::Text => s.text(),
            model::Pattern::NotAllowed => s.not_allowed(),
            model::Pattern::Optional(p) => s.choice(Self::compile(s, p), s.empty()),
            model::Pattern::ZeroOrMore(p) => {
                s.choice(s.one_or_more(Self::compile(s, p)), s.empty())
            }
            model::Pattern::OneOrMore(p) => s.one_or_more(Self::compile(s, p)),
            model::Pattern::Attribute(name, p) => s.attribute(name.clone(), Self::compile(s, p)),
            model::Pattern::Element(name, p) => s.element(name.clone(), Self::compile(s, p)),
            model::Pattern::Ref(whence, name, r) => {
                let ptr = r.0.as_ptr();
                if let Some(id) = s.get_ref(ptr) {
                    id
                } else {
                    let placeholder_id = s.ref_placeholder(ptr, name);
                    if let Some(thing) = Rc::as_ref(&r.0).borrow().as_ref() {
                        let id = Self::compile(s, thing.pattern());
                        s.resolve_ref(placeholder_id, id, name);
                        placeholder_id
                    } else {
                        panic!(
                            "Somehow definition for {name:#?} is missing, used in {whence:?}"
                        )
                    }
                }
            }
            model::Pattern::DatatypeValue { datatype } => s.datatype_value(datatype.clone()),
            model::Pattern::DatatypeName { datatype, except } => s.datatype_name(
                datatype.clone(),
                except.as_ref().map(|e| Self::compile(s, e)),
            ),
            model::Pattern::List(p) => s.list(Self::compile(s, p)),
        }
    }

    pub fn validate_next(&mut self) -> Option<Result<(), ValidatorError<'a>>> {
        match self.tokenizer.next() {
            Some(Ok(evt)) => Some(self.validate(evt)),
            Some(Err(err)) => Some(Err(ValidatorError::Xml(err))),
            None => None,
        }
    }

    #[allow(unused)]
    fn assert_health(&self) {
        let mut fail = false;
        for v in self.schema.inner.borrow().refs.values() {
            if let Pat::Placeholder(_p) = self.schema.patt(*v) {
                println!("Still a placeholder: {v:?}");
                fail = true;
            }
        }
        if fail {
            panic!();
        }

        let mut seen = vec![];
        self.schema.check_choices(self.current_step, &mut seen);
    }

    fn validate(&mut self, evt: Token<'a>) -> Result<(), ValidatorError<'a>> {
        let pat = self.schema.patt(self.current_step);
        let new = match evt {
            Token::EmptyDtd { .. }
            | Token::Comment { .. }
            | Token::ProcessingInstruction { .. } => {
                // does not change current_step state
                return Ok(());
            }
            Token::ElementStart {
                prefix,
                local,
                span,
            } => {
                self.stack.push(prefix, local, span);
                // does not change current_step state
                return Ok(());
            }
            /*
                let next_pat = Self::start_tag_open_deriv(pat, &mut self.schema, namespace, &name);
                // TODO: refactor early-returns
                let next_pat = match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => Self::attrs_deriv(next_pat, &mut self.schema, attributes)
                };
                let next_pat = match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => Self::start_tag_close_deriv(next_pat, &mut self.schema)
                };
                match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => next_pat //Self::children_deriv(next_pat, &mut self.schema)
                }
            */
            Token::Attribute {
                prefix,
                local,
                value,
                span,
            } => {
                self.stack.add_attr(prefix, local, value, span);
                // does not change current_step state
                return Ok(());
            }
            Token::ElementEnd { end, span: _ } => {
                match end {
                    ElementEnd::Open => {
                        Self::close_element_start(&self.stack, &mut self.schema, evt, pat)?
                    }
                    ElementEnd::Close(_, _) => {
                        let next_pat = if self.last_was_start_element {
                            // The last event was XmlEvent::StartElement with no child elements or child
                            // text nodes.
                            //
                            // Per https://relaxng.org/jclark/derivative.html ,
                            //     "The case where the list of children is empty is
                            //      treated as if there were a text node whose value
                            //      were the empty string."
                            //
                            // This fake text node is required for a pattern like 'element foo { token }'
                            // to match the input '<foo/>' or '<foo></foo>'
                            let p = Self::text_deriv(pat, &mut self.schema, "");
                            self.schema.patt(p)
                        } else {
                            pat
                        };
                        Self::end_tag_deriv(next_pat, &mut self.schema)
                    }
                    ElementEnd::Empty => {
                        let next_id =
                            Self::close_element_start(&self.stack, &mut self.schema, evt, pat)?;
                        let pat = self.schema.patt(next_id);
                        let next_pat = if self.last_was_start_element {
                            // The last event was XmlEvent::StartElement with no child elements or child
                            // text nodes.
                            //
                            // Per https://relaxng.org/jclark/derivative.html ,
                            //     "The case where the list of children is empty is
                            //      treated as if there were a text node whose value
                            //      were the empty string."
                            //
                            // This fake text node is required for a pattern like 'element foo { token }'
                            // to match the input '<foo/>' or '<foo></foo>'
                            let p = Self::text_deriv(pat, &mut self.schema, "");
                            self.schema.patt(p)
                        } else {
                            pat
                        };
                        Self::end_tag_deriv(next_pat, &mut self.schema)
                    }
                }
            }
            Token::Cdata { text, span: _ } => Self::text_deriv(pat, &mut self.schema, &text),
            Token::Text { text } => {
                let mut buffer = String::new();
                for val in parse_entities(text.start(), text.as_str()) {
                    match val {
                        Ok(val) => {
                            let txt = match val {
                                Txt::Text(_pos, val) => val,
                                Txt::Entity(pos, name) => {
                                    if let Some(txt) = self.entity_definitions.get(name) {
                                        txt
                                    } else {
                                        return Err(ValidatorError::UndefinedEntity {
                                            name,
                                            span: pos..pos + name.len(),
                                        });
                                    }
                                }
                                Txt::Char(_pos, val) => {
                                    buffer.push(val);
                                    continue;
                                }
                            };
                            // we only reach this point for Txt::Text and Txt::Entity cases,
                            if txt.len() == text.len() {
                                // no need to copy data into the buffer, just process the whole input in one go
                                break;
                            } else {
                                // the input contains entities, so we decode these and append to buffer
                                buffer.push_str(txt);
                            }
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }
                }
                let data = if buffer.is_empty() {
                    text.as_str()
                } else {
                    &buffer[..]
                };
                let next_id = Self::text_deriv(pat, &mut self.schema, data);
                let next_pat = self.schema.patt(next_id);
                if let Pat::NotAllowed = next_pat {
                    return Err(ValidatorError::NotAllowed(Token::Text { text }));
                }
                next_id
            }
            Token::EntityDeclaration {
                name,
                definition,
                span: _,
            } => {
                match definition {
                    EntityDefinition::EntityValue(val) => {
                        self.entity_definitions
                            .insert(name.to_string(), val.to_string());
                        // does not change current_step state
                        return Ok(());
                    }
                    EntityDefinition::ExternalId(_) => {
                        // no support for resolving external ids
                        // does not change current_step state
                        return Err(ValidatorError::NotAllowed(evt));
                    }
                }
            }
            Token::Declaration { .. } | Token::DtdStart { .. } | Token::DtdEnd { .. } => {
                // does not change current_step state
                return Ok(());
            }
        };
        if let Token::ElementStart { .. } = evt {
            self.last_was_start_element = true;
        } else {
            self.last_was_start_element = false;
        }
        if let Pat::NotAllowed = self.schema.patt(new) {
            Err(ValidatorError::NotAllowed(evt))
        } else {
            self.current_step = new;
            Ok(())
        }
    }

    fn close_element_start<'b: 'a>(
        stack: &ElementStack<'b>,
        schema: &mut Schema,
        evt: Token<'b>,
        pat: Pat,
    ) -> Result<PatId, ValidatorError<'b>> {
        let name = stack.current_element()?;
        let next_pat = Self::start_tag_open_deriv(pat, schema, name);
        // TODO: refactor early-returns
        let next_pat = match schema.patt(next_pat) {
            Pat::NotAllowed => {
                return Err(ValidatorError::NotAllowed(Token::ElementStart {
                    prefix: name.namespace_uri.unwrap_or_else(|| StrSpan::from("")),
                    local: name.local_name,
                    span: name.local_name,
                }))
            }
            _p => {
                let attributes: Vec<_> = stack.current_attributes()?;
                let mut pat = next_pat;
                for att in attributes {
                    let mut memo = HashMap::new();
                    pat = Self::att_deriv(&mut memo, pat, schema, att);
                    if let Pat::NotAllowed = schema.patt(pat) {
                        return Err(ValidatorError::NotAllowed(Token::Attribute {
                            prefix: att.name.namespace_uri.unwrap_or_else(|| StrSpan::from("")),
                            local: att.name.local_name,
                            value: att.value,
                            span: att.span,
                        }));
                    }
                }
                pat
            }
        };
        let next_pat = match schema.patt(next_pat) {
            Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
            _p => Self::start_tag_close_deriv(next_pat, schema),
        };
        Ok(match schema.patt(next_pat) {
            Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
            _p => next_pat, //Self::children_deriv(next_pat, &mut self.schema)
        })
    }

    fn text_deriv(current: Pat, schema: &mut Schema, text: &str) -> PatId {
        // TODO: do we need to memoize here per att_deriv() ?
        match current {
            Pat::Choice(p1, p2, _) => {
                let p1 = schema.patt(p1);
                let p2 = schema.patt(p2);
                let a = Self::text_deriv(p1, schema, text);
                let b = Self::text_deriv(p2, schema, text);
                schema.choice(a, b)
            }
            Pat::Interleave(p1, p2, _) => {
                let pat1 = schema.patt(p1);
                let pat2 = schema.patt(p2);

                let d1 = Self::text_deriv(pat1, schema, text);
                let a = schema.interleave(d1, p2);

                let d2 = Self::text_deriv(pat2, schema, text);
                let b = schema.interleave(p1, d2);
                schema.choice(a, b)
            }
            Pat::Group(p1, p2, _) => {
                let pat1 = schema.patt(p1);
                let nullable = pat1.is_nullable();
                let pat2 = schema.patt(p2);
                let d1 = Self::text_deriv(pat1, schema, text);
                let p = schema.group(d1, p2);
                if nullable {
                    let d2 = Self::text_deriv(pat2, schema, text);
                    schema.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::After(p1, p2) => {
                let pat1 = schema.patt(p1);
                let d = Self::text_deriv(pat1, schema, text);
                schema.after(d, p2)
            }
            Pat::OneOrMore(p, _) => {
                let pat = schema.patt(p);
                let d = Self::text_deriv(pat, schema, text);
                schema.group(d, schema.choice(schema.one_or_more(p), schema.empty()))
            }
            Pat::Text => schema.text(),
            Pat::Datatype(dt) => {
                if dt.is_valid(text) {
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::DatatypeValue(dt) => {
                if dt.is_valid(text) {
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::DatatypeExcept(dt, except) => {
                let pat = schema.patt(except);
                let d = Self::text_deriv(pat, schema, text);
                let pat2 = schema.patt(d);
                if dt.is_valid(text) && !pat2.is_nullable() {
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::List(p) => {
                let mut p = p;
                for item in text.split_whitespace() {
                    let pat = schema.patt(p);
                    p = Self::text_deriv(pat, schema, item);
                    if let Pat::NotAllowed = schema.patt(p) {
                        return p;
                    }
                }
                let last_patt = schema.patt(p);
                if let Pat::Empty = last_patt {
                    p
                } else if last_patt.is_nullable() {
                    // List is not able to be nullable per https://relaxng.org/jclark/derivative.html
                    // but that definition assumes that we can see all text content up-front
                    // whereas processing instructions CDATA sections etc may mean we see
                    // text children piecemeal here.  To accommodate this, we make the list
                    // optional here (TODO: should we rather adjust List to be nullable?)
                    schema.choice(schema.list(p), schema.empty())
                } else {
                    schema.list(p)
                }
            }
            Pat::Empty => {
                // from 'An algorithm for RELAX NG validation':
                //   "In the case where the list of children consists of a single text node and the
                //    value of the text node consists only of whitespace, the list of children
                //    matches if the list matches either with or without stripping the text node."
                //   "Otherwise, there must be one or more elements amongst the children, in which
                //    case any whitespace-only text nodes are stripped before the derivative is
                //    computed."
                // The document assumes however that we can see the list of child nodes, so we need
                // to handle this case in a streaming manner.  Right now we don't track that
                // that this pattern is being tested in the context of child nodes - FIXME!
                if xml::common::is_whitespace_str(text) {
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::NotAllowed | Pat::Attribute(_, _) => schema.not_allowed(),
            Pat::Element(_, _) => {
                if xml::common::is_whitespace_str(text) {
                    schema.push(current) // TODO: just have the PatId to hand
                } else {
                    schema.not_allowed()
                }
            }
            Pat::Placeholder(name) => unreachable!("Placeholder {:?}", name),
        }
    }

    fn start_tag_open_deriv(current: Pat, schema: &mut Schema, name: QualifiedName<'a>) -> PatId {
        match current {
            Pat::Choice(l, r, _) => {
                let left = schema.patt(l);
                let right = schema.patt(r);
                let d1 = Self::start_tag_open_deriv(left, schema, name);
                let d2 = Self::start_tag_open_deriv(right, schema, name);
                schema.choice(d1, d2)
            }
            Pat::OneOrMore(pid, _) => {
                let p = schema.patt(pid);
                let deriv = Self::start_tag_open_deriv(p, schema, name);
                Self::apply_after(schema.patt(deriv), schema, |pat, schema| {
                    schema.group(pat, schema.choice(schema.one_or_more(pid), schema.empty()))
                })
            }
            Pat::Interleave(pid1, pid2, _) => {
                let p1 = schema.patt(pid1);
                let p2 = schema.patt(pid2);
                let d1 = Self::start_tag_open_deriv(p1, schema, name);
                let c1 = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.interleave(pat, pid2)
                });
                let d2 = Self::start_tag_open_deriv(p2, schema, name);
                let c2 = Self::apply_after(schema.patt(d2), schema, |pat, schema| {
                    schema.interleave(pid1, pat)
                });
                schema.choice(c1, c2)
            }
            Pat::Group(pid1, pid2, _) => {
                let p1 = schema.patt(pid1);
                let nullable = p1.is_nullable();
                let _p2 = schema.patt(pid2);
                let d1 = Self::start_tag_open_deriv(p1, schema, name);
                let x = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.group(pat, pid2)
                });
                if nullable {
                    let p2 = schema.patt(pid2);
                    let d2 = Self::start_tag_open_deriv(p2, schema, name);
                    schema.choice(x, d2)
                } else {
                    x
                }
            }
            Pat::Element(ref nc, pat) => {
                if contains(nc, name) {
                    let empty = schema.empty();
                    schema.after(pat, empty)
                } else {
                    schema.not_allowed()
                }
            }
            Pat::After(pid1, pid2) => {
                let p1 = schema.patt(pid1);
                let d = Self::start_tag_open_deriv(p1, schema, name);
                Self::apply_after(schema.patt(d), schema, |pat, schema| {
                    schema.after(pat, pid2)
                })
            }

            Pat::Empty
            | Pat::Text
            | Pat::NotAllowed
            | Pat::Attribute(_, _)
            | Pat::Datatype(_)
            | Pat::DatatypeValue(_)
            | Pat::DatatypeExcept(_, _)
            | Pat::List(_) => schema.not_allowed(),
            Pat::Placeholder(name) => unreachable!("Placeholder {:?}", name),
        }
    }

    // in the spec, the applyAfter() 'f' argument comes before the pattern, in rust it's more
    // convenient if the 'f' argument is last in the list
    fn apply_after<F>(pat: Pat, schema: &mut Schema, f: F) -> PatId
    where
        F: Fn(PatId, &mut Schema) -> PatId + Clone,
    {
        match pat {
            Pat::After(p1, p2) => {
                let p2 = f(p2, schema);
                schema.after(p1, p2)
            },
            Pat::Choice(p1, p2, _) => {
                let p1 = schema.patt(p1);
                let p2 = schema.patt(p2);
                let c1 = Self::apply_after(p1, schema, f.clone());
                let c2 = Self::apply_after(p2, schema, f);
                schema.choice(c1, c2)
            },
            Pat::NotAllowed => schema.not_allowed(),
            _ => panic!("Only 'Choice', 'Interleave' or 'NotAllowed' patterns may be passed to apply_after(): {pat:?}")
        }
    }

    fn att_deriv(
        memo: &mut HashMap<PatId, PatId>,
        pat: PatId,
        schema: &mut Schema,
        att: Attr,
    ) -> PatId {
        if let Some(result) = memo.get(&pat) {
            return *result;
        }
        //let mut o = io::stdout();
        //println!("Lets see,");
        //schema.dumpy(pat, &mut o).unwrap();
        let v = match schema.patt(pat) {
            Pat::After(p1, p2) => {
                let d = Self::att_deriv(memo, p1, schema, att);
                schema.after(d, p2)
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = Self::att_deriv(memo, p1, schema, att);
                let c2 = Self::att_deriv(memo, p2, schema, att);
                schema.choice(c1, c2)
            }
            Pat::Group(p1, p2, _) => {
                let d1 = Self::att_deriv(memo, p1, schema, att);
                let s1 = schema.group(d1, p2);
                let d2 = Self::att_deriv(memo, p2, schema, att);
                let s2 = schema.group(p1, d2);
                schema.choice(s1, s2)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = Self::att_deriv(memo, p1, schema, att);
                let i1 = schema.interleave(d1, p2);
                let d2 = Self::att_deriv(memo, p2, schema, att);
                let i2 = schema.interleave(p1, d2);
                schema.choice(i1, i2)
            }
            Pat::OneOrMore(p, _) => {
                let s1 = Self::att_deriv(memo, p, schema, att);
                let s2 = schema.choice(pat, schema.empty());
                schema.group(s1, s2)
            }
            Pat::Attribute(ref nc, p) => {
                let att_pat = schema.patt(p);
                if contains(nc, att.name) && Self::value_match(att_pat, schema, &att.value) {
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            _ => schema.not_allowed(),
        };
        memo.insert(pat, v);
        v
    }

    fn value_match(pat: Pat, schema: &mut Schema, val: &str) -> bool {
        if pat.is_nullable() && is_whitespace_str(val) {
            true
        } else {
            let d = Self::text_deriv(pat, schema, val);
            schema.patt(d).is_nullable()
        }
    }

    fn start_tag_close_deriv(pid: PatId, schema: &mut Schema) -> PatId {
        let pat = schema.patt(pid);
        match pat {
            Pat::After(p1, p2) => {
                let a1 = Self::start_tag_close_deriv(p1, schema);
                schema.after(a1, p2)
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = Self::start_tag_close_deriv(p1, schema);
                let c2 = Self::start_tag_close_deriv(p2, schema);
                schema.choice(c1, c2)
            }
            Pat::Group(p1, p2, _) => {
                let c1 = Self::start_tag_close_deriv(p1, schema);
                let c2 = Self::start_tag_close_deriv(p2, schema);
                schema.group(c1, c2)
            }
            Pat::Interleave(p1, p2, _) => {
                let c1 = Self::start_tag_close_deriv(p1, schema);
                let c2 = Self::start_tag_close_deriv(p2, schema);
                schema.interleave(c1, c2)
            }
            Pat::OneOrMore(p, _) => {
                let _o = Self::start_tag_close_deriv(p, schema);
                schema.one_or_more(p)
            }
            Pat::Attribute(_, _) => schema.not_allowed(),
            _ => pid,
        }
    }

    fn end_tag_deriv(pat: Pat, schema: &mut Schema) -> PatId {
        match pat {
            Pat::Choice(p1, p2, _) => {
                let p1 = schema.patt(p1);
                let p2 = schema.patt(p2);
                let c1 = Self::end_tag_deriv(p1, schema);
                let c2 = Self::end_tag_deriv(p2, schema);
                schema.choice(c1, c2)
            }
            Pat::After(p1, p2) => {
                let p1 = schema.patt(p1);
                if p1.is_nullable() {
                    p2
                } else {
                    schema.not_allowed()
                }
            }
            _ => schema.not_allowed(),
        }
    }

    #[allow(clippy::mutable_key_type)] // false-positive
    fn heads(&self, id: PatId) -> HashSet<Pat> {
        let mut result = HashSet::new();
        self.head(&mut result, id);
        result
    }
    #[allow(clippy::mutable_key_type)] // false-positive
    fn head(&self, result: &mut HashSet<Pat>, p: PatId) {
        // https://www.kohsuke.org/relaxng/implbook/Validation1.html#IDATGOO
        let pat = self.schema.patt(p);
        match pat {
            Pat::Choice(p1, p2, _) => {
                self.head(result, p1);
                self.head(result, p2);
            }
            Pat::Interleave(p1, p2, _) => {
                self.head(result, p1);
                self.head(result, p2);
            }
            Pat::Group(p1, p2, _) => {
                if self.schema.patt(p1).is_nullable() {
                    self.head(result, p1);
                } else {
                    self.head(result, p1);
                    self.head(result, p2);
                }
            }
            Pat::OneOrMore(p, _) => self.head(result, p),
            Pat::Empty => {}
            Pat::Text => {}
            Pat::NotAllowed => {}
            Pat::Attribute(_, _) => {
                result.insert(pat);
            }
            Pat::Element(_, _) => {
                result.insert(pat);
            }
            Pat::Datatype(_) => {
                result.insert(pat);
            }
            Pat::DatatypeValue(_) => {
                result.insert(pat);
            }
            Pat::DatatypeExcept(_, _) => {
                result.insert(pat);
            }
            Pat::List(p) => self.head(result, p),
            Pat::Placeholder(_) => panic!("Unexpected {pat:?}"),
            Pat::After(p, _) => self.head(result, p),
        }
    }

    fn describe_expected(&self, expected_patt: PatId) -> String {
        #[allow(clippy::mutable_key_type)] // false-positive
        let heads = self.heads(expected_patt);
        let mut result = String::new();
        const MAX_ELEMENTS: usize = 4;
        let mut rest = 0;
        for (i, nameclass) in heads
            .iter()
            .filter_map(|p| {
                if let Pat::Element(nameclass, _) = p {
                    Some(nameclass)
                } else {
                    None
                }
            })
            .enumerate()
        {
            if i == 0 {
                result.push_str("Element ");
            }
            if i >= MAX_ELEMENTS {
                rest += 1;
            } else {
                if i > 0 {
                    result.push(' ');
                }
                // TODO: also provide namespace information; grouping by namespace to make the
                //       information more succinct
                let mut desc = String::new();
                self.describe_nameclass(nameclass, &mut desc);
                result.push_str(&desc);
            }
        }
        if rest > 0 {
            result.push_str(&format!(" .. or one of {rest} more"))
        }
        // TODO: plus attributes and everything else
        result
    }
    #[allow(clippy::only_used_in_recursion)]
    fn describe_nameclass(&self, nc: &NameClass, desc: &mut String) {
        match nc {
            NameClass::Named {
                namespace_uri: _,
                name,
            } => {
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
                    self.describe_nameclass(except, desc);
                }
            }
            NameClass::AnyName { except } => {
                desc.push('*');
                if let Some(except) = except {
                    desc.push('-');
                    self.describe_nameclass(except, desc);
                }
            }
            NameClass::Alt { a, b } => {
                self.describe_nameclass(a, desc);
                desc.push('|');
                self.describe_nameclass(b, desc);
            }
        }
    }

    pub fn diagnostic(
        &self,
        name: String,
        source: String,
        err: &ValidatorError,
    ) -> (codemap::CodeMap, Vec<codemap_diagnostic::Diagnostic>) {
        let mut map = codemap::CodeMap::new();
        let file = map.add_file(name, source);
        let mut diagnostics = vec![];
        match err {
            ValidatorError::Xml(err) => {
                let line = file.line_span(err.pos().row as _);
                let span = line.subspan(err.pos().row as _, err.pos().row as _);

                let label = codemap_diagnostic::SpanLabel {
                    span,
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{err}"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::NotAllowed(tok) => {
                let span = match tok {
                    Token::Declaration { span, .. }
                    | Token::ProcessingInstruction { span, .. }
                    | Token::Comment { span, .. }
                    | Token::DtdStart { span, .. }
                    | Token::EmptyDtd { span, .. }
                    | Token::EntityDeclaration { span, .. }
                    | Token::DtdEnd { span, .. }
                    | Token::ElementStart { span, .. }
                    | Token::Attribute { span, .. }
                    | Token::ElementEnd { span, .. }
                    | Token::Cdata { span, .. } => span,
                    Token::Text { text } => text,
                };
                let name = match tok {
                    Token::Declaration { .. } => "declaration",
                    Token::ProcessingInstruction { .. } => "processing-instruction",
                    Token::Comment { .. } => "comment",
                    Token::DtdStart { .. } => "dtd-start",
                    Token::EmptyDtd { .. } => "empty-dtd",
                    Token::EntityDeclaration { .. } => "entity-declaration",
                    Token::DtdEnd { .. } => "dtd-end",
                    Token::ElementStart { .. } => "element-start",
                    Token::Attribute { .. } => "attribute",
                    Token::ElementEnd { end: _, .. } => "element-end",
                    Token::Text { .. } => "text",
                    Token::Cdata { .. } => "cdata",
                };
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start() as _, span.end() as _),
                    label: Some("Not allowed".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{name} not expected here"),
                    code: None,
                    spans: vec![label],
                });

                let desc = self.describe_expected(self.current_step);
                let message = if desc.is_empty() {
                    "Remove this".to_string()
                } else {
                    format!("Expected {desc}")
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Help,
                    message,
                    code: None,
                    spans: vec![],
                });
            }
            ValidatorError::UndefinedNamespacePrefix { prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(prefix.start() as _, prefix.end() as _),
                    label: Some(format!(
                        "Add an xmlns:{}=\"..\" attribute to define this prefix",
                        prefix.as_str()
                    )),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The prefix {:?} is not defined", prefix.as_str()),
                    code: None,
                    spans: vec![label],
                })
            }
            ValidatorError::UndefinedEntity { name, span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("undefined".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The entity &{name:?}; is not defined"),
                    code: None,
                    spans: vec![label],
                })
            }
            ValidatorError::InvalidOrUnclosedEntity { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Invalid or unclosed entity reference".to_string(),
                    code: None,
                    spans: vec![label],
                })
            }
        }
        (map, diagnostics)
    }
}

#[derive(Debug)]
enum Txt<'a> {
    Text(usize, &'a str),
    Entity(usize, &'a str),
    Char(usize, char),
}

fn parse_entities(pos: usize, text: &str) -> impl Iterator<Item = Result<Txt, ValidatorError>> {
    struct Entities<'a> {
        text: &'a str,
        pos: usize,
        offset: usize,
        in_entity: bool,
    }
    impl<'a> Iterator for Entities<'a> {
        type Item = Result<Txt<'a>, ValidatorError<'a>>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.offset == self.text.len() {
                return None;
            }
            for (i, c) in self.text[self.offset..].char_indices() {
                if self.in_entity {
                    if c == ';' {
                        self.in_entity = false;
                        let text = &self.text[self.offset..self.offset + i];
                        let result = if let Some(text) = text.strip_prefix('#') {
                            numeric_entity(self.offset, text)
                        } else {
                            Ok(Txt::Entity(self.offset + self.pos, text))
                        };
                        self.offset += i + 1;
                        return Some(result);
                    }
                } else if c == '&' {
                    self.in_entity = true;
                    let result = Txt::Text(
                        self.offset + self.pos,
                        &self.text[self.offset..self.offset + i],
                    );
                    self.offset += i + 1;
                    return Some(Ok(result));
                }
            }
            if self.in_entity {
                Some(Err(ValidatorError::InvalidOrUnclosedEntity {
                    span: self.pos + self.offset - 1..self.pos + self.offset,
                }))
            } else {
                let result = Txt::Text(self.offset + self.pos, &self.text[self.offset..]);
                self.offset = self.text.len();
                Some(Ok(result))
            }
        }
    }
    fn numeric_entity(pos: usize, text: &str) -> Result<Txt, ValidatorError> {
        if text.is_empty() {
            return Err(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos });
        }
        let c = if let Some(text) = text.strip_prefix('x') {
            let pos = pos + 1;
            if text.is_empty() {
                return Err(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos });
            }
            u32::from_str_radix(text, 16)
                .map_err(|_e| ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?
        } else {
            text.parse()
                .map_err(|_e| ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?
        };
        Ok(Txt::Char(
            pos,
            std::char::from_u32(c)
                .ok_or(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?,
        ))
    }
    Entities {
        text,
        pos,
        offset: 0,
        in_entity: false,
    }
}

fn is_whitespace_char(c: char) -> bool {
    ['\x20', '\x09', '\x0d', '\x0a'].contains(&c)
}

fn is_whitespace_str(s: &str) -> bool {
    s.chars().all(is_whitespace_char)
}

#[derive(Default)]
struct ElementStack<'a> {
    elements: Vec<ElementState<'a>>,
}

impl<'a> ElementStack<'a> {
    fn lookup_namespace_uri(&self, prefix: &str) -> Option<StrSpan<'a>> {
        self.elements
            .iter()
            .rev()
            .find_map(|elem| elem.lookup_namespace_uri(prefix))
    }

    fn try_lookup_namespace_uri(
        &self,
        prefix: StrSpan<'a>,
    ) -> Result<Option<StrSpan<'a>>, ValidatorError<'a>> {
        if "" == prefix.as_str() {
            Ok(None)
        } else {
            Ok(Some(self.lookup_namespace_uri(&prefix).ok_or(
                ValidatorError::UndefinedNamespacePrefix { prefix },
            )?))
        }
    }

    fn push(&mut self, prefix: StrSpan<'a>, local: StrSpan<'a>, _span: StrSpan<'a>) {
        self.elements.push(ElementState {
            prefix,
            local,
            namespaces: vec![],
            attributes: vec![],
        })
    }
    fn add_attr(
        &mut self,
        prefix: StrSpan<'a>,
        local: StrSpan<'a>,
        value: StrSpan<'a>,
        span: StrSpan<'a>,
    ) {
        if prefix.as_str() == "xmlns" {
            self.elements.last_mut().unwrap().namespaces.push(Ns {
                prefix: local,
                namespace_uri: value,
            })
        } else if prefix.as_str() == "" && local.as_str() == "xmlns" {
            self.elements.last_mut().unwrap().namespaces.push(Ns {
                prefix,
                namespace_uri: value,
            })
        } else {
            self.elements
                .last_mut()
                .unwrap()
                .attributes
                .push(UnresolvedAttr {
                    prefix,
                    local,
                    value,
                    span,
                })
        }
    }
    fn current_element(&self) -> Result<QualifiedName<'a>, ValidatorError<'a>> {
        let curr = self.elements.last().unwrap();
        let namespace_uri = self.try_lookup_namespace_uri(curr.prefix)?;
        Ok(QualifiedName {
            namespace_uri,
            local_name: curr.local,
        })
    }
    fn current_attributes(&self) -> Result<Vec<Attr<'a>>, ValidatorError<'a>> {
        self.elements
            .last()
            .unwrap()
            .attributes
            .iter()
            .map(move |unresolved| {
                let namespace_uri = if unresolved.prefix.as_str() == "" {
                    None
                } else {
                    self.try_lookup_namespace_uri(unresolved.prefix)?
                };
                Ok(Attr {
                    name: QualifiedName {
                        namespace_uri,
                        local_name: unresolved.local,
                    },
                    value: unresolved.value,
                    span: unresolved.span,
                })
            })
            .collect()
    }
}

struct UnresolvedAttr<'a> {
    prefix: StrSpan<'a>,
    local: StrSpan<'a>,
    value: StrSpan<'a>,
    span: StrSpan<'a>,
}
struct ElementState<'a> {
    prefix: StrSpan<'a>,
    local: StrSpan<'a>,
    namespaces: Vec<Ns<'a>>,
    attributes: Vec<UnresolvedAttr<'a>>,
}

impl<'a> ElementState<'a> {
    fn lookup_namespace_uri(&self, prefix: &str) -> Option<StrSpan<'a>> {
        self.namespaces
            .iter()
            .find(|ns| ns.prefix.as_str() == prefix)
            .map(|ns| ns.namespace_uri)
    }
}

struct Ns<'a> {
    prefix: StrSpan<'a>,
    namespace_uri: StrSpan<'a>,
}

#[cfg(test)]
mod tests {
    use crate::Validator;
    use assert_matches::assert_matches;
    use relaxng_model::model::DefineRule;
    use relaxng_model::{Compiler, Files, RelaxError, Syntax};
    use std::io;
    use std::path::Path;

    struct Fixture {
        schema: std::rc::Rc<std::cell::RefCell<Option<DefineRule>>>,
    }
    impl Fixture {
        fn correct(schema: &str) -> Fixture {
            struct FS(String);
            impl Files for FS {
                fn load(&self, name: &Path) -> Result<String, RelaxError> {
                    let t = match name.to_str().unwrap() {
                        "main.rnc" => self.0.clone(),
                        _ => {
                            return Err(RelaxError::Io(
                                name.to_path_buf(),
                                io::Error::from(io::ErrorKind::NotFound),
                            ))
                        }
                    };
                    Ok(t)
                }
            }
            let mut c = Compiler::new(FS(schema.to_string()), Syntax::Compact);
            let input = Path::new("main.rnc");
            let schema = match c.compile(input) {
                Ok(s) => s,
                Err(e) => {
                    c.dump_diagnostic(&e);
                    panic!("{e:?}");
                }
            };
            Fixture { schema }
        }

        fn valid(&self, xml: &str) {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new(self.schema.clone(), reader);
            while let Some(i) = v.validate_next() {
                if let Err(err) = i {
                    let (map, d) = v.diagnostic("valid.xml".to_string(), xml.to_string(), &err);
                    let mut emitter = codemap_diagnostic::Emitter::stderr(
                        codemap_diagnostic::ColorConfig::Auto,
                        Some(&map),
                    );
                    emitter.emit(&d[..]);
                    panic!("{err:?}");
                }
            }
        }

        fn invalid(&self, xml: &str) {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new(self.schema.clone(), reader);
            while let Some(i) = v.validate_next() {
                if let Err(_err) = i {
                    return;
                }
            }
            panic!("Invalid input was accepted by the validator")
        }
    }

    fn check_simple(schema: &str, doc: &str) -> Result<(), String> {
        struct FS(String);
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "main.rnc" => self.0.clone(),
                    _ => {
                        return Err(RelaxError::Io(
                            name.to_path_buf(),
                            io::Error::from(io::ErrorKind::NotFound),
                        ))
                    }
                };
                Ok(t)
            }
        }
        let mut c = Compiler::new(FS(schema.to_string()), Syntax::Compact);
        let input = Path::new("main.rnc");
        let schema = match c.compile(input) {
            Ok(s) => s,
            Err(e) => {
                c.dump_diagnostic(&e);
                panic!("{e:?}");
            }
        };

        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(schema, reader);
        println!("====");
        v.schema.d(v.current_step).unwrap();
        println!("====");
        let mut fail = None;
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                fail = Some(format!("{err:?}"));
                break;
            }
        }
        if let Some(err) = fail {
            return Err(format!("{err:?}"));
        }
        Ok(())
    }

    #[test]
    fn recur() {
        let res = check_simple(
            "start = b  b = element a { b? }",
            "<?xml version=\"1.0\"?><a><a></a></a>",
        );
        assert!(res.is_ok())
    }

    #[test]
    fn attr() {
        let res = check_simple(
            "start = element a { attribute b { text } }",
            "<?xml version=\"1.0\"?><a b=\"\"/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn attr_group_incomplete() {
        // schema requires both attributes, but the instance document only has the first one
        let res = check_simple(
            "start = element e { attribute a { text }, attribute b { text } }",
            "<?xml version=\"1.0\"?><e a=\"\"/>",
        );
        assert_matches!(res, Err(_));
    }

    #[test]
    fn attr_choice() {
        let res = check_simple(
            "start = element a { attribute a { text } | attribute b { text } }",
            "<?xml version=\"1.0\"?><a a=\"\"/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn attr_unexpected() {
        let res = check_simple(
            "start = element a { attribute a { text } }",
            "<?xml version=\"1.0\"?><a a=\"\" b=\"\"/>",
        );
        assert_matches!(res, Err(_));
    }

    #[test]
    fn elem_text_empty() {
        // if the only child of an element is whitespace, this should still match 'empty'
        let res = check_simple(
            "start = element e { empty }",
            "<?xml version=\"1.0\"?><e> </e>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn elem_only_children_whitespace() {
        // if the only child of an element is whitespace, this should still match 'empty'
        let res = check_simple(
            "start = element e1 { element e2 { empty } }",
            "<?xml version=\"1.0\"?><e1> <e2/> </e1>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn list() {
        let res = check_simple(
            "start = element e1 { list { token 'one', token 'two' } }",
            "<?xml version=\"1.0\"?><e1>one two</e1>",
        );
        if let Err(e) = res {
            panic!("{e:?}");
        }
    }

    #[test]
    fn list_one_or_more() {
        let res = check_simple(
            "start = element e1 { list { 'x'+ } }",
            "<?xml version=\"1.0\"?><e1>x</e1>",
        );
        if let Err(e) = res {
            panic!("{e:?}");
        }
    }

    #[test]
    fn element_whitespace_one_or_more() {
        // the whitespace prior to the initial <b/> should not make the doc invalid
        Fixture::correct("start = element a { element b { empty }+ }")
            .valid("<a> <b/><b/><b/></a>");
    }

    #[test]
    fn text_element_group() {
        // a bug in the implementation of start_tag_open_deriv(Pat::Group) used to cause this to
        // fail
        Fixture::correct("start = element a { text, element b { empty } }")
            .valid("<a>foo <b/></a>");
    }

    #[test]
    fn datavalue_in_parts() {
        // the ampersand in the input document means that the text is delivered to the validator
        // in pieces,
        //  - the initial "foo "
        //  - the "&" of the decoded character reference
        //  - the trailing " bar"
        // this should still be a valid value of the string datatype
        Fixture::correct("start = element a { xsd:string }").valid("<a>foo &amp; bar</a>");
    }

    #[test]
    fn top_level_grammar() {
        Fixture::correct("grammar { start = element a { empty } }").valid("<a/>");
    }

    #[test]
    fn attribute_any_other_namespace() {
        Fixture::correct(
            "namespace local = \"\" \
             default namespace foo = \"urn:foo\" \
             element MPD { \
                attribute mediaPresentationDuration { xsd:duration }?, \
                attribute * - (foo:* | local:*)  { text } \
             }",
        )
        .invalid(
            "<?xml verion=\"1.0\"?>\
            <MPD xmlns=\"urn:foo\" mediaPresentationDuration=\"XPT0H11M58.998S\"></MPD>",
        );
    }

    #[test]
    #[ignore = "we need some optimisations in place to avoid exponential blow-up"]
    fn blowup() {
        // https://relaxng.org/jclark/derivative.html#Avoiding_exponential_blowup
        Fixture::correct(
            "start = element root { a8 | b8 } \
            a8 = a7 | b7+ \
            b8 = b7 | a7+ \
            a7 = a6 | b6+ \
            b7 = b6 | a6+ \
            a6 = a5 | b5+ \
            b6 = b5 | a5+ \
            a5 = a4 | b4+ \
            b5 = b4 | a4+ \
            a4 = a3 | b3+ \
            b4 = b3 | a3+ \
            a3 = a2 | b2+ \
            b3 = b2 | a2+ \
            a2 = a1 | b1+ \
            b2 = b1 | a1+ \
            a1 = a | b+ \
            b1 = b | a+ \
            a = element a { text } \
            b = element b { text }",
        )
        .valid("<root><b/><b/><b/></root>");
    }

    #[test]
    fn parse_entities() {
        let mut iter = super::parse_entities(0, "foo &bar; blat");
        assert_matches!(iter.next(), Some(Ok(super::Txt::Text(0, "foo "))));
        assert_matches!(iter.next(), Some(Ok(super::Txt::Entity(5, "bar"))));
        assert_matches!(iter.next(), Some(Ok(super::Txt::Text(9, " blat"))));
    }
}
