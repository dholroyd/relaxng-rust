use relaxng_model::datatype::Datatype;
use relaxng_model::model::{NameClass, PatRef};
use relaxng_model::{datatype, model};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::rc::Rc;
use xmlparser::{ElementEnd, EntityDefinition, StrSpan, Token, Tokenizer};

/// Identity key for a schema definition, used to deduplicate recursive `Ref`
/// patterns during compilation.  Holds a clone of the model's `Rc` so that the
/// heap allocation (and therefore its address) is guaranteed to remain stable.
#[derive(Clone)]
struct DefineId(Rc<RefCell<Option<model::DefineRule>>>);

impl DefineId {
    fn of(r: &PatRef) -> Self {
        Self(r.0.clone())
    }
}

impl PartialEq for DefineId {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for DefineId {}

impl Hash for DefineId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

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
    TextBufferOverflow,
    TooManyPatterns,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct PatId(u16);

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Pat {
    Choice(PatId, PatId, bool),
    Interleave(PatId, PatId, bool),
    Group(PatId, PatId, bool),
    OneOrMore(PatId, bool),
    Empty,
    Text,
    NotAllowed,
    Attribute(Box<model::NameClass>, PatId),
    Element(Box<model::NameClass>, PatId),
    Datatype(Box<datatype::Datatypes>),
    DatatypeValue(Box<datatype::DatatypeValues>),
    DatatypeExcept(Box<datatype::Datatypes>, PatId),
    List(PatId),
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
            Pat::After(_, _) => false,
        }
    }
}

type NameCache = HashMap<(PatId, Box<str>, Option<Box<str>>), PatId>;

#[derive(Default)]
struct Inner {
    memo: HashMap<Pat, PatId>,
    patterns: Vec<Pat>,
    span_map: HashMap<u16, Vec<codemap::Span>>,

    // Our implementation of https://relaxng.org/jclark/derivative.html#Memoization

    // Persistent cross-call memo: (input PatId, local-name, namespace-uri) -> result PatId
    start_tag_open_cache: NameCache,
    // Persistent cross-call memo: input PatId -> result PatId
    start_tag_close_cache: HashMap<PatId, PatId>,
    // Persistent cross-call memo: input PatId -> result PatId (Vec indexed by PatId.0)
    mixed_text_cache: Vec<Option<PatId>>,
    // Persistent cross-call memo: (input PatId, local-name, namespace-uri) -> result PatId
    start_att_cache: NameCache,
}
#[derive(Default)]
struct Schema {
    inner: RefCell<Inner>,
    coverage: Option<Box<[u64]>>,
    compile_time_count: u16,
    /// Namespace context for QName resolution during text_deriv.
    /// Set by the validator before calls to text_deriv.
    ns_context: Option<NsContext>,
}

/// Captured namespace context for resolving prefixes in document text.
#[derive(Clone)]
struct NsContext {
    default_ns: String,
    bindings: Vec<(String, String)>,
}
impl NsContext {
    fn lookup(&self, prefix: &str) -> Option<String> {
        if prefix == "xml" {
            return Some("http://www.w3.org/XML/1998/namespace".to_string());
        }
        self.bindings
            .iter()
            .rev()
            .find(|(p, _)| p == prefix)
            .map(|(_, uri)| uri.clone())
    }
}
/// Build-time state for compiling a relaxng-model schema into the internal
/// pattern representation.  Uses `Vec<Option<Pat>>` so that recursive `Ref`
/// patterns can reserve a slot (`None`) before the referenced definition has
/// been compiled.  After compilation, `finalize()` converts to `Vec<Pat>`.
struct SchemaBuilder {
    memo: HashMap<Pat, PatId>,
    patterns: Vec<Option<Pat>>,
    refs: HashMap<DefineId, PatId>,
    span_map: HashMap<u16, Vec<codemap::Span>>,
    /// Deferred `(placeholder, target)` pairs where the target was still an
    /// unresolved slot at the time `resolve_ref` was called (mutual recursion).
    deferred: Vec<(PatId, PatId)>,
    /// Set to true if the pattern limit (2^16) was exceeded during compilation.
    overflowed: bool,
}

impl SchemaBuilder {
    fn new() -> Self {
        SchemaBuilder {
            memo: HashMap::new(),
            patterns: Vec::new(),
            refs: HashMap::new(),
            span_map: HashMap::new(),
            deferred: Vec::new(),
            overflowed: false,
        }
    }

    fn push(&mut self, p: Pat) -> PatId {
        if self.patterns.len() > 0xffff {
            self.overflowed = true;
            return *self.memo.get(&Pat::NotAllowed).unwrap_or(&PatId(0));
        }
        if let Some(id) = self.memo.get(&p) {
            *id
        } else {
            let id = PatId(self.patterns.len() as u16);
            self.memo.insert(p.clone(), id);
            self.patterns.push(Some(p));
            id
        }
    }

    fn try_patt(&self, id: PatId) -> Option<&Pat> {
        self.patterns[id.0 as usize].as_ref()
    }

    fn is_nullable_id(&self, id: PatId) -> bool {
        self.try_patt(id).is_some_and(|p| p.is_nullable())
    }

    fn record_source_span(&mut self, id: PatId, span: codemap::Span) {
        self.span_map.entry(id.0).or_default().push(span);
    }

    fn choice(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) => right,
            (_, Some(Pat::NotAllowed)) => left,
            (Some(_), Some(_)) if left == right => left,
            (l, r) => {
                let nullable =
                    l.is_some_and(|p| p.is_nullable()) || r.is_some_and(|p| p.is_nullable());
                self.push(Pat::Choice(left, right, nullable))
            }
        }
    }

    fn interleave(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) | (_, Some(Pat::NotAllowed)) => self.not_allowed(),
            (Some(Pat::Empty), _) => right,
            (_, Some(Pat::Empty)) => left,
            (l, r) => {
                let nullable =
                    l.is_some_and(|p| p.is_nullable()) && r.is_some_and(|p| p.is_nullable());
                self.push(Pat::Interleave(left, right, nullable))
            }
        }
    }

    fn group(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) | (_, Some(Pat::NotAllowed)) => self.not_allowed(),
            (Some(Pat::Empty), _) => right,
            (_, Some(Pat::Empty)) => left,
            (l, r) => {
                let nullable =
                    l.is_some_and(|p| p.is_nullable()) && r.is_some_and(|p| p.is_nullable());
                self.push(Pat::Group(left, right, nullable))
            }
        }
    }

    fn one_or_more(&mut self, pattern: PatId) -> PatId {
        let nullable = self.is_nullable_id(pattern);
        self.push(Pat::OneOrMore(pattern, nullable))
    }

    fn mixed(&mut self, pattern: PatId) -> PatId {
        let text = self.text();
        self.interleave(pattern, text)
    }

    fn empty(&mut self) -> PatId {
        self.push(Pat::Empty)
    }

    fn text(&mut self) -> PatId {
        self.push(Pat::Text)
    }

    fn not_allowed(&mut self) -> PatId {
        self.push(Pat::NotAllowed)
    }

    fn attribute(&mut self, name: model::NameClass, p: PatId) -> PatId {
        self.push(Pat::Attribute(Box::new(name), p))
    }

    fn element(&mut self, name: model::NameClass, p: PatId) -> PatId {
        self.push(Pat::Element(Box::new(name), p))
    }

    fn datatype_value(&mut self, dt: datatype::DatatypeValues) -> PatId {
        self.push(Pat::DatatypeValue(Box::new(dt)))
    }

    fn datatype_name(&mut self, dt: datatype::Datatypes, except: Option<PatId>) -> PatId {
        if let Some(except) = except {
            self.push(Pat::DatatypeExcept(Box::new(dt), except))
        } else {
            self.push(Pat::Datatype(Box::new(dt)))
        }
    }

    fn list(&mut self, p: PatId) -> PatId {
        self.push(Pat::List(p))
    }

    fn get_ref(&self, id: &DefineId) -> Option<PatId> {
        self.refs.get(id).copied()
    }

    fn reserve_slot(&mut self, define_id: DefineId, _name: &str) -> PatId {
        if self.patterns.len() > 0xffff {
            self.overflowed = true;
            return *self.memo.get(&Pat::NotAllowed).unwrap_or(&PatId(0));
        }
        let pat_id = PatId(self.patterns.len() as u16);
        self.patterns.push(None);
        self.refs.insert(define_id, pat_id);
        pat_id
    }

    fn resolve_ref(&mut self, placeholder_id: PatId, id: PatId, name: &str) {
        if placeholder_id == id {
            return;
        }
        match &self.patterns[placeholder_id.0 as usize] {
            None => (),
            Some(p) => panic!(
                "expected reserved slot but got {:?}, with id {} while trying to resolve it to {}, for definition {:?}",
                p, placeholder_id.0, id.0, name
            ),
        }
        match self.patterns[id.0 as usize].clone() {
            Some(target) => {
                self.patterns[placeholder_id.0 as usize] = Some(target);
            }
            None => {
                // Target is still an unresolved placeholder (mutual recursion);
                // defer resolution until finalize().
                self.deferred.push((placeholder_id, id));
            }
        }
    }

    fn finalize(mut self) -> Inner {
        // Resolve deferred placeholders from mutual recursion.  Iterate until
        // no more progress is made to handle chains (A→B→C).
        let mut made_progress = true;
        while made_progress {
            made_progress = false;
            self.deferred.retain(|&(placeholder, target)| {
                if let Some(pat) = self.patterns[target.0 as usize].clone() {
                    self.patterns[placeholder.0 as usize] = Some(pat);
                    made_progress = true;
                    false // resolved, remove from list
                } else {
                    true // still unresolved, keep
                }
            });
        }
        let patterns: Vec<Pat> = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(i, p)| p.unwrap_or_else(|| panic!("unresolved slot at index {}", i)))
            .collect();
        Inner {
            memo: self.memo,
            patterns,
            span_map: self.span_map,
            ..Default::default()
        }
    }
}

impl Schema {
    fn push(&self, p: Pat) -> PatId {
        let mut inner = self.inner.borrow_mut();
        if inner.patterns.len() > 0xffff {
            // Gracefully degrade: treat as not-allowed rather than panicking.
            // The NotAllowed sentinel is always at index 0 or exists in the memo.
            return *inner.memo.get(&Pat::NotAllowed).unwrap_or(&PatId(0));
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
    #[inline(always)]
    fn mark_covered(&mut self, id: PatId) {
        if let Some(ref mut bits) = self.coverage {
            let limit = self.compile_time_count as usize;
            let idx = id.0 as usize;
            if idx < limit {
                bits[idx / 64] |= 1u64 << (idx % 64);
            }
            // Resolved Ref placeholders hold a copy of the original pattern,
            // but the memo maps the pattern to its canonical PatId.  Mark both
            // so that coverage_report() (which iterates by canonical PatId)
            // sees the hit.
            let inner = self.inner.borrow();
            if let Some(&canonical) = inner.memo.get(&inner.patterns[idx]) {
                let cidx = canonical.0 as usize;
                if cidx < limit && cidx != idx {
                    bits[cidx / 64] |= 1u64 << (cidx % 64);
                }
            }
        }
    }
    pub fn choice(&self, left: PatId, right: PatId) -> PatId {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => right,
            (_, Pat::NotAllowed) => left,
            (l, r) => {
                if left == right {
                    return left;
                }
                if !matches!(l, Pat::Choice(..)) && !matches!(r, Pat::Choice(..)) {
                    // Both are single non-Choice leaves and are already distinct —
                    // no deduplication is possible, take the fast path.
                    return self.push(Pat::Choice(left, right, l.is_nullable() || r.is_nullable()));
                }
                // At least one side is a Choice tree: eliminate duplicate leaves to
                // prevent polynomial growth when the same sub-pattern appears via
                // multiple derivative paths (e.g. interleaved alternatives sharing a
                // common attribute).
                let inner = self.inner.borrow();
                let mut left_set = HashSet::new();
                Self::collect_leaves_into_set(left, &inner, &mut left_set);
                let mut new_right = Vec::new();
                Self::collect_new_leaves(right, &inner, &left_set, &mut new_right);
                drop(inner);
                if new_right.is_empty() {
                    return left;
                }
                new_right.into_iter().fold(left, |acc, r| {
                    let nullable = self.patt(acc).is_nullable() || self.patt(r).is_nullable();
                    self.push(Pat::Choice(acc, r, nullable))
                })
            }
        }
    }

    // Traverses a Choice tree and inserts every leaf PatId into `set`.
    // Using an accumulator avoids the O(N²) Vec allocations of a naive recursive approach.
    fn collect_leaves_into_set(id: PatId, inner: &Inner, set: &mut HashSet<PatId>) {
        match &inner.patterns[id.0 as usize] {
            Pat::Choice(l, r, _) => {
                Self::collect_leaves_into_set(*l, inner, set);
                Self::collect_leaves_into_set(*r, inner, set);
            }
            _ => {
                set.insert(id);
            }
        }
    }

    // Traverses a Choice tree and appends to `out` any leaf PatId not present in `left_set`.
    fn collect_new_leaves(
        id: PatId,
        inner: &Inner,
        left_set: &HashSet<PatId>,
        out: &mut Vec<PatId>,
    ) {
        match &inner.patterns[id.0 as usize] {
            Pat::Choice(l, r, _) => {
                Self::collect_new_leaves(*l, inner, left_set, out);
                Self::collect_new_leaves(*r, inner, left_set, out);
            }
            _ => {
                if !left_set.contains(&id) {
                    out.push(id);
                }
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
        match (self.patt(p1), self.patt(p2)) {
            (Pat::NotAllowed, _) | (_, Pat::NotAllowed) => self.not_allowed(),
            (_, _) => self.push(Pat::After(p1, p2)),
        }
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
    fn list(&self, p: PatId) -> PatId {
        self.push(Pat::List(p))
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
            Pat::After(_, _) => unreachable!(),
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
            Pat::After(_, _) => unreachable!(),
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

fn describe_nameclass(nc: &NameClass, desc: &mut String) {
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

fn describe_datatype(dt: &datatype::Datatypes) -> String {
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
                NmTokens(_) => "xsd:NMTOKENS",
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
                IdRefs(_) => "xsd:IDREFS",
                Entity(_) => "xsd:ENTITY",
                Entities(_) => "xsd:ENTITIES",
            };
            name.to_string()
        }
    }
}

fn describe_datatype_value(dt: &datatype::DatatypeValues) -> String {
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
                String(s) | Token(s) => format!("\"{}\"", s),
                QName(q) => format!("{:?}", q),
            }
        }
    }
}

/// Description of a trackable pattern in the schema.
pub struct TrackablePattern {
    /// Index of this pattern in the schema's pattern arena.
    pub pat_id: u16,
    /// Kind of pattern.
    pub kind: &'static str,
    /// Human-readable name or description.
    pub name: String,
    /// Source spans where this pattern was defined.
    pub spans: Vec<codemap::Span>,
}

/// Coverage report tracking which schema patterns were exercised during validation.
pub struct CoverageReport {
    covered: Box<[u64]>,
    patterns: Vec<TrackablePattern>,
}

impl CoverageReport {
    /// Check whether a specific pattern (by arena index) was covered.
    pub fn is_covered(&self, pat_id: u16) -> bool {
        let idx = pat_id as usize;
        idx < self.covered.len() * 64 && (self.covered[idx / 64] >> (idx % 64)) & 1 != 0
    }

    /// Merge another report into this one (bitwise OR). Use this to aggregate
    /// coverage across multiple document validations against the same schema.
    pub fn merge(&mut self, other: &CoverageReport) {
        for (a, b) in self.covered.iter_mut().zip(other.covered.iter()) {
            *a |= *b;
        }
    }

    /// Number of trackable patterns that were covered.
    pub fn covered_count(&self) -> usize {
        self.patterns
            .iter()
            .filter(|p| self.is_covered(p.pat_id))
            .count()
    }

    /// Total number of trackable patterns in the schema.
    pub fn total_trackable(&self) -> usize {
        self.patterns.len()
    }

    /// All trackable patterns.
    pub fn patterns(&self) -> &[TrackablePattern] {
        &self.patterns
    }

    /// Iterator over patterns that were NOT covered.
    pub fn uncovered_patterns(&self) -> impl Iterator<Item = &TrackablePattern> {
        self.patterns.iter().filter(|p| !self.is_covered(p.pat_id))
    }
}

pub struct Validator<'a> {
    schema: Schema,
    tokenizer: Tokenizer<'a>,
    current_step: PatId,
    last_was_start_element: bool,
    stack: ElementStack<'a>,
    entity_definitions: HashMap<String, String>,
    text_buffer: String,
    max_text_buffer: usize,
}

impl<'a> Validator<'a> {
    pub fn new(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        tokenizer: Tokenizer<'a>,
    ) -> Result<Validator<'a>, ValidatorError<'a>> {
        let mut builder = SchemaBuilder::new();
        let start = Self::compile(
            &mut builder,
            Rc::as_ref(&model).borrow().as_ref().unwrap().pattern(),
        );
        if builder.overflowed {
            return Err(ValidatorError::TooManyPatterns);
        }
        let schema = Schema {
            inner: RefCell::new(builder.finalize()),
            ..Default::default()
        };
        let mut entity_definitions = HashMap::default();
        entity_definitions.insert("lt".to_string(), "<".to_string());
        entity_definitions.insert("gt".to_string(), ">".to_string());
        entity_definitions.insert("amp".to_string(), "&".to_string());
        entity_definitions.insert("apos".to_string(), "'".to_string());
        entity_definitions.insert("quot".to_string(), "\"".to_string());
        Ok(Validator {
            schema,
            tokenizer,
            current_step: start,
            last_was_start_element: false,
            stack: ElementStack::default(),
            entity_definitions,
            text_buffer: String::new(),
            max_text_buffer: 1024 * 1024,
        })
    }

    /// Create a validator with coverage tracking enabled.
    pub fn new_with_coverage(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        tokenizer: Tokenizer<'a>,
    ) -> Result<Validator<'a>, ValidatorError<'a>> {
        let mut v = Self::new(model, tokenizer)?;
        let compile_time_count = v.schema.inner.borrow().patterns.len() as u16;
        let word_count = (compile_time_count as usize).div_ceil(64);
        v.schema.compile_time_count = compile_time_count;
        v.schema.coverage = Some(vec![0u64; word_count].into_boxed_slice());
        Ok(v)
    }

    /// Extract the coverage report. Returns `None` if coverage tracking was not enabled.
    pub fn coverage_report(&self) -> Option<CoverageReport> {
        let covered = self.schema.coverage.as_ref()?.clone();
        let inner = self.schema.inner.borrow();
        let count = self.schema.compile_time_count as usize;
        let mut patterns = Vec::new();
        for i in 0..count {
            let pat = &inner.patterns[i];
            // Skip resolved-Ref duplicates: if the memo maps this pattern to
            // a different (canonical) PatId, this slot is a placeholder copy.
            if inner.memo.get(pat).is_some_and(|c| c.0 as usize != i) {
                continue;
            }
            let (kind, name) = match pat {
                Pat::Element(nc, _) => {
                    let mut desc = String::new();
                    describe_nameclass(nc, &mut desc);
                    ("Element", desc)
                }
                Pat::Attribute(nc, _) => {
                    let mut desc = String::new();
                    describe_nameclass(nc, &mut desc);
                    ("Attribute", desc)
                }
                Pat::Datatype(dt) => ("Datatype", describe_datatype(dt)),
                Pat::DatatypeValue(dt) => ("DatatypeValue", describe_datatype_value(dt)),
                Pat::DatatypeExcept(dt, _) => ("DatatypeExcept", describe_datatype(dt)),
                Pat::Text => ("Text", "text".to_string()),
                _ => continue,
            };
            let spans = inner.span_map.get(&(i as u16)).cloned().unwrap_or_default();
            patterns.push(TrackablePattern {
                pat_id: i as u16,
                kind,
                name,
                spans,
            });
        }
        Some(CoverageReport { covered, patterns })
    }

    fn compile(s: &mut SchemaBuilder, p: &model::Pattern) -> PatId {
        match p {
            model::Pattern::Choice(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    let l = Self::compile(s, left);
                    right = s.choice(l, right);
                }
                right
            }
            model::Pattern::Interleave(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    let l = Self::compile(s, left);
                    right = s.interleave(l, right);
                }
                right
            }
            model::Pattern::Group(v) => {
                let mut iter = v.iter().rev();
                let mut right = Self::compile(s, iter.next().unwrap());
                for left in iter {
                    let l = Self::compile(s, left);
                    right = s.group(l, right);
                }
                right
            }
            model::Pattern::Mixed(p) => {
                let inner = Self::compile(s, p);
                s.mixed(inner)
            }
            model::Pattern::Empty => s.empty(),
            model::Pattern::Text(span) => {
                let id = s.text();
                if let Some(span) = span {
                    s.record_source_span(id, *span);
                }
                id
            }
            model::Pattern::NotAllowed => s.not_allowed(),
            model::Pattern::Optional(p) => {
                let inner = Self::compile(s, p);
                let empty = s.empty();
                s.choice(inner, empty)
            }
            model::Pattern::ZeroOrMore(p) => {
                let inner = Self::compile(s, p);
                let one = s.one_or_more(inner);
                let empty = s.empty();
                s.choice(one, empty)
            }
            model::Pattern::OneOrMore(p) => {
                let inner = Self::compile(s, p);
                s.one_or_more(inner)
            }
            model::Pattern::Attribute(name, p, span, _) => {
                let content = Self::compile(s, p);
                let id = s.attribute(name.clone(), content);
                if let Some(span) = span {
                    s.record_source_span(id, *span);
                }
                id
            }
            model::Pattern::Element(name, p, span, _) => {
                let content = Self::compile(s, p);
                let id = s.element(name.clone(), content);
                if let Some(span) = span {
                    s.record_source_span(id, *span);
                }
                id
            }
            model::Pattern::Ref(whence, name, r) => {
                let define_id = DefineId::of(r);
                if let Some(id) = s.get_ref(&define_id) {
                    id
                } else {
                    let placeholder_id = s.reserve_slot(define_id, name);
                    if let Some(thing) = Rc::as_ref(&r.0).borrow().as_ref() {
                        let id = Self::compile(s, thing.pattern());
                        s.resolve_ref(placeholder_id, id, name);
                        placeholder_id
                    } else {
                        panic!("Somehow definition for {name:#?} is missing, used in {whence:?}")
                    }
                }
            }
            model::Pattern::DatatypeValue { datatype, span } => {
                let id = s.datatype_value(datatype.clone());
                if let Some(span) = span {
                    s.record_source_span(id, *span);
                }
                id
            }
            model::Pattern::DatatypeName {
                datatype,
                except,
                span,
            } => {
                let except_id = except.as_ref().map(|e| Self::compile(s, e));
                let id = s.datatype_name(datatype.clone(), except_id);
                if let Some(span) = span {
                    s.record_source_span(id, *span);
                }
                id
            }
            model::Pattern::List(p) => {
                let inner = Self::compile(s, p);
                s.list(inner)
            }
        }
    }

    pub fn validate_next(&mut self) -> Option<Result<(), ValidatorError<'a>>> {
        match self.tokenizer.next() {
            Some(Ok(evt)) => Some(self.validate(evt)),
            Some(Err(err)) => Some(Err(ValidatorError::Xml(err))),
            None => None,
        }
    }

    /// Flush any buffered text by applying text_deriv.
    /// Returns true if the result was NotAllowed.
    fn flush_text(&mut self) -> bool {
        if !self.text_buffer.is_empty() {
            let next_id = Self::text_deriv(self.current_step, &mut self.schema, &self.text_buffer);
            self.text_buffer.clear();
            self.current_step = next_id;
            matches!(self.schema.patt(next_id), Pat::NotAllowed)
        } else {
            false
        }
    }

    #[allow(unused)]
    fn assert_health(&self) {
        let mut seen = vec![];
        self.schema.check_choices(self.current_step, &mut seen);
    }

    fn validate(&mut self, evt: Token<'a>) -> Result<(), ValidatorError<'a>> {
        self.schema.ns_context = if self.stack.elements.is_empty() {
            None
        } else {
            Some(self.stack.capture_ns_context())
        };
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
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
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
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
                match end {
                    ElementEnd::Open => Self::close_element_start(
                        &self.stack,
                        &mut self.schema,
                        evt,
                        self.current_step,
                    )?,
                    ElementEnd::Close(_, _) => {
                        let next_pid = if self.last_was_start_element {
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
                            Self::text_deriv(self.current_step, &mut self.schema, "")
                        } else {
                            self.current_step
                        };
                        Self::end_tag_deriv(next_pid, &mut self.schema)
                    }
                    ElementEnd::Empty => {
                        let next_id = Self::close_element_start(
                            &self.stack,
                            &mut self.schema,
                            evt,
                            self.current_step,
                        )?;
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
                        let p = Self::text_deriv(next_id, &mut self.schema, "");
                        Self::end_tag_deriv(p, &mut self.schema)
                    }
                }
            }
            Token::Cdata { text, span: _ } => {
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
                let mixed = Self::mixed_text_deriv(self.current_step, &mut self.schema);
                if mixed == self.current_step {
                    self.current_step
                } else {
                    Self::text_deriv(self.current_step, &mut self.schema, &text)
                }
            }
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
                // Fast path: if mixed_text_deriv returns the same PatId, the pattern is a
                // text fixed-point (e.g. After(Text, cont)) and text_deriv is a no-op.
                let mixed = Self::mixed_text_deriv(self.current_step, &mut self.schema);
                if mixed == self.current_step {
                    self.current_step
                } else {
                    // Buffer text for deferred processing — text fragments split by
                    // PIs/comments must be concatenated before validation (spec 6.2.7).
                    self.text_buffer.push_str(data);
                    if self.text_buffer.len() > self.max_text_buffer {
                        return Err(ValidatorError::TextBufferOverflow);
                    }
                    self.last_was_start_element = false;
                    return Ok(());
                }
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
        if let Token::ElementEnd {
            end: ElementEnd::Open,
            ..
        } = evt
        {
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
        pat_id: PatId,
    ) -> Result<PatId, ValidatorError<'b>> {
        let name = stack.current_element()?;
        let next_pat = Self::start_tag_open_deriv(pat_id, schema, name);
        // TODO: refactor early-returns
        let next_pat = match schema.patt(next_pat) {
            Pat::NotAllowed => {
                return Err(ValidatorError::NotAllowed(Token::ElementStart {
                    prefix: name.namespace_uri.unwrap_or_else(|| StrSpan::from("")),
                    local: name.local_name,
                    span: name.local_name,
                }));
            }
            _p => {
                let attributes: Vec<_> = stack.current_attributes()?;
                let mut pat = next_pat;
                for att in attributes {
                    let mid = Self::start_att_deriv(pat, schema, att.name);
                    pat = Self::att_value_deriv(mid, schema, att.value.as_str());
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

    fn text_deriv(pid: PatId, schema: &mut Schema, text: &str) -> PatId {
        let current = schema.patt(pid);
        match current {
            Pat::Choice(p1, p2, _) => {
                let a = Self::text_deriv(p1, schema, text);
                let b = Self::text_deriv(p2, schema, text);
                schema.choice(a, b)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = Self::text_deriv(p1, schema, text);
                let a = schema.interleave(d1, p2);

                let d2 = Self::text_deriv(p2, schema, text);
                let b = schema.interleave(p1, d2);
                schema.choice(a, b)
            }
            Pat::Group(p1, p2, _) => {
                let nullable = schema.patt(p1).is_nullable();
                let d1 = Self::text_deriv(p1, schema, text);
                let p = schema.group(d1, p2);
                if nullable {
                    let d2 = Self::text_deriv(p2, schema, text);
                    schema.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::After(p1, p2) => {
                let d = Self::text_deriv(p1, schema, text);
                schema.after(d, p2)
            }
            Pat::OneOrMore(p, _) => {
                let d = Self::text_deriv(p, schema, text);
                schema.group(d, schema.choice(schema.one_or_more(p), schema.empty()))
            }
            Pat::Text => {
                schema.mark_covered(pid);
                schema.text()
            }
            Pat::Datatype(dt) => {
                if dt.is_valid(text) {
                    schema.mark_covered(pid);
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::DatatypeValue(dt) => {
                let valid = if let Some(ref ns_ctx) = schema.ns_context {
                    let ns_ctx = ns_ctx.clone();
                    dt.is_valid_with_ns(text, &ns_ctx.default_ns, |p| ns_ctx.lookup(p))
                } else {
                    dt.is_valid(text)
                };
                if valid {
                    schema.mark_covered(pid);
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::DatatypeExcept(dt, except) => {
                let d = Self::text_deriv(except, schema, text);
                let pat2 = schema.patt(d);
                if dt.is_valid(text) && !pat2.is_nullable() {
                    schema.mark_covered(pid);
                    schema.empty()
                } else {
                    schema.not_allowed()
                }
            }
            Pat::List(p) => {
                let mut p = p;
                for item in text.split_whitespace() {
                    p = Self::text_deriv(p, schema, item);
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
                    pid
                } else {
                    schema.not_allowed()
                }
            }
        }
    }

    // Per https://relaxng.org/jclark/derivative.html — text nodes in mixed content can only
    // match Text patterns (RELAX NG spec §7.2).  This lets us memoize on PatId alone, ignoring
    // the actual text value.  Returns the same PatId for patterns where text is a fixed-point
    // (e.g. After(Text, cont)), enabling a fast skip of text_deriv at call sites.
    fn mixed_text_deriv(pid: PatId, schema: &mut Schema) -> PatId {
        let idx = pid.0 as usize;
        {
            let inner = schema.inner.borrow();
            if let Some(Some(cached)) = inner.mixed_text_cache.get(idx) {
                return *cached;
            }
        }
        let pat = schema.patt(pid);
        let result = match pat {
            Pat::Choice(p1, p2, _) => {
                let c1 = Self::mixed_text_deriv(p1, schema);
                let c2 = Self::mixed_text_deriv(p2, schema);
                schema.choice(c1, c2)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = Self::mixed_text_deriv(p1, schema);
                let c1 = schema.interleave(d1, p2);
                let d2 = Self::mixed_text_deriv(p2, schema);
                let c2 = schema.interleave(p1, d2);
                schema.choice(c1, c2)
            }
            Pat::After(p1, p2) => {
                let d = Self::mixed_text_deriv(p1, schema);
                schema.after(d, p2)
            }
            Pat::Group(p1, p2, _) => {
                let nullable = schema.patt(p1).is_nullable();
                let d1 = Self::mixed_text_deriv(p1, schema);
                let p = schema.group(d1, p2);
                if nullable {
                    let d2 = Self::mixed_text_deriv(p2, schema);
                    schema.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::OneOrMore(p, _) => {
                let d = Self::mixed_text_deriv(p, schema);
                schema.group(d, schema.choice(schema.one_or_more(p), schema.empty()))
            }
            Pat::Text => {
                schema.mark_covered(pid);
                pid
            }
            _ => schema.not_allowed(),
        };
        let mut inner = schema.inner.borrow_mut();
        if idx >= inner.mixed_text_cache.len() {
            inner.mixed_text_cache.resize(idx + 1, None);
        }
        inner.mixed_text_cache[idx] = Some(result);
        result
    }

    fn start_tag_open_deriv(pid: PatId, schema: &mut Schema, name: QualifiedName<'a>) -> PatId {
        let local_key: Box<str> = name.local_name.as_str().into();
        let ns_key: Option<Box<str>> = name.namespace_uri.map(|s| s.as_str().into());

        // Cache check (borrow released before any mutation)
        {
            let inner = schema.inner.borrow();
            if let Some(&cached) =
                inner
                    .start_tag_open_cache
                    .get(&(pid, local_key.clone(), ns_key.clone()))
            {
                return cached;
            }
        }

        let current = schema.patt(pid);
        let result = match current {
            Pat::Choice(l, r, _) => {
                let d1 = Self::start_tag_open_deriv(l, schema, name);
                let d2 = Self::start_tag_open_deriv(r, schema, name);
                schema.choice(d1, d2)
            }
            Pat::OneOrMore(inner_pid, _) => {
                let deriv = Self::start_tag_open_deriv(inner_pid, schema, name);
                Self::apply_after(schema.patt(deriv), schema, |pat, schema| {
                    schema.group(
                        pat,
                        schema.choice(schema.one_or_more(inner_pid), schema.empty()),
                    )
                })
            }
            Pat::Interleave(pid1, pid2, _) => {
                let d1 = Self::start_tag_open_deriv(pid1, schema, name);
                let c1 = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.interleave(pat, pid2)
                });
                let d2 = Self::start_tag_open_deriv(pid2, schema, name);
                let c2 = Self::apply_after(schema.patt(d2), schema, |pat, schema| {
                    schema.interleave(pid1, pat)
                });
                schema.choice(c1, c2)
            }
            Pat::Group(pid1, pid2, _) => {
                let nullable = schema.patt(pid1).is_nullable();
                let d1 = Self::start_tag_open_deriv(pid1, schema, name);
                let x = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.group(pat, pid2)
                });
                if nullable {
                    let d2 = Self::start_tag_open_deriv(pid2, schema, name);
                    schema.choice(x, d2)
                } else {
                    x
                }
            }
            Pat::Element(ref nc, inner_pat) => {
                if contains(nc, name) {
                    schema.mark_covered(pid);
                    let empty = schema.empty();
                    schema.after(inner_pat, empty)
                } else {
                    schema.not_allowed()
                }
            }
            Pat::After(pid1, pid2) => {
                let d = Self::start_tag_open_deriv(pid1, schema, name);
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
        };

        schema
            .inner
            .borrow_mut()
            .start_tag_open_cache
            .insert((pid, local_key, ns_key), result);
        result
    }

    fn start_att_deriv(pid: PatId, schema: &mut Schema, name: QualifiedName<'a>) -> PatId {
        let local_key: Box<str> = name.local_name.as_str().into();
        let ns_key: Option<Box<str>> = name.namespace_uri.map(|s| s.as_str().into());

        {
            let inner = schema.inner.borrow();
            if let Some(&cached) =
                inner
                    .start_att_cache
                    .get(&(pid, local_key.clone(), ns_key.clone()))
            {
                return cached;
            }
        }

        let current = schema.patt(pid);
        let result = match current {
            Pat::Choice(l, r, _) => {
                let d1 = Self::start_att_deriv(l, schema, name);
                let d2 = Self::start_att_deriv(r, schema, name);
                schema.choice(d1, d2)
            }
            Pat::OneOrMore(inner_pid, _) => {
                let deriv = Self::start_att_deriv(inner_pid, schema, name);
                Self::apply_after(schema.patt(deriv), schema, |pat, schema| {
                    schema.group(
                        pat,
                        schema.choice(schema.one_or_more(inner_pid), schema.empty()),
                    )
                })
            }
            Pat::Interleave(pid1, pid2, _) => {
                let d1 = Self::start_att_deriv(pid1, schema, name);
                let c1 = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.interleave(pat, pid2)
                });
                let d2 = Self::start_att_deriv(pid2, schema, name);
                let c2 = Self::apply_after(schema.patt(d2), schema, |pat, schema| {
                    schema.interleave(pid1, pat)
                });
                schema.choice(c1, c2)
            }
            Pat::Group(pid1, pid2, _) => {
                // Attributes may appear in any order, so unlike start_tag_open_deriv
                // we always try both branches unconditionally.
                let d1 = Self::start_att_deriv(pid1, schema, name);
                let x = Self::apply_after(schema.patt(d1), schema, |pat, schema| {
                    schema.group(pat, pid2)
                });
                let d2 = Self::start_att_deriv(pid2, schema, name);
                let y = Self::apply_after(schema.patt(d2), schema, |pat, schema| {
                    schema.group(pid1, pat)
                });
                schema.choice(x, y)
            }
            Pat::Attribute(ref nc, val_pat) => {
                if contains(nc, name) {
                    schema.mark_covered(pid);
                    let empty = schema.empty();
                    schema.after(val_pat, empty)
                } else {
                    schema.not_allowed()
                }
            }
            Pat::After(pid1, pid2) => {
                let d = Self::start_att_deriv(pid1, schema, name);
                Self::apply_after(schema.patt(d), schema, |pat, schema| {
                    schema.after(pat, pid2)
                })
            }
            _ => schema.not_allowed(),
        };

        schema
            .inner
            .borrow_mut()
            .start_att_cache
            .insert((pid, local_key, ns_key), result);
        result
    }

    fn att_value_deriv(pid: PatId, schema: &mut Schema, value: &str) -> PatId {
        let pat = schema.patt(pid);
        match pat {
            Pat::After(val_pat, cont) => {
                if Self::value_match(val_pat, schema, value) {
                    cont
                } else {
                    schema.not_allowed()
                }
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = Self::att_value_deriv(p1, schema, value);
                let c2 = Self::att_value_deriv(p2, schema, value);
                schema.choice(c1, c2)
            }
            _ => schema.not_allowed(),
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
            }
            Pat::Choice(p1, p2, _) => {
                let p1 = schema.patt(p1);
                let p2 = schema.patt(p2);
                let c1 = Self::apply_after(p1, schema, f.clone());
                let c2 = Self::apply_after(p2, schema, f);
                schema.choice(c1, c2)
            }
            Pat::NotAllowed => schema.not_allowed(),
            _ => panic!(
                "Only 'Choice', 'Interleave' or 'NotAllowed' patterns may be passed to apply_after(): {pat:?}"
            ),
        }
    }

    fn value_match(pid: PatId, schema: &mut Schema, val: &str) -> bool {
        let pat = schema.patt(pid);
        if pat.is_nullable() && is_whitespace_str(val) {
            true
        } else {
            let d = Self::text_deriv(pid, schema, val);
            schema.patt(d).is_nullable()
        }
    }

    fn start_tag_close_deriv(pid: PatId, schema: &mut Schema) -> PatId {
        {
            let inner = schema.inner.borrow();
            if let Some(&cached) = inner.start_tag_close_cache.get(&pid) {
                return cached;
            }
        }
        let pat = schema.patt(pid);
        let result = match pat {
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
        };
        schema
            .inner
            .borrow_mut()
            .start_tag_close_cache
            .insert(pid, result);
        result
    }

    // Note: the spec lists endTagDeriv as efficiently memoizable, but benchmarking showed
    // that both HashMap and Vec caches regressed performance by 7-15%. The function body
    // is only ~3 ops (RefCell borrow + array index + match), so the RefCell borrow overhead
    // of any cache lookup exceeds the savings from avoiding recomputation.
    fn end_tag_deriv(pid: PatId, schema: &mut Schema) -> PatId {
        let pat = schema.patt(pid);
        match pat {
            Pat::Choice(p1, p2, _) => {
                let c1 = Self::end_tag_deriv(p1, schema);
                let c2 = Self::end_tag_deriv(p2, schema);
                schema.choice(c1, c2)
            }
            Pat::After(p1, p2) => {
                if schema.patt(p1).is_nullable() {
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
    fn describe_nameclass(&self, nc: &NameClass, desc: &mut String) {
        describe_nameclass(nc, desc);
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
            ValidatorError::TextBufferOverflow => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Text content exceeds maximum buffer size".to_string(),
                    code: None,
                    spans: vec![],
                })
            }
            ValidatorError::TooManyPatterns => diagnostics.push(codemap_diagnostic::Diagnostic {
                level: codemap_diagnostic::Level::Error,
                message: "Schema exceeds maximum number of patterns (65535)".to_string(),
                code: None,
                spans: vec![],
            }),
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

fn parse_entities(
    pos: usize,
    text: &str,
) -> impl Iterator<Item = Result<Txt<'_>, ValidatorError<'_>>> {
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
    fn numeric_entity(pos: usize, text: &str) -> Result<Txt<'_>, ValidatorError<'_>> {
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
        if prefix == "xml" {
            return Some(StrSpan::from("http://www.w3.org/XML/1998/namespace"));
        }
        self.elements
            .iter()
            .rev()
            .find_map(|elem| elem.lookup_namespace_uri(prefix))
    }

    fn capture_ns_context(&self) -> NsContext {
        let mut default_ns = String::new();
        let mut bindings = Vec::new();
        for elem in &self.elements {
            for ns in &elem.namespaces {
                if ns.prefix.as_str().is_empty() {
                    // Default namespace (xmlns="...")
                    default_ns = ns.namespace_uri.as_str().to_string();
                } else {
                    // Prefixed namespace (xmlns:prefix="...")
                    bindings.push((
                        ns.prefix.as_str().to_string(),
                        ns.namespace_uri.as_str().to_string(),
                    ));
                }
            }
        }
        NsContext {
            default_ns,
            bindings,
        }
    }

    fn resolve_element_namespace(
        &self,
        prefix: StrSpan<'a>,
    ) -> Result<Option<StrSpan<'a>>, ValidatorError<'a>> {
        if prefix.as_str() == "" {
            // For elements, empty prefix means look up the default namespace (xmlns="...")
            Ok(self.lookup_namespace_uri(""))
        } else {
            Ok(Some(self.lookup_namespace_uri(&prefix).ok_or(
                ValidatorError::UndefinedNamespacePrefix { prefix },
            )?))
        }
    }

    fn resolve_attribute_namespace(
        &self,
        prefix: StrSpan<'a>,
    ) -> Result<Option<StrSpan<'a>>, ValidatorError<'a>> {
        if prefix.as_str() == "" {
            // Per XML Namespaces spec, unprefixed attributes have no namespace
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
        let namespace_uri = self.resolve_element_namespace(curr.prefix)?;
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
                let namespace_uri = self.resolve_attribute_namespace(unresolved.prefix)?;
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
                            ));
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
            let mut v = Validator::new(self.schema.clone(), reader).unwrap();
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

        fn valid_with_coverage(&self, xml: &str) -> super::CoverageReport {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new_with_coverage(self.schema.clone(), reader).unwrap();
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
            v.coverage_report().expect("coverage should be enabled")
        }

        fn invalid(&self, xml: &str) {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new(self.schema.clone(), reader).unwrap();
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
                        ));
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
        let mut v = Validator::new(schema, reader).unwrap();
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
                (attribute * - (foo:* | local:*)  { text })+ \
             }",
        )
        .invalid(
            "<?xml verion=\"1.0\"?>\
            <MPD xmlns=\"urn:foo\" mediaPresentationDuration=\"XPT0H11M58.998S\"></MPD>",
        );
    }

    #[test]
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

    #[test]
    fn accept_empty_except() {
        let res = check_simple(
            "start = element a { token - \"x\" }",
            "<?xml version=\"1.0\"?><a/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn coverage_element_choice() {
        let f = Fixture::correct("start = element a { element b { empty } | element c { empty } }");
        let report = f.valid_with_coverage("<a><b/></a>");
        // Element 'a' and 'b' covered, 'c' not covered
        assert!(report.covered_count() > 0);
        assert!(report.total_trackable() > 0);
        assert!(report.covered_count() < report.total_trackable());
        let uncovered: Vec<_> = report.uncovered_patterns().collect();
        assert!(
            uncovered
                .iter()
                .any(|p| p.kind == "Element" && p.name == "c"),
            "Element 'c' should be uncovered, got: {:?}",
            uncovered
                .iter()
                .map(|p| format!("{}:{}", p.kind, p.name))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn coverage_full() {
        let f = Fixture::correct("start = element a { element b { empty } }");
        let report = f.valid_with_coverage("<a><b/></a>");
        // Both elements covered
        assert_eq!(report.covered_count(), report.total_trackable());
    }

    #[test]
    fn coverage_attribute() {
        let f =
            Fixture::correct("start = element a { attribute x { text }, attribute y { text } }");
        let report = f.valid_with_coverage("<a x=\"1\" y=\"2\"/>");
        let uncovered: Vec<_> = report.uncovered_patterns().collect();
        // Both attributes should be covered
        assert!(
            !uncovered.iter().any(|p| p.kind == "Attribute"),
            "All attributes should be covered, uncovered: {:?}",
            uncovered
                .iter()
                .map(|p| format!("{}:{}", p.kind, p.name))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn coverage_text() {
        let f = Fixture::correct("start = element a { text }");
        let report = f.valid_with_coverage("<a>hello</a>");
        let covered: Vec<_> = report
            .patterns()
            .iter()
            .filter(|p| report.is_covered(p.pat_id) && p.kind == "Text")
            .collect();
        assert!(!covered.is_empty(), "Text pattern should be covered");
    }

    #[test]
    fn coverage_datatype() {
        let f = Fixture::correct("start = element a { xsd:string }");
        let report = f.valid_with_coverage("<a>hello</a>");
        let covered: Vec<_> = report
            .patterns()
            .iter()
            .filter(|p| report.is_covered(p.pat_id) && p.kind == "Datatype")
            .collect();
        assert!(!covered.is_empty(), "Datatype pattern should be covered");
    }

    #[test]
    fn coverage_merge() {
        let f = Fixture::correct("start = element a { element b { empty } | element c { empty } }");
        let mut report1 = f.valid_with_coverage("<a><b/></a>");
        let report2 = f.valid_with_coverage("<a><c/></a>");
        let before = report1.covered_count();
        report1.merge(&report2);
        // After merging, more patterns should be covered
        assert!(report1.covered_count() > before);
    }

    #[test]
    fn coverage_disabled_by_default() {
        let f = Fixture::correct("start = element a { empty }");
        let reader = xmlparser::Tokenizer::from("<a/>");
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        while let Some(i) = v.validate_next() {
            i.unwrap();
        }
        assert!(v.coverage_report().is_none());
    }
}
