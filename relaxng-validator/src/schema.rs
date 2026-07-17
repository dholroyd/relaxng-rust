use relaxng_model::model::{NameClass, PatRef};
use relaxng_model::{datatype, model};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Error
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum SchemaError {
    TooManyPatterns,
}

// ---------------------------------------------------------------------------
// Pattern types
// ---------------------------------------------------------------------------

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) struct PatId(pub(crate) u16);

/// Well-known pattern identifiers, always at fixed positions in the pattern list.
pub(crate) const PAT_NOT_ALLOWED: PatId = PatId(0);
pub(crate) const PAT_EMPTY: PatId = PatId(1);
pub(crate) const PAT_TEXT: PatId = PatId(2);

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) enum Pat {
    Choice(PatId, PatId, bool),
    Interleave(PatId, PatId, bool),
    Group(PatId, PatId, bool),
    OneOrMore(PatId, bool),
    Empty,
    Text,
    NotAllowed,
    Attribute(Box<NameClass>, PatId),
    Element(Box<NameClass>, PatId),
    Datatype(Box<datatype::Datatypes>),
    DatatypeValue(Box<datatype::DatatypeValues>),
    DatatypeExcept(Box<datatype::Datatypes>, PatId),
    List(PatId),
    After(PatId, PatId),
}

impl Pat {
    pub(crate) fn is_nullable(&self) -> bool {
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

// ---------------------------------------------------------------------------
// Schema internals
// ---------------------------------------------------------------------------

pub(crate) type NameCache = HashMap<(PatId, Box<str>, Option<Box<str>>), PatId>;

#[derive(Default)]
pub(crate) struct Inner {
    pub(crate) memo: HashMap<Pat, PatId>,
    pub(crate) patterns: Vec<Pat>,
    pub(crate) span_map: HashMap<u16, Vec<codemap::Span>>,

    // Our implementation of https://relaxng.org/jclark/derivative.html#Memoization

    // Persistent cross-call memo: (input PatId, local-name, namespace-uri) -> result PatId
    pub(crate) start_tag_open_cache: NameCache,
    // Persistent cross-call memo: input PatId -> result PatId
    pub(crate) start_tag_close_cache: HashMap<PatId, PatId>,
    // Persistent cross-call memo: input PatId -> result PatId (Vec indexed by PatId.0)
    pub(crate) mixed_text_cache: Vec<Option<PatId>>,
    // Persistent cross-call memo: (input PatId, local-name, namespace-uri) -> result PatId
    pub(crate) start_att_cache: NameCache,
}

/// Captured namespace context for resolving prefixes in document text.
#[derive(Clone)]
pub(crate) struct NsContext {
    pub(crate) default_ns: String,
    pub(crate) bindings: Vec<(String, String)>,
}
impl NsContext {
    pub(crate) fn lookup(&self, prefix: &str) -> Option<String> {
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

#[derive(Default)]
pub(crate) struct Schema {
    pub(crate) inner: RefCell<Inner>,
    pub(crate) coverage: Option<Box<[u64]>>,
    pub(crate) compile_time_count: u16,
    /// Namespace context for QName resolution during text_deriv.
    /// Lazily populated: rebuilt on demand when `ns_context_dirty` is set.
    pub(crate) ns_context: Option<NsContext>,
    /// When true, `ns_context` needs rebuilding before use.
    pub(crate) ns_context_dirty: bool,
}

impl Schema {
    pub(crate) fn push(&self, p: Pat) -> PatId {
        let mut inner = self.inner.borrow_mut();
        if inner.patterns.len() > 0xffff {
            return PAT_NOT_ALLOWED;
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
    pub(crate) fn mark_covered(&mut self, id: PatId) {
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

    pub(crate) fn choice(&self, left: PatId, right: PatId) -> PatId {
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

    pub(crate) fn interleave(&self, left: PatId, right: PatId) -> PatId {
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

    pub(crate) fn group(&self, left: PatId, right: PatId) -> PatId {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => self.not_allowed(),
            (_, Pat::NotAllowed) => self.not_allowed(),
            (Pat::Empty, _) => right,
            (_, Pat::Empty) => left,
            (l, r) => self.push(Pat::Group(left, right, l.is_nullable() && r.is_nullable())),
        }
    }

    pub(crate) fn after(&self, p1: PatId, p2: PatId) -> PatId {
        match (self.patt(p1), self.patt(p2)) {
            (Pat::NotAllowed, _) | (_, Pat::NotAllowed) => self.not_allowed(),
            (_, _) => self.push(Pat::After(p1, p2)),
        }
    }

    pub(crate) fn empty(&self) -> PatId {
        PAT_EMPTY
    }
    pub(crate) fn text(&self) -> PatId {
        PAT_TEXT
    }
    pub(crate) fn not_allowed(&self) -> PatId {
        PAT_NOT_ALLOWED
    }
    pub(crate) fn one_or_more(&self, pattern: PatId) -> PatId {
        let p = self.patt(pattern);
        self.push(Pat::OneOrMore(pattern, p.is_nullable()))
    }
    pub(crate) fn list(&self, p: PatId) -> PatId {
        self.push(Pat::List(p))
    }
    pub(crate) fn patt(&self, id: PatId) -> Pat {
        self.inner.borrow().patterns[id.0 as usize].clone()
    }

    pub(crate) fn check_choice(
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

    pub(crate) fn check_choices(&self, id: PatId, seen: &mut Vec<PatId>) {
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
    pub(crate) fn d(&self, pat: PatId) -> Result<(), io::Error> {
        let mut o = io::stdout();
        self.dumpy(pat, &mut o)
    }

    pub(crate) fn dumpy<W: io::Write>(&self, pat: PatId, w: &mut W) -> Result<(), io::Error> {
        let mut seen = HashSet::new();
        self.dumpy_dump(0, pat, w, &mut seen)
    }

    pub(crate) fn dumpy_dump<W: io::Write>(
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

// ---------------------------------------------------------------------------
// Schema builder (compile-time)
// ---------------------------------------------------------------------------

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
        let mut memo = HashMap::new();
        let patterns = vec![
            Some(Pat::NotAllowed), // PAT_NOT_ALLOWED = 0
            Some(Pat::Empty),      // PAT_EMPTY = 1
            Some(Pat::Text),       // PAT_TEXT = 2
        ];
        memo.insert(Pat::NotAllowed, PAT_NOT_ALLOWED);
        memo.insert(Pat::Empty, PAT_EMPTY);
        memo.insert(Pat::Text, PAT_TEXT);
        SchemaBuilder {
            memo,
            patterns,
            refs: HashMap::new(),
            span_map: HashMap::new(),
            deferred: Vec::new(),
            overflowed: false,
        }
    }

    fn push(&mut self, p: Pat) -> PatId {
        if self.patterns.len() > 0xffff {
            self.overflowed = true;
            return PAT_NOT_ALLOWED;
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
        PAT_EMPTY
    }

    fn text(&mut self) -> PatId {
        PAT_TEXT
    }

    fn not_allowed(&mut self) -> PatId {
        PAT_NOT_ALLOWED
    }

    fn attribute(&mut self, name: NameClass, p: PatId) -> PatId {
        self.push(Pat::Attribute(Box::new(name), p))
    }

    fn element(&mut self, name: NameClass, p: PatId) -> PatId {
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

// ---------------------------------------------------------------------------
// Schema compilation
// ---------------------------------------------------------------------------

fn compile_pattern(s: &mut SchemaBuilder, p: &model::Pattern) -> PatId {
    match p {
        model::Pattern::Choice(v, _) => {
            let mut iter = v.iter().rev();
            let mut right = compile_pattern(s, iter.next().unwrap());
            for left in iter {
                let l = compile_pattern(s, left);
                right = s.choice(l, right);
            }
            right
        }
        model::Pattern::Interleave(v, _) => {
            let mut iter = v.iter().rev();
            let mut right = compile_pattern(s, iter.next().unwrap());
            for left in iter {
                let l = compile_pattern(s, left);
                right = s.interleave(l, right);
            }
            right
        }
        model::Pattern::Group(v, _) => {
            let mut iter = v.iter().rev();
            let mut right = compile_pattern(s, iter.next().unwrap());
            for left in iter {
                let l = compile_pattern(s, left);
                right = s.group(l, right);
            }
            right
        }
        model::Pattern::Mixed(p, _) => {
            let inner = compile_pattern(s, p);
            s.mixed(inner)
        }
        model::Pattern::Empty(_) => s.empty(),
        model::Pattern::Text(span) => {
            let id = s.text();
            if let Some(span) = span {
                s.record_source_span(id, *span);
            }
            id
        }
        model::Pattern::NotAllowed(_) => s.not_allowed(),
        model::Pattern::Optional(p, _) => {
            let inner = compile_pattern(s, p);
            let empty = s.empty();
            s.choice(inner, empty)
        }
        model::Pattern::ZeroOrMore(p, _) => {
            let inner = compile_pattern(s, p);
            let one = s.one_or_more(inner);
            let empty = s.empty();
            s.choice(one, empty)
        }
        model::Pattern::OneOrMore(p, _) => {
            let inner = compile_pattern(s, p);
            s.one_or_more(inner)
        }
        model::Pattern::Attribute(name, p, span, _) => {
            let content = compile_pattern(s, p);
            let id = s.attribute(name.clone(), content);
            if let Some(span) = span {
                s.record_source_span(id, *span);
            }
            id
        }
        model::Pattern::Element(name, p, span, _) => {
            let content = compile_pattern(s, p);
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
                    let id = compile_pattern(s, thing.pattern());
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
            let except_id = except.as_ref().map(|e| compile_pattern(s, e));
            let id = s.datatype_name(datatype.clone(), except_id);
            if let Some(span) = span {
                s.record_source_span(id, *span);
            }
            id
        }
        model::Pattern::List(p, _) => {
            let inner = compile_pattern(s, p);
            s.list(inner)
        }
    }
}

/// Compile a model pattern into a Schema and its start PatId.
pub(crate) fn build_schema(
    model: &Rc<RefCell<Option<model::DefineRule>>>,
) -> Result<(Schema, PatId), SchemaError> {
    let mut builder = SchemaBuilder::new();
    let start = compile_pattern(
        &mut builder,
        Rc::as_ref(model).borrow().as_ref().unwrap().pattern(),
    );
    if builder.overflowed {
        return Err(SchemaError::TooManyPatterns);
    }
    let schema = Schema {
        inner: RefCell::new(builder.finalize()),
        ..Default::default()
    };
    Ok((schema, start))
}
