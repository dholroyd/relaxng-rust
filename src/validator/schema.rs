use crate::model::{NameClass, PatRef};

use crate::model::{self, datatype};
use crate::validator::nameclass::{NameKey, NameMatcher, SchemaNames};
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::rc::Rc;

pub(crate) const MAX_PATTERNS: usize = 1 << 16;

/// Validation needed a new pattern when the pattern table was already full.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Overflow;

#[derive(Debug)]
pub enum SchemaError {
    TooManyPatterns,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) struct PatId(pub(crate) u16);

/// Well-known pattern identifiers, always at fixed positions in the pattern list.
pub(crate) const PAT_NOT_ALLOWED: PatId = PatId(0);
pub(crate) const PAT_EMPTY: PatId = PatId(1);
pub(crate) const PAT_TEXT: PatId = PatId(2);

/// Index into `Schema::names`.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) struct NameClassId(u16);

/// Index into `Schema::datatypes`.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) struct DatatypeId(u16);

/// Index into `Schema::datatype_values`.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) struct DatatypeValueId(u16);

/// Build-time deduplication of values, assigning dense `u16` ids.  [`Interner::into_table`]
/// yields the values as a slice indexed by those ids.
struct Interner<T> {
    ids: HashMap<T, u16>,
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Interner {
            ids: HashMap::new(),
        }
    }
}

impl<T: Hash + Eq> Interner<T> {
    /// Returns `None` if the `u16` id space is exhausted.
    fn intern(&mut self, value: T) -> Option<u16> {
        if let Some(&id) = self.ids.get(&value) {
            return Some(id);
        }
        let id = u16::try_from(self.ids.len()).ok()?;
        self.ids.insert(value, id);
        Some(id)
    }

    fn into_table(self) -> Box<[T]> {
        let mut slots: Vec<Option<T>> = (0..self.ids.len()).map(|_| None).collect();
        for (value, id) in self.ids {
            slots[id as usize] = Some(value);
        }
        slots.into_iter().map(|v| v.unwrap()).collect()
    }
}

/// Properties of a composite pattern that follow from its operands, computed when it is built so
/// that reading them costs nothing.  Leaf patterns need no flags, as their variant decides them.
/// While a schema is being compiled the flags are left unset, and [`compute_flags`] fills them
/// in once every pattern is known.
///
/// Every flag is a function of the operands, so including the flags in a `Pat`'s equality and
/// hash does not stop equal patterns from being shared.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug, Default)]
pub(crate) struct PatFlags(u8);

impl PatFlags {
    /// The pattern matches an empty sequence.
    const NULLABLE: u8 = 1 << 0;
    /// The pattern may match data (a datatype, value or list), so text matched against it must be
    /// checked by value, not just as some text.  This is the RELAX NG spec's "simple" content
    /// type (section 7.2), which outranks the others when combined: the restrictions forbid
    /// grouping data with elements, so a pattern that may match data anywhere has simple content.
    /// Jing calls such content "text typed".
    const TEXT_TYPED: u8 = 1 << 1;

    const fn new(nullable: bool, text_typed: bool) -> PatFlags {
        PatFlags((nullable as u8 * Self::NULLABLE) | (text_typed as u8 * Self::TEXT_TYPED))
    }

    fn nullable(self) -> bool {
        self.0 & Self::NULLABLE != 0
    }

    fn text_typed(self) -> bool {
        self.0 & Self::TEXT_TYPED != 0
    }

    /// The flags of `a | b`.
    pub(crate) fn choice(a: PatFlags, b: PatFlags) -> PatFlags {
        PatFlags::new(
            a.nullable() || b.nullable(),
            a.text_typed() || b.text_typed(),
        )
    }

    /// The flags of `a, b` and of `a & b`.
    pub(crate) fn group(a: PatFlags, b: PatFlags) -> PatFlags {
        PatFlags::new(
            a.nullable() && b.nullable(),
            a.text_typed() || b.text_typed(),
        )
    }

    /// The flags of `After(a, _)`, which is never nullable since the end tag is still to come,
    /// and whose content is `a`.
    pub(crate) fn after(a: PatFlags) -> PatFlags {
        PatFlags::new(false, a.text_typed())
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub(crate) enum Pat {
    Choice(PatId, PatId, PatFlags),
    Interleave(PatId, PatId, PatFlags),
    Group(PatId, PatId, PatFlags),
    /// Has the same flags as its operand.
    OneOrMore(PatId, PatFlags),
    Empty,
    Text,
    NotAllowed,
    Attribute(NameClassId, PatId),
    Element(NameClassId, PatId),
    Datatype(DatatypeId),
    DatatypeValue(DatatypeValueId),
    DatatypeExcept(DatatypeId, PatId),
    List(PatId),
    After(PatId, PatId, PatFlags),
}

// The pattern table and the hash-consing memo hold a lot of these, so keep the flags from
// growing them.
const _: () = assert!(std::mem::size_of::<Pat>() == 6);

impl Pat {
    pub(crate) fn flags(&self) -> PatFlags {
        match self {
            Pat::Choice(_, _, flags)
            | Pat::Interleave(_, _, flags)
            | Pat::Group(_, _, flags)
            | Pat::OneOrMore(_, flags)
            | Pat::After(_, _, flags) => *flags,
            Pat::Empty | Pat::Text => PatFlags::new(true, false),
            // An attribute's value is not part of the element's content, so it is not text typed
            Pat::NotAllowed | Pat::Attribute(_, _) | Pat::Element(_, _) => {
                PatFlags::new(false, false)
            }
            Pat::Datatype(_) | Pat::DatatypeValue(_) | Pat::DatatypeExcept(_, _) | Pat::List(_) => {
                PatFlags::new(false, true)
            }
        }
    }

    pub(crate) fn is_nullable(&self) -> bool {
        self.flags().nullable()
    }

    /// Whether text content matched against this pattern must be checked by its value; see
    /// [`PatFlags::TEXT_TYPED`].
    pub(crate) fn is_text_typed(&self) -> bool {
        self.flags().text_typed()
    }
}

// ---------------------------------------------------------------------------
// Schema internals
// ---------------------------------------------------------------------------

pub(crate) type NameCache = FnvHashMap<(PatId, NameKey), PatId>;

// TODO: think about keeping the compile-time and run-time state separate, so that the pattern
//       list etc in the schema can be immutable, meaning that we don't need to track compiled_len
//       in order to reset this state if we want to reuse an existing Schema instance to perform
//       a new validation, or maybe even share it between multiple concurrent validations.
#[derive(Default)]
pub(crate) struct Inner {
    pub(crate) memo: HashMap<Pat, PatId>,
    pub(crate) patterns: Vec<Pat>,
    pub(crate) span_map: HashMap<u16, Vec<codemap::Span>>,
    /// The number of patterns the compiled schema itself has; those created by derivation
    /// during validation come after them.
    pub(crate) compiled_len: usize,

    // Our implementation of https://relaxng.org/jclark/derivative.html#Memoization

    // Persistent cross-call memo: (input PatId, name) -> result PatId
    pub(crate) start_tag_open_cache: NameCache,
    // Persistent cross-call memo: input PatId -> result PatId
    pub(crate) start_tag_close_cache: HashMap<PatId, PatId>,
    // Persistent cross-call memo: input PatId -> result PatId (Vec indexed by PatId.0)
    pub(crate) mixed_text_cache: Vec<Option<PatId>>,
    // Persistent cross-call memo: (input PatId, name) -> result PatId
    pub(crate) start_att_cache: NameCache,
    // Persistent cross-call memo for whitespace-only (including empty) text: input PatId ->
    // result PatId (Vec indexed by PatId.0).  Only holds results that are the same for every
    // whitespace-only string, i.e. that did not consult a datatype, so it is keyed on the
    // pattern alone and bounded by the pattern table.
    pub(crate) whitespace_text_cache: Vec<Option<PatId>>,
}

#[derive(Default)]
pub(crate) struct Schema {
    pub(crate) inner: RefCell<Inner>,
    // Payloads referenced by `Pat` variants, created while the schema is compiled and never
    // changed afterwards.  Entries are interned, so equal payloads share an id.
    names: Box<[NameClass]>,
    datatypes: Box<[datatype::Datatypes]>,
    datatype_values: Box<[datatype::DatatypeValues]>,

    pub(crate) coverage: Option<Box<[u64]>>,
    pub(crate) compile_time_count: u16,
    /// `names` compiled for matching, at the same indexes.
    matchers: Box<[NameMatcher]>,
    /// The names that `matchers` mention.
    pub(crate) schema_names: SchemaNames,
}

impl Schema {
    pub(crate) fn name_class(&self, id: NameClassId) -> &NameClass {
        &self.names[id.0 as usize]
    }
    pub(crate) fn name_matcher(&self, id: NameClassId) -> &NameMatcher {
        &self.matchers[id.0 as usize]
    }
    pub(crate) fn datatype(&self, id: DatatypeId) -> &datatype::Datatypes {
        &self.datatypes[id.0 as usize]
    }
    pub(crate) fn datatype_value(&self, id: DatatypeValueId) -> &datatype::DatatypeValues {
        &self.datatype_values[id.0 as usize]
    }

    pub(crate) fn push(&self, p: Pat) -> Result<PatId, Overflow> {
        let mut inner = self.inner.borrow_mut();
        if let Some(id) = inner.memo.get(&p) {
            Ok(*id)
        } else if inner.patterns.len() >= MAX_PATTERNS {
            Err(Overflow)
        } else {
            let id = PatId(inner.patterns.len() as u16);
            inner.memo.insert(p, id);
            inner.patterns.push(p);
            Ok(id)
        }
    }

    /// Whether the pattern table is full, so that deriving anything new will overflow.
    pub(crate) fn is_full(&self) -> bool {
        self.inner.borrow().patterns.len() >= MAX_PATTERNS
    }

    /// Discards every pattern created by derivation, and everything cached about them, to make
    /// room for more.  Any `PatId` created during validation is invalid afterwards.
    pub(crate) fn discard_derived_patterns(&self) {
        let mut inner = self.inner.borrow_mut();
        let Inner {
            mut memo,
            mut patterns,
            span_map,
            compiled_len,
            ..
        } = std::mem::take(&mut *inner);
        patterns.truncate(compiled_len);
        memo.retain(|_, id| (id.0 as usize) < compiled_len);
        // Every cache is rebuilt empty, including any added later.
        *inner = Inner {
            memo,
            patterns,
            span_map,
            compiled_len,
            ..Default::default()
        };
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

    /// Builds `left | right`, normalised so that no leaf appears twice and no two `After` leaves
    /// have the same first operand: `After(p, x) | After(p, y)` becomes `After(p, x | y)`.
    ///
    /// Duplicate leaves arise when the same sub-pattern is reached by several derivative paths
    /// (e.g. interleaved alternatives sharing a common attribute), and would otherwise accumulate.
    /// Without merging `After`s, a recursive element that can match at two places in its parent,
    /// followed by different things, doubles the number of leaves at each level of nesting, and
    /// validation time grows exponentially with depth.  With it, the top level of a state has at
    /// most one leaf per distinct element content, and each continuation is bounded in the same
    /// way, so the cost of each event depends only on the schema.
    pub(crate) fn choice(&self, left: PatId, right: PatId) -> Result<PatId, Overflow> {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => return Ok(right),
            (_, Pat::NotAllowed) => return Ok(left),
            _ if left == right => return Ok(left),
            (Pat::Choice(..), _) | (_, Pat::Choice(..)) => {}
            (Pat::After(p, _, _), Pat::After(q, _, _)) if p == q => {}
            // Two distinct leaves, so there is nothing to normalise.
            (l, r) => {
                return self.push(Pat::Choice(
                    left,
                    right,
                    PatFlags::choice(l.flags(), r.flags()),
                ));
            }
        }
        let mut leaves = Vec::new();
        {
            let inner = self.inner.borrow();
            Self::collect_leaves(left, &inner, &mut leaves);
            Self::collect_leaves(right, &inner, &mut leaves);
        }
        let mut seen = HashSet::new();
        // The index in `merged` of the `After` leaf with each first operand.
        let mut by_head: HashMap<PatId, usize> = HashMap::new();
        let mut merged: Vec<PatId> = Vec::with_capacity(leaves.len());
        for leaf in leaves {
            if !seen.insert(leaf) {
                continue;
            }
            if let Pat::After(p, k, _) = self.patt(leaf) {
                if let Some(&i) = by_head.get(&p) {
                    let Pat::After(_, k0, _) = self.patt(merged[i]) else {
                        unreachable!("by_head only indexes After leaves")
                    };
                    let k = self.choice(k0, k)?;
                    merged[i] = self.after(p, k)?;
                    continue;
                }
                by_head.insert(p, merged.len());
            }
            merged.push(leaf);
        }
        let mut merged = merged.into_iter();
        let first = merged.next().expect("a choice has at least one leaf");
        merged.try_fold(first, |acc, r| {
            let flags = PatFlags::choice(self.patt(acc).flags(), self.patt(r).flags());
            self.push(Pat::Choice(acc, r, flags))
        })
    }

    // Traverses a Choice tree and appends every leaf PatId to `out`, left to right.
    fn collect_leaves(id: PatId, inner: &Inner, out: &mut Vec<PatId>) {
        match &inner.patterns[id.0 as usize] {
            Pat::Choice(l, r, _) => {
                Self::collect_leaves(*l, inner, out);
                Self::collect_leaves(*r, inner, out);
            }
            _ => out.push(id),
        }
    }

    pub(crate) fn interleave(&self, left: PatId, right: PatId) -> Result<PatId, Overflow> {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => Ok(self.not_allowed()),
            (_, Pat::NotAllowed) => Ok(self.not_allowed()),
            (Pat::Empty, _) => Ok(right),
            (_, Pat::Empty) => Ok(left),
            (l, r) => self.push(Pat::Interleave(
                left,
                right,
                PatFlags::group(l.flags(), r.flags()),
            )),
        }
    }

    pub(crate) fn group(&self, left: PatId, right: PatId) -> Result<PatId, Overflow> {
        match (self.patt(left), self.patt(right)) {
            (Pat::NotAllowed, _) => Ok(self.not_allowed()),
            (_, Pat::NotAllowed) => Ok(self.not_allowed()),
            (Pat::Empty, _) => Ok(right),
            (_, Pat::Empty) => Ok(left),
            (l, r) => self.push(Pat::Group(
                left,
                right,
                PatFlags::group(l.flags(), r.flags()),
            )),
        }
    }

    pub(crate) fn after(&self, p1: PatId, p2: PatId) -> Result<PatId, Overflow> {
        match (self.patt(p1), self.patt(p2)) {
            (Pat::NotAllowed, _) | (_, Pat::NotAllowed) => Ok(self.not_allowed()),
            (a, _) => self.push(Pat::After(p1, p2, PatFlags::after(a.flags()))),
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
    pub(crate) fn one_or_more(&self, pattern: PatId) -> Result<PatId, Overflow> {
        match self.patt(pattern) {
            Pat::NotAllowed => Ok(self.not_allowed()),
            p => self.push(Pat::OneOrMore(pattern, p.flags())),
        }
    }
    pub(crate) fn patt(&self, id: PatId) -> Pat {
        self.inner.borrow().patterns[id.0 as usize]
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
                    writeln!(w, "Att{}({:?}=", pat.0, self.name_class(nc))?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Element(nc, p) => {
                    writeln!(w, "Elem{}({:?}=", pat.0, self.name_class(nc))?;
                    self.dumpy_dump(depth + 1, p, w, seen)?;
                    for _ in 0..depth {
                        w.write_all(b"  ")?;
                    }
                    writeln!(w, ")")
                }
                Pat::Datatype(dt) => {
                    writeln!(w, "{:?}{}", self.datatype(dt), pat.0)
                }
                Pat::DatatypeValue(dt) => {
                    writeln!(w, "{:?}{}", self.datatype_value(dt), pat.0)
                }
                Pat::DatatypeExcept(dt, p) => {
                    writeln!(w, "{:?}{}-", self.datatype(dt), pat.0)?;
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
                Pat::After(p1, p2, _) => {
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
                    writeln!(w, "{:?}{}!", self.datatype(dt), pat.0)
                }
                Pat::DatatypeValue(dt) => {
                    writeln!(w, "{:?}{}!", self.datatype_value(dt), pat.0)
                }
                Pat::DatatypeExcept(dt, _p) => {
                    writeln!(w, "{:?}{}!", self.datatype(dt), pat.0)
                }
                Pat::List(_p) => {
                    writeln!(w, "List{}!", pat.0)
                }
                Pat::After(_p1, _p2, _) => {
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
///
/// Composite patterns are built with default flags, since an operand may be a slot not yet
/// filled; `finalize()` computes them all once every slot is resolved.
struct SchemaBuilder {
    memo: HashMap<Pat, PatId>,
    patterns: Vec<Option<Pat>>,
    names: Interner<NameClass>,
    datatypes: Interner<datatype::Datatypes>,
    datatype_values: Interner<datatype::DatatypeValues>,
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
            names: Interner::default(),
            datatypes: Interner::default(),
            datatype_values: Interner::default(),
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
            self.memo.insert(p, id);
            self.patterns.push(Some(p));
            id
        }
    }

    fn try_patt(&self, id: PatId) -> Option<&Pat> {
        self.patterns[id.0 as usize].as_ref()
    }

    fn record_source_span(&mut self, id: PatId, span: codemap::Span) {
        self.span_map.entry(id.0).or_default().push(span);
    }

    fn choice(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) => right,
            (_, Some(Pat::NotAllowed)) => left,
            (Some(_), Some(_)) if left == right => left,
            _ => self.push(Pat::Choice(left, right, PatFlags::default())),
        }
    }

    fn interleave(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) | (_, Some(Pat::NotAllowed)) => self.not_allowed(),
            (Some(Pat::Empty), _) => right,
            (_, Some(Pat::Empty)) => left,
            _ => self.push(Pat::Interleave(left, right, PatFlags::default())),
        }
    }

    fn group(&mut self, left: PatId, right: PatId) -> PatId {
        match (self.try_patt(left), self.try_patt(right)) {
            (Some(Pat::NotAllowed), _) | (_, Some(Pat::NotAllowed)) => self.not_allowed(),
            (Some(Pat::Empty), _) => right,
            (_, Some(Pat::Empty)) => left,
            _ => self.push(Pat::Group(left, right, PatFlags::default())),
        }
    }

    fn one_or_more(&mut self, pattern: PatId) -> PatId {
        if let Some(Pat::NotAllowed) = self.try_patt(pattern) {
            return self.not_allowed();
        }
        self.push(Pat::OneOrMore(pattern, PatFlags::default()))
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

    // Interning fails only if a table outgrows the `u16` id space; flag that like pattern overflow.
    fn attribute(&mut self, name: NameClass, p: PatId) -> PatId {
        let Some(name) = self.names.intern(name) else {
            self.overflowed = true;
            return PAT_NOT_ALLOWED;
        };
        self.push(Pat::Attribute(NameClassId(name), p))
    }

    fn element(&mut self, name: NameClass, p: PatId) -> PatId {
        let Some(name) = self.names.intern(name) else {
            self.overflowed = true;
            return PAT_NOT_ALLOWED;
        };
        self.push(Pat::Element(NameClassId(name), p))
    }

    fn datatype_value(&mut self, dt: datatype::DatatypeValues) -> PatId {
        let Some(dt) = self.datatype_values.intern(dt) else {
            self.overflowed = true;
            return PAT_NOT_ALLOWED;
        };
        self.push(Pat::DatatypeValue(DatatypeValueId(dt)))
    }

    fn datatype_name(&mut self, dt: datatype::Datatypes, except: Option<PatId>) -> PatId {
        let Some(dt) = self.datatypes.intern(dt) else {
            self.overflowed = true;
            return PAT_NOT_ALLOWED;
        };
        let dt = DatatypeId(dt);
        if let Some(except) = except {
            self.push(Pat::DatatypeExcept(dt, except))
        } else {
            self.push(Pat::Datatype(dt))
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
        match self.patterns[id.0 as usize] {
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

    fn finalize(mut self) -> Schema {
        // Resolve deferred placeholders from mutual recursion.  Iterate until
        // no more progress is made to handle chains (A→B→C).
        let mut made_progress = true;
        while made_progress {
            made_progress = false;
            self.deferred.retain(|&(placeholder, target)| {
                if let Some(pat) = self.patterns[target.0 as usize] {
                    self.patterns[placeholder.0 as usize] = Some(pat);
                    made_progress = true;
                    false // resolved, remove from list
                } else {
                    true // still unresolved, keep
                }
            });
        }
        let mut patterns: Vec<Pat> = self
            .patterns
            .into_iter()
            .enumerate()
            .map(|(i, p)| p.unwrap_or_else(|| panic!("unresolved slot at index {}", i)))
            .collect();
        compute_flags(&mut patterns);
        // The flags are part of each key, so rekey the memo by the completed patterns.  Each
        // id keeps its place; a resolved `Ref` slot (not itself in the memo) holds a copy of its
        // target and so gets the same flags, keeping its lookup working.
        let memo = self
            .memo
            .into_values()
            .map(|id| (patterns[id.0 as usize], id))
            .collect();
        let inner = Inner {
            memo,
            compiled_len: patterns.len(),
            patterns,
            span_map: self.span_map,
            ..Default::default()
        };
        let names = self.names.into_table();
        let mut schema_names = SchemaNames::default();
        let matchers = names
            .iter()
            .map(|nc| NameMatcher::compile(nc, &mut schema_names))
            .collect();
        Schema {
            inner: RefCell::new(inner),
            names,
            matchers,
            schema_names,
            datatypes: self.datatypes.into_table(),
            datatype_values: self.datatype_values.into_table(),
            ..Default::default()
        }
    }
}

/// Sets every composite pattern's flags from its operands' flags.
///
/// The builder cannot compute flags as it goes, since an operand may be a `Ref` slot not yet
/// resolved (a definition reached again from within its own body), so it leaves them unset.
/// Every slot is resolved by now, so computing them here makes every pattern's flags right
/// before any document is validated, and patterns derived later inherit correct flags from
/// their operands.
///
/// Operands are visited before the patterns that use them.  The graph is acyclic apart from
/// paths through `Element`, whose flags do not depend on its content, so flags are only read
/// across `Element` boundaries without being followed.  An explicit stack keeps deep schemas
/// from overflowing the call stack.
fn compute_flags(patterns: &mut [Pat]) {
    fn flag_operands(p: &Pat) -> (Option<PatId>, Option<PatId>) {
        match *p {
            Pat::Choice(a, b, _) | Pat::Interleave(a, b, _) | Pat::Group(a, b, _) => {
                (Some(a), Some(b))
            }
            Pat::OneOrMore(a, _) | Pat::After(a, _, _) => (Some(a), None),
            _ => (None, None),
        }
    }

    const UNVISITED: u8 = 0;
    const VISITING: u8 = 1;
    const DONE: u8 = 2;
    let mut state = vec![UNVISITED; patterns.len()];
    let mut stack = Vec::new();
    for root in 0..patterns.len() {
        if state[root] != UNVISITED {
            continue;
        }
        stack.push(root);
        while let Some(&i) = stack.last() {
            match state[i] {
                UNVISITED => {
                    state[i] = VISITING;
                    let (a, b) = flag_operands(&patterns[i]);
                    for op in [a, b].into_iter().flatten() {
                        if state[op.0 as usize] == UNVISITED {
                            stack.push(op.0 as usize);
                        }
                    }
                }
                VISITING => {
                    // All operands are now done (or, were there a cycle, still in progress
                    // further down the stack and read as they stand).
                    let f = |id: PatId| patterns[id.0 as usize].flags();
                    patterns[i] = match patterns[i] {
                        Pat::Choice(a, b, _) => Pat::Choice(a, b, PatFlags::choice(f(a), f(b))),
                        Pat::Interleave(a, b, _) => {
                            Pat::Interleave(a, b, PatFlags::group(f(a), f(b)))
                        }
                        Pat::Group(a, b, _) => Pat::Group(a, b, PatFlags::group(f(a), f(b))),
                        Pat::OneOrMore(a, _) => Pat::OneOrMore(a, f(a)),
                        Pat::After(a, b, _) => Pat::After(a, b, PatFlags::after(f(a))),
                        p => p,
                    };
                    state[i] = DONE;
                    stack.pop();
                }
                _ => {
                    // Pushed again by another user before its first visit completed
                    stack.pop();
                }
            }
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
            s.record_source_span(id, *span);
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
            s.record_source_span(id, *span);
            id
        }
        model::Pattern::Element(name, p, span, _) => {
            let content = compile_pattern(s, p);
            let id = s.element(name.clone(), content);
            s.record_source_span(id, *span);
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
            s.record_source_span(id, *span);
            id
        }
        model::Pattern::DatatypeName {
            datatype,
            except,
            span,
        } => {
            let except_id = except.as_ref().map(|e| compile_pattern(s, e));
            let id = s.datatype_name(datatype.clone(), except_id);
            s.record_source_span(id, *span);
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
    Ok((builder.finalize(), start))
}
