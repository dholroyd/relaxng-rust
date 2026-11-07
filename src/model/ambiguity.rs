//! Ambiguity detection for RELAX NG grammars.
//!
//! Implements the algorithm from Kawaguchi's "Ambiguity Detection Of RELAX Grammars" (2001).
//! A grammar is ambiguous when there exists a valid XML instance that can be interpreted
//! (definitions mapped to elements) in more than one way.

use crate::model::restrictions;
use crate::model::{NameClass, Pattern};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;

/// The ambiguity check was abandoned because an automaton built from the grammar
/// would have more states than a `StateId` can number.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StateSpaceOverflow;

impl fmt::Display for StateSpaceOverflow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "content-model automaton would exceed {} states",
            StateId::MAX
        )
    }
}

impl std::error::Error for StateSpaceOverflow {}

fn add_states(a: StateId, b: StateId) -> Result<StateId, StateSpaceOverflow> {
    a.checked_add(b).ok_or(StateSpaceOverflow)
}

fn mul_states(a: StateId, b: StateId) -> Result<StateId, StateSpaceOverflow> {
    a.checked_mul(b).ok_or(StateSpaceOverflow)
}

/// Unique identifier for an Element pattern in the grammar.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LabelId(u32);

/// NFA state identifier.
type StateId = u32;

/// An epsilon-free NFA in pieces: per-state labelled transitions, and per-state
/// acceptance. State 0 is the start state.
type EpsilonFree = (Vec<Vec<(LabelId, StateId)>>, Vec<bool>);

/// An NFA over a LabelId alphabet (with epsilon transitions).
/// Start state is always state 0.
#[derive(Debug, Clone)]
struct Nfa {
    num_states: u32,
    /// transitions[state] = vec of (label_or_epsilon, target_state)
    transitions: Vec<Vec<(Option<LabelId>, StateId)>>,
    /// accept[state] = true if state is accepting
    accept: Vec<bool>,
}

impl Nfa {
    /// Accepts only the empty sequence (single state that is both start and accepting).
    fn new_empty() -> Self {
        Nfa {
            num_states: 1,
            transitions: vec![vec![]],
            accept: vec![true],
        }
    }

    /// Accepts exactly the one-element sequence `[label]`.
    fn new_single(label: LabelId) -> Self {
        Nfa {
            num_states: 2,
            transitions: vec![vec![(Some(label), 1)], vec![]],
            accept: vec![false, true],
        }
    }

    /// Accepts nothing (start state with no path to any accepting state).
    fn new_dead() -> Self {
        Nfa {
            num_states: 1,
            transitions: vec![vec![]],
            accept: vec![false],
        }
    }

    /// Sequential composition: accepts sequences `ab` where `a` is accepted by `self`
    /// and `b` is accepted by `other`.
    fn concatenate(mut self, other: Nfa) -> Result<Nfa, StateSpaceOverflow> {
        // Checked up front; every renumbered state below is less than this total.
        let num_states = add_states(self.num_states, other.num_states)?;
        let offset = self.num_states;
        // Add other's states (renumbered)
        for trans in &other.transitions {
            self.transitions.push(
                trans
                    .iter()
                    .map(|(label, target)| (*label, target + offset))
                    .collect(),
            );
        }
        // Epsilon from self's accept states to other's start
        for acc in 0..self.num_states {
            if self.accept[acc as usize] {
                self.transitions[acc as usize].push((None, offset));
            }
        }
        self.num_states = num_states;
        // Accept states are now only other's accept states
        let mut new_accept = vec![false; self.num_states as usize];
        for (s, &is_acc) in other.accept.iter().enumerate() {
            if is_acc {
                new_accept[(s as u32 + offset) as usize] = true;
            }
        }
        self.accept = new_accept;
        Ok(self)
    }

    /// Choice: accepts any sequence accepted by any of the given NFAs.
    fn alternate(nfas: Vec<Nfa>) -> Result<Nfa, StateSpaceOverflow> {
        if nfas.is_empty() {
            return Ok(Nfa::new_dead());
        }
        if nfas.len() == 1 {
            return Ok(nfas.into_iter().next().unwrap());
        }
        // Checked up front (new start state plus every sub-NFA); the running
        // offsets below never exceed this total.
        let mut total: StateId = 1;
        for nfa in &nfas {
            total = add_states(total, nfa.num_states)?;
        }
        // New start state (state 0), epsilon to each sub-NFA's start
        let mut result = Nfa {
            num_states: 1,
            transitions: vec![vec![]],
            accept: vec![false],
        };
        for nfa in nfas {
            let offset = result.num_states;
            for trans in &nfa.transitions {
                result.transitions.push(
                    trans
                        .iter()
                        .map(|(label, target)| (*label, target + offset))
                        .collect(),
                );
            }
            // Epsilon from new start to sub-NFA's start
            result.transitions[0].push((None, offset));
            // Merge accept states
            result
                .accept
                .resize(result.num_states as usize + nfa.num_states as usize, false);
            for (s, &is_acc) in nfa.accept.iter().enumerate() {
                if is_acc {
                    result.accept[(s as u32 + offset) as usize] = true;
                }
            }
            result.num_states += nfa.num_states;
        }
        debug_assert_eq!(result.num_states, total);
        Ok(result)
    }

    /// Kleene plus: one or more repetitions.
    fn one_or_more(mut self) -> Nfa {
        // Epsilon from each accept state back to start
        for acc in 0..self.num_states {
            if self.accept[acc as usize] {
                self.transitions[acc as usize].push((None, 0));
            }
        }
        self
    }

    /// Kleene star: zero or more repetitions.
    fn zero_or_more(self) -> Result<Nfa, StateSpaceOverflow> {
        self.one_or_more().optional()
    }

    /// Optional: zero or one occurrence.
    ///
    /// Making the start state accepting only adds the empty sequence when nothing
    /// re-enters the start state except by an epsilon edge from an accepting state.
    /// After `reduce()` merges states that need not hold, so a fresh start state
    /// is added instead.
    fn optional(mut self) -> Result<Nfa, StateSpaceOverflow> {
        if self.accept[0] {
            return Ok(self);
        }
        // Re-entering the start state is harmless only via an epsilon edge from an
        // accepting state (as one_or_more() adds): whatever was read to get there is
        // already accepted. Any other edge into it would gain new accepted words.
        let start_reentered_unsafely = self.transitions.iter().enumerate().any(|(from, trans)| {
            trans
                .iter()
                .any(|&(label, target)| target == 0 && (label.is_some() || !self.accept[from]))
        });
        if start_reentered_unsafely {
            Nfa::alternate(vec![Nfa::new_empty(), self])
        } else {
            self.accept[0] = true;
            Ok(self)
        }
    }

    /// Shuffle product (for interleave): accepts all interleavings of sequences
    /// from `self` and `other`.
    fn shuffle(self, other: &Nfa) -> Result<Nfa, StateSpaceOverflow> {
        // States are (state_in_self, state_in_other) pairs.
        // We map pair (a, b) -> a * other.num_states + b, which is less than
        // total_states for a < self.num_states and b < other.num_states.
        let total_states = mul_states(self.num_states, other.num_states)?;
        let pair_to_id = |a: StateId, b: StateId| -> StateId { a * other.num_states + b };

        let mut transitions = vec![vec![]; total_states as usize];
        let mut accept = vec![false; total_states as usize];

        for sa in 0..self.num_states {
            for sb in 0..other.num_states {
                let id = pair_to_id(sa, sb);

                // Transitions from self: advance sa, keep sb
                for &(label, sa_next) in &self.transitions[sa as usize] {
                    transitions[id as usize].push((label, pair_to_id(sa_next, sb)));
                }

                // Transitions from other: keep sa, advance sb
                for &(label, sb_next) in &other.transitions[sb as usize] {
                    transitions[id as usize].push((label, pair_to_id(sa, sb_next)));
                }

                // Accept if both components accept
                if self.accept[sa as usize] && other.accept[sb as usize] {
                    accept[id as usize] = true;
                }
            }
        }

        Ok(Nfa {
            num_states: total_states,
            transitions,
            accept,
        })
    }

    /// Return an NFA accepting the same language, usually with far fewer states.
    ///
    /// 1. Epsilon removal: each state takes on the labelled transitions and
    ///    acceptance of its epsilon closure.
    /// 2. Trimming: states unreachable from the start, or that cannot reach an
    ///    accepting state, are dropped.
    /// 3. Bisimulation: states with the same acceptance whose transitions lead,
    ///    label for label, to equivalent states are merged (partition refinement).
    ///
    /// None of these steps can increase the state count. This matters most before
    /// `shuffle()`, whose output size is the product of its operands' sizes.
    fn reduce(self) -> Nfa {
        let (trans, accept) = self.remove_epsilons();
        let Some((trans, accept)) = trim(trans, accept) else {
            return Nfa::new_dead();
        };
        let (block, num_blocks) = bisimulation_blocks(&trans, &accept);

        let mut q_trans: Vec<Vec<(Option<LabelId>, StateId)>> = vec![vec![]; num_blocks];
        let mut q_accept = vec![false; num_blocks];
        for (s, s_trans) in trans.iter().enumerate() {
            let b = block[s] as usize;
            q_accept[b] = accept[s];
            for &(label, target) in s_trans {
                q_trans[b].push((Some(label), block[target as usize]));
            }
        }
        for t in &mut q_trans {
            t.sort_unstable();
            t.dedup();
        }
        Nfa {
            // num_blocks <= original num_states, so it fits in a StateId
            num_states: num_blocks as StateId,
            transitions: q_trans,
            accept: q_accept,
        }
    }

    /// Epsilon-free transitions and acceptance for every state.
    fn remove_epsilons(self) -> EpsilonFree {
        let n = self.num_states as usize;
        let has_eps = self.transitions.iter().flatten().any(|(l, _)| l.is_none());
        if !has_eps {
            let trans = self
                .transitions
                .into_iter()
                .map(|t| {
                    t.into_iter()
                        .map(|(l, target)| (l.unwrap(), target))
                        .collect()
                })
                .collect();
            return (trans, self.accept);
        }
        let mut trans = Vec::with_capacity(n);
        let mut accept = Vec::with_capacity(n);
        // stamp[q] == s marks q as already in the closure of s
        let mut stamp = vec![usize::MAX; n];
        let mut stack = Vec::new();
        for s in 0..n {
            let mut out = Vec::new();
            let mut acc = false;
            stamp[s] = s;
            stack.push(s);
            while let Some(q) = stack.pop() {
                acc |= self.accept[q];
                for &(label, target) in &self.transitions[q] {
                    match label {
                        Some(l) => out.push((l, target)),
                        None => {
                            if stamp[target as usize] != s {
                                stamp[target as usize] = s;
                                stack.push(target as usize);
                            }
                        }
                    }
                }
            }
            out.sort_unstable();
            out.dedup();
            trans.push(out);
            accept.push(acc);
        }
        (trans, accept)
    }

    /// Returns true if the NFA accepts no strings (language is empty).
    #[cfg(test)]
    fn is_empty(&self) -> bool {
        if !self.accept.iter().any(|&a| a) {
            return true;
        }
        if self.accept[0] {
            return false;
        }
        // BFS from start state
        let mut visited = vec![false; self.num_states as usize];
        let mut queue = VecDeque::new();
        visited[0] = true;
        queue.push_back(0u32);
        while let Some(state) = queue.pop_front() {
            for &(_, target) in &self.transitions[state as usize] {
                if self.accept[target as usize] {
                    return false;
                }
                if !visited[target as usize] {
                    visited[target as usize] = true;
                    queue.push_back(target);
                }
            }
        }
        true
    }

    /// Check if this NFA accepts a given sequence of labels (for testing).
    #[cfg(test)]
    fn accepts(&self, input: &[LabelId]) -> bool {
        // Compute epsilon closure of a set of states
        fn epsilon_closure(nfa: &Nfa, states: &HashSet<StateId>) -> HashSet<StateId> {
            let mut closure = states.clone();
            let mut queue: VecDeque<StateId> = states.iter().copied().collect();
            while let Some(s) = queue.pop_front() {
                for &(label, target) in &nfa.transitions[s as usize] {
                    if label.is_none() && closure.insert(target) {
                        queue.push_back(target);
                    }
                }
            }
            closure
        }

        let mut current = HashSet::new();
        current.insert(0);
        current = epsilon_closure(self, &current);

        for &sym in input {
            let mut next = HashSet::new();
            for &state in &current {
                for &(label, target) in &self.transitions[state as usize] {
                    if label == Some(sym) {
                        next.insert(target);
                    }
                }
            }
            if next.is_empty() {
                return false;
            }
            current = epsilon_closure(self, &next);
        }

        current.iter().any(|s| self.accept[*s as usize])
    }
}

/// Drop states that are unreachable from state 0 or cannot reach an accepting
/// state, renumbering the rest (state 0 stays 0). Returns `None` when state 0
/// itself cannot reach an accepting state, i.e. the language is empty.
fn trim(trans: Vec<Vec<(LabelId, StateId)>>, accept: Vec<bool>) -> Option<EpsilonFree> {
    let n = trans.len();
    let mut forward = vec![false; n];
    let mut stack = vec![0usize];
    forward[0] = true;
    while let Some(s) = stack.pop() {
        for &(_, t) in &trans[s] {
            if !forward[t as usize] {
                forward[t as usize] = true;
                stack.push(t as usize);
            }
        }
    }
    let mut reverse: Vec<Vec<usize>> = vec![vec![]; n];
    for (s, s_trans) in trans.iter().enumerate() {
        for &(_, t) in s_trans {
            reverse[t as usize].push(s);
        }
    }
    let mut backward = vec![false; n];
    for s in 0..n {
        if accept[s] {
            backward[s] = true;
            stack.push(s);
        }
    }
    while let Some(s) = stack.pop() {
        for &p in &reverse[s] {
            if !backward[p] {
                backward[p] = true;
                stack.push(p);
            }
        }
    }
    if !backward[0] {
        return None;
    }
    let mut new_id = vec![StateId::MAX; n];
    let mut next: StateId = 0;
    for s in 0..n {
        if forward[s] && backward[s] {
            new_id[s] = next;
            next += 1;
        }
    }
    let mut new_trans = Vec::with_capacity(next as usize);
    let mut new_accept = Vec::with_capacity(next as usize);
    for (s, s_trans) in trans.into_iter().enumerate() {
        if new_id[s] == StateId::MAX {
            continue;
        }
        new_trans.push(
            s_trans
                .into_iter()
                .filter(|&(_, t)| new_id[t as usize] != StateId::MAX)
                .map(|(l, t)| (l, new_id[t as usize]))
                .collect(),
        );
        new_accept.push(accept[s]);
    }
    Some((new_trans, new_accept))
}

/// Partition states into bisimulation classes by iterated refinement.
///
/// Returns each state's block and the number of blocks. Blocks are numbered in
/// order of their lowest state, so state 0 is always in block 0.
fn bisimulation_blocks(
    trans: &[Vec<(LabelId, StateId)>],
    accept: &[bool],
) -> (Vec<StateId>, usize) {
    let n = trans.len();
    let mut block: Vec<StateId> = accept.iter().map(|&a| a as StateId).collect();
    let mut num_blocks = usize::MAX;
    let mut sig: Vec<(LabelId, StateId)> = Vec::new();
    loop {
        // A state's signature is its current block plus the set of
        // (label, target block) pairs; equal signatures stay together.
        let mut ids: HashMap<(StateId, Vec<(LabelId, StateId)>), StateId> = HashMap::new();
        let mut new_block = Vec::with_capacity(n);
        for s in 0..n {
            sig.clear();
            sig.extend(trans[s].iter().map(|&(l, t)| (l, block[t as usize])));
            sig.sort_unstable();
            sig.dedup();
            let next_id = ids.len() as StateId;
            let id = *ids.entry((block[s], sig.clone())).or_insert(next_id);
            new_block.push(id);
        }
        let count = ids.len();
        block = new_block;
        if count == num_blocks {
            return (block, count);
        }
        num_blocks = count;
    }
}

// ---------------------------------------------------------------------------
// Label enumeration
// ---------------------------------------------------------------------------

/// Information about one Element pattern in the grammar.
/// Elements are stored in LabelId order, so elements[i] has label LabelId(i).
struct ElementInfo {
    name_class: NameClass,
    span: Option<codemap::Span>,
}

/// Walk the pattern tree and enumerate all Element patterns, assigning each a unique LabelId.
/// Returns the list of elements and a map from Pattern pointer address to LabelId.
fn enumerate_elements(start: &Pattern) -> (Vec<ElementInfo>, HashMap<usize, LabelId>) {
    let mut elements = Vec::new();
    let mut ptr_to_label: HashMap<usize, LabelId> = HashMap::new();
    let mut seen_refs = HashSet::new();
    let mut next_label = 0u32;
    enumerate_walk(
        start,
        &mut elements,
        &mut ptr_to_label,
        &mut seen_refs,
        &mut next_label,
    );
    (elements, ptr_to_label)
}

fn enumerate_walk(
    pattern: &Pattern,
    elements: &mut Vec<ElementInfo>,
    ptr_to_label: &mut HashMap<usize, LabelId>,
    seen_refs: &mut HashSet<usize>,
    next_label: &mut u32,
) {
    match pattern {
        Pattern::Element(nc, body, span, _) => {
            let ptr = pattern as *const Pattern as usize;
            if !ptr_to_label.contains_key(&ptr) {
                let label = LabelId(*next_label);
                *next_label += 1;
                ptr_to_label.insert(ptr, label);
                elements.push(ElementInfo {
                    name_class: nc.clone(),
                    span: *span,
                });
                // Recurse into body to find nested elements
                enumerate_walk(body, elements, ptr_to_label, seen_refs, next_label);
            }
        }
        Pattern::Choice(children, _)
        | Pattern::Interleave(children, _)
        | Pattern::Group(children, _) => {
            for child in children {
                enumerate_walk(child, elements, ptr_to_label, seen_refs, next_label);
            }
        }
        Pattern::OneOrMore(body, _)
        | Pattern::ZeroOrMore(body, _)
        | Pattern::Optional(body, _)
        | Pattern::Mixed(body, _)
        | Pattern::List(body, _) => {
            enumerate_walk(body, elements, ptr_to_label, seen_refs, next_label);
        }
        Pattern::Attribute(_, body, _, _) => {
            // Attributes can contain elements in their value patterns (rare but possible)
            enumerate_walk(body, elements, ptr_to_label, seen_refs, next_label);
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    enumerate_walk(
                        rule.pattern(),
                        elements,
                        ptr_to_label,
                        seen_refs,
                        next_label,
                    );
                }
            }
        }
        Pattern::DatatypeName { except, .. } => {
            if let Some(e) = except {
                enumerate_walk(e, elements, ptr_to_label, seen_refs, next_label);
            }
        }
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. } => {}
    }
}

// ---------------------------------------------------------------------------
// Pattern to NFA conversion
// ---------------------------------------------------------------------------

/// Convert a content model pattern into an NFA over the label alphabet.
/// Element patterns become single transitions; we do NOT recurse into their bodies.
///
/// Every sub-pattern's NFA is passed through `Nfa::reduce()`. Label-free
/// sub-patterns (attributes, text, data, and groups or interleaves of those)
/// collapse to a single state, and repetitions of elements lose the epsilon
/// structure of their construction. Without this, shuffle products of the
/// unreduced NFAs can reach hundreds of thousands of states.
fn pattern_to_nfa(
    pattern: &Pattern,
    ptr_to_label: &HashMap<usize, LabelId>,
    seen_refs: &mut HashSet<usize>,
) -> Result<Nfa, StateSpaceOverflow> {
    Ok(pattern_to_nfa_inner(pattern, ptr_to_label, seen_refs)?.reduce())
}

fn pattern_to_nfa_inner(
    pattern: &Pattern,
    ptr_to_label: &HashMap<usize, LabelId>,
    seen_refs: &mut HashSet<usize>,
) -> Result<Nfa, StateSpaceOverflow> {
    let nfa = match pattern {
        Pattern::Element(_, _, _, _) => {
            let ptr = pattern as *const Pattern as usize;
            if let Some(&label) = ptr_to_label.get(&ptr) {
                Nfa::new_single(label)
            } else {
                // Element not in our label map — shouldn't happen
                Nfa::new_dead()
            }
        }
        Pattern::Choice(children, _) => {
            let nfas = children
                .iter()
                .map(|c| pattern_to_nfa(c, ptr_to_label, seen_refs))
                .collect::<Result<Vec<Nfa>, _>>()?;
            Nfa::alternate(nfas)?
        }
        Pattern::Group(children, _) => {
            let mut result = Nfa::new_empty();
            for child in children {
                result = result.concatenate(pattern_to_nfa(child, ptr_to_label, seen_refs)?)?;
            }
            result
        }
        Pattern::Interleave(children, _) => {
            let mut iter = children.iter();
            let first = iter.next();
            match first {
                None => Nfa::new_empty(),
                Some(first) => {
                    let mut result = pattern_to_nfa(first, ptr_to_label, seen_refs)?;
                    for child in iter {
                        let child_nfa = pattern_to_nfa(child, ptr_to_label, seen_refs)?;
                        result = result.shuffle(&child_nfa)?;
                    }
                    result
                }
            }
        }
        Pattern::Mixed(body, _) => {
            // Mixed = interleave(text, body). Text contributes no element children,
            // so the NFA is just the body's NFA.
            pattern_to_nfa(body, ptr_to_label, seen_refs)?
        }
        Pattern::OneOrMore(body, _) => pattern_to_nfa(body, ptr_to_label, seen_refs)?.one_or_more(),
        Pattern::ZeroOrMore(body, _) => {
            pattern_to_nfa(body, ptr_to_label, seen_refs)?.zero_or_more()?
        }
        Pattern::Optional(body, _) => pattern_to_nfa(body, ptr_to_label, seen_refs)?.optional()?,
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if !seen_refs.insert(ptr) {
                // Cycle — RELAX NG spec requires recursion through Element,
                // so this shouldn't happen in valid schemas. Return dead NFA.
                return Ok(Nfa::new_dead());
            }
            let result = {
                let borrowed = pat_ref.0.borrow();
                match borrowed.as_ref() {
                    Some(rule) => pattern_to_nfa(rule.pattern(), ptr_to_label, seen_refs),
                    None => Ok(Nfa::new_dead()),
                }
            };
            seen_refs.remove(&ptr);
            result?
        }
        Pattern::Attribute(_, _, _, _) => {
            // Attributes don't contribute child elements
            Nfa::new_empty()
        }
        Pattern::List(_, _) => Nfa::new_empty(),
        Pattern::Empty(_) => Nfa::new_empty(),
        Pattern::Text(_) => Nfa::new_empty(),
        Pattern::NotAllowed(_) => Nfa::new_dead(),
        Pattern::DatatypeValue { .. } => Nfa::new_empty(),
        Pattern::DatatypeName { .. } => Nfa::new_empty(),
    };
    Ok(nfa)
}

// ---------------------------------------------------------------------------
// Score matrix
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Score {
    Ambiguous,
    Unambiguous,
}

struct ScoreMatrix {
    size: usize,
    cells: Vec<Option<Score>>,
}

impl ScoreMatrix {
    fn new(size: usize) -> Self {
        ScoreMatrix {
            size,
            cells: vec![None; size * size],
        }
    }

    fn get(&self, i: LabelId, j: LabelId) -> Option<Score> {
        self.cells[i.0 as usize * self.size + j.0 as usize]
    }

    fn set(&mut self, i: LabelId, j: LabelId, score: Score) {
        self.cells[i.0 as usize * self.size + j.0 as usize] = Some(score);
        self.cells[j.0 as usize * self.size + i.0 as usize] = Some(score);
    }
}

// ---------------------------------------------------------------------------
// dTLA: detect two labels ambiguity (Algorithm 2)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DtlaResult {
    Ambiguous,
    Unambiguous,
    Undecidable,
}

/// Check whether two labels are in the ambiguous relation.
/// Uses lazy BFS product automaton construction.
fn dtla(
    l1: LabelId,
    l2: LabelId,
    elements: &[ElementInfo],
    nfas: &[Nfa],
    score: &ScoreMatrix,
) -> Result<DtlaResult, StateSpaceOverflow> {
    let e1 = &elements[l1.0 as usize];
    let e2 = &elements[l2.0 as usize];

    // Different element names can never be confused
    if !restrictions::name_class_overlaps(&e1.name_class, &e2.name_class) {
        return Ok(DtlaResult::Unambiguous);
    }

    let nfa1 = &nfas[l1.0 as usize];
    let nfa2 = &nfas[l2.0 as usize];

    // Check M*: alphabet restricted to pairs where a==b or score(a,b)==Ambiguous
    let m_star_nonempty = product_nonempty(nfa1, nfa2, |a, b| {
        a == b || score.get(a, b) == Some(Score::Ambiguous)
    })?;

    if m_star_nonempty {
        return Ok(DtlaResult::Ambiguous);
    }

    // Check M+: alphabet includes undecided pairs too
    let m_plus_nonempty = product_nonempty(nfa1, nfa2, |a, b| {
        a == b || score.get(a, b) != Some(Score::Unambiguous)
    })?;

    if !m_plus_nonempty {
        return Ok(DtlaResult::Unambiguous);
    }

    Ok(DtlaResult::Undecidable)
}

/// Compute epsilon closure of a single NFA state, returned as a sorted Vec.
fn eps_closure(nfa: &Nfa, start: StateId) -> Vec<StateId> {
    let mut in_closure = vec![false; nfa.num_states as usize];
    let mut closure = Vec::new();
    let mut queue = VecDeque::new();
    in_closure[start as usize] = true;
    closure.push(start);
    queue.push_back(start);
    while let Some(s) = queue.pop_front() {
        for &(label, target) in &nfa.transitions[s as usize] {
            if label.is_none() && !in_closure[target as usize] {
                in_closure[target as usize] = true;
                closure.push(target);
                queue.push_back(target);
            }
        }
    }
    closure
}

/// Collect non-epsilon transitions per state.
fn non_eps_transitions(nfa: &Nfa) -> Vec<Vec<(LabelId, StateId)>> {
    (0..nfa.num_states as usize)
        .map(|s| {
            nfa.transitions[s]
                .iter()
                .filter_map(|&(label, target)| label.map(|l| (l, target)))
                .collect()
        })
        .collect()
}

/// Lazy BFS to check if the product automaton of two NFAs is non-empty.
/// `allow` determines which symbol pairs (a, b) are valid transitions.
fn product_nonempty(
    nfa1: &Nfa,
    nfa2: &Nfa,
    allow: impl Fn(LabelId, LabelId) -> bool,
) -> Result<bool, StateSpaceOverflow> {
    let n2 = nfa2.num_states;
    // Checked before any other work; every flat index s1 * n2 + s2 below is less
    // than this.
    let product_size = mul_states(nfa1.num_states, n2)?;

    // Precompute epsilon closures for all states
    let eps1: Vec<Vec<StateId>> = (0..nfa1.num_states).map(|s| eps_closure(nfa1, s)).collect();
    let eps2: Vec<Vec<StateId>> = (0..nfa2.num_states).map(|s| eps_closure(nfa2, s)).collect();

    let trans1 = non_eps_transitions(nfa1);
    let trans2 = non_eps_transitions(nfa2);

    // Flat visited array indexed by s1 * n2 + s2
    let mut visited = vec![false; product_size as usize];
    let mut queue: VecDeque<(StateId, StateId)> = VecDeque::new();

    // Initialize with all pairs in eps_closure(start1) x eps_closure(start2)
    for &s1 in &eps1[0] {
        for &s2 in &eps2[0] {
            let idx = (s1 * n2 + s2) as usize;
            if !visited[idx] {
                visited[idx] = true;
                if nfa1.accept[s1 as usize] && nfa2.accept[s2 as usize] {
                    return Ok(true);
                }
                queue.push_back((s1, s2));
            }
        }
    }

    while let Some((s1, s2)) = queue.pop_front() {
        for &(label_a, target_a) in &trans1[s1 as usize] {
            for &(label_b, target_b) in &trans2[s2 as usize] {
                if allow(label_a, label_b) {
                    for &t1 in &eps1[target_a as usize] {
                        for &t2 in &eps2[target_b as usize] {
                            let idx = (t1 * n2 + t2) as usize;
                            if !visited[idx] {
                                visited[idx] = true;
                                if nfa1.accept[t1 as usize] && nfa2.accept[t2 as usize] {
                                    return Ok(true);
                                }
                                queue.push_back((t1, t2));
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(false)
}

// ---------------------------------------------------------------------------
// Catalyst detection (Algorithm 3, dC)
// ---------------------------------------------------------------------------

/// Check if a pair of elements with overlapping name classes acts as a catalyst.
/// A catalyst is an element that "provides a choice" between ambiguous labels.
fn is_catalyst(
    l1: LabelId,
    l2: LabelId,
    elements: &[ElementInfo],
    nfas: &[Nfa],
    score: &ScoreMatrix,
) -> Result<bool, StateSpaceOverflow> {
    let e1 = &elements[l1.0 as usize];
    let e2 = &elements[l2.0 as usize];

    if !restrictions::name_class_overlaps(&e1.name_class, &e2.name_class) {
        return Ok(false);
    }

    let nfa1 = &nfas[l1.0 as usize];
    let nfa2 = &nfas[l2.0 as usize];

    // Build product automaton M* with the fully-resolved score matrix.
    // Check if there exists a reachable transition using (a, b) where a~b (ambiguous).
    catalyst_check(nfa1, nfa2, score)
}

/// Build the product automaton, trim right-unreachable states, and check
/// whether any transition uses a pair (a, b) where a != b and score(a,b) == Ambiguous.
fn catalyst_check(nfa1: &Nfa, nfa2: &Nfa, score: &ScoreMatrix) -> Result<bool, StateSpaceOverflow> {
    let n2 = nfa2.num_states;
    // Checked before any other work; every flat index s1 * n2 + s2 below is less
    // than this, and so is every compact index (one per visited product state).
    // Compact indices therefore never reach the u32::MAX "unassigned" sentinel.
    let product_size = mul_states(nfa1.num_states, n2)? as usize;

    let eps1: Vec<Vec<StateId>> = (0..nfa1.num_states).map(|s| eps_closure(nfa1, s)).collect();
    let eps2: Vec<Vec<StateId>> = (0..nfa2.num_states).map(|s| eps_closure(nfa2, s)).collect();

    let trans1 = non_eps_transitions(nfa1);
    let trans2 = non_eps_transitions(nfa2);

    // Phase 1: Forward BFS to find all reachable product states and their transitions.
    // Use flat visited array indexed by s1 * n2 + s2.
    // Also assign a compact index to each visited product state.
    let mut visited = vec![false; product_size];
    // Maps flat product-state id -> compact index (u32::MAX = not assigned)
    let mut flat_to_idx = vec![u32::MAX; product_size];
    let mut queue: VecDeque<(StateId, StateId)> = VecDeque::new();
    let mut all_states: Vec<(StateId, StateId)> = Vec::new();
    // Edges stored with compact indices: (from_idx, label_a, label_b, to_idx)
    let mut edges: Vec<(u32, LabelId, LabelId, u32)> = Vec::new();

    // Helper: mark visited and assign compact index, returns the compact index
    let visit = |s1: StateId,
                 s2: StateId,
                 visited: &mut Vec<bool>,
                 flat_to_idx: &mut Vec<u32>,
                 all_states: &mut Vec<(StateId, StateId)>|
     -> u32 {
        let flat = (s1 * n2 + s2) as usize;
        if !visited[flat] {
            visited[flat] = true;
            let idx = all_states.len() as u32;
            flat_to_idx[flat] = idx;
            all_states.push((s1, s2));
            idx
        } else {
            flat_to_idx[flat]
        }
    };

    for &s1 in &eps1[0] {
        for &s2 in &eps2[0] {
            let flat = (s1 * n2 + s2) as usize;
            if !visited[flat] {
                visit(s1, s2, &mut visited, &mut flat_to_idx, &mut all_states);
                queue.push_back((s1, s2));
            }
        }
    }

    while let Some((s1, s2)) = queue.pop_front() {
        let from_idx = flat_to_idx[(s1 * n2 + s2) as usize];
        for &(label_a, target_a) in &trans1[s1 as usize] {
            for &(label_b, target_b) in &trans2[s2 as usize] {
                let allowed =
                    label_a == label_b || score.get(label_a, label_b) == Some(Score::Ambiguous);
                if !allowed {
                    continue;
                }
                for &t1 in &eps1[target_a as usize] {
                    for &t2 in &eps2[target_b as usize] {
                        let flat = (t1 * n2 + t2) as usize;
                        let was_new = !visited[flat];
                        let to_idx = visit(t1, t2, &mut visited, &mut flat_to_idx, &mut all_states);
                        edges.push((from_idx, label_a, label_b, to_idx));
                        if was_new {
                            queue.push_back((t1, t2));
                        }
                    }
                }
            }
        }
    }

    // Phase 2: Compute right-reachable states (can reach an accepting state).
    let n = all_states.len();
    let mut reverse_adj: Vec<Vec<u32>> = vec![vec![]; n];
    let mut forward_edges_by_source: Vec<Vec<u32>> = vec![vec![]; n];

    for (edge_idx, &(from_idx, _, _, to_idx)) in edges.iter().enumerate() {
        reverse_adj[to_idx as usize].push(from_idx);
        // Edges are not bounded by product_size, so this conversion is checked.
        let edge_idx = u32::try_from(edge_idx).map_err(|_| StateSpaceOverflow)?;
        forward_edges_by_source[from_idx as usize].push(edge_idx);
    }

    let mut right_reachable = vec![false; n];
    let mut rqueue: VecDeque<u32> = VecDeque::new();
    for (idx, &(s1, s2)) in all_states.iter().enumerate() {
        if nfa1.accept[s1 as usize] && nfa2.accept[s2 as usize] {
            right_reachable[idx] = true;
            rqueue.push_back(idx as u32);
        }
    }
    while let Some(idx) = rqueue.pop_front() {
        for &pred_idx in &reverse_adj[idx as usize] {
            if !right_reachable[pred_idx as usize] {
                right_reachable[pred_idx as usize] = true;
                rqueue.push_back(pred_idx);
            }
        }
    }

    // Phase 3: Check if any edge from a right-reachable state to a right-reachable state
    // uses a pair (a, b) where a != b and a ~ b.
    for idx in 0..n {
        if !right_reachable[idx] {
            continue;
        }
        for &edge_idx in &forward_edges_by_source[idx] {
            let (_, label_a, label_b, to_idx) = edges[edge_idx as usize];
            if right_reachable[to_idx as usize]
                && label_a != label_b
                && score.get(label_a, label_b) == Some(Score::Ambiguous)
            {
                return Ok(true);
            }
        }
    }

    Ok(false)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Description of an ambiguity found in the grammar.
#[derive(Debug)]
pub struct AmbiguityWarning {
    pub kind: AmbiguityKind,
}

/// The type of ambiguity detected.
#[derive(Debug)]
pub enum AmbiguityKind {
    /// The grammar is ambiguous: a catalyst element was found that enables
    /// multiple valid interpretations of some document instances.
    AmbiguousGrammar {
        /// Pairs of element spans that are in the ambiguous relation
        ambiguous_pairs: Vec<(Option<codemap::Span>, Option<codemap::Span>)>,
    },
}

/// Check a compiled RELAX NG schema for ambiguity.
///
/// Returns a list of ambiguity warnings. An empty list means the grammar
/// is unambiguous (every valid document has exactly one interpretation).
///
/// Returns `Err` if the grammar's content models are too large for the check to
/// represent, in which case nothing is known about the grammar's ambiguity.
pub fn check_ambiguity(start: &Pattern) -> Result<Vec<AmbiguityWarning>, StateSpaceOverflow> {
    // Phase 1: Enumerate all Element patterns
    let (elements, ptr_to_label) = enumerate_elements(start);
    let n = elements.len();

    if n == 0 {
        return Ok(vec![]);
    }

    // Phase 2: Build NFAs for each element's content model
    let nfas = build_content_nfas(start, &elements, &ptr_to_label)?;

    // Phase 3: Initialize score matrix
    let mut score = ScoreMatrix::new(n);

    // Pre-fill unambiguous for pairs with non-overlapping name classes
    for i in 0..n {
        for j in (i + 1)..n {
            let li = LabelId(i as u32);
            let lj = LabelId(j as u32);
            if !restrictions::name_class_overlaps(&elements[i].name_class, &elements[j].name_class)
            {
                score.set(li, lj, Score::Unambiguous);
            }
        }
    }

    // Phase 4: Fixed-point iteration
    loop {
        let mut changed = false;
        for i in 0..n {
            for j in (i + 1)..n {
                let li = LabelId(i as u32);
                let lj = LabelId(j as u32);
                if score.get(li, lj).is_some() {
                    continue; // Already determined
                }
                match dtla(li, lj, &elements, &nfas, &score)? {
                    DtlaResult::Ambiguous => {
                        score.set(li, lj, Score::Ambiguous);
                        changed = true;
                    }
                    DtlaResult::Unambiguous => {
                        score.set(li, lj, Score::Unambiguous);
                        changed = true;
                    }
                    DtlaResult::Undecidable => {}
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Finalize: remaining undecided entries are unambiguous
    for i in 0..n {
        for j in (i + 1)..n {
            let li = LabelId(i as u32);
            let lj = LabelId(j as u32);
            if score.get(li, lj).is_none() {
                score.set(li, lj, Score::Unambiguous);
            }
        }
    }

    // Phase 5: Collect ambiguous pairs and verify at least one catalyst exists
    let mut ambiguous_pairs = Vec::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let li = LabelId(i as u32);
            let lj = LabelId(j as u32);
            if score.get(li, lj) == Some(Score::Ambiguous) {
                ambiguous_pairs.push((elements[i].span, elements[j].span));
            }
        }
    }

    if ambiguous_pairs.is_empty() {
        return Ok(vec![]);
    }

    // A grammar is only truly ambiguous if a catalyst exists — an element
    // whose content model can exercise an ambiguous pair.
    let mut has_catalyst = false;
    'search: for i in 0..n {
        for j in i..n {
            let li = LabelId(i as u32);
            let lj = LabelId(j as u32);
            if (i == j
                || restrictions::name_class_overlaps(
                    &elements[i].name_class,
                    &elements[j].name_class,
                ))
                && is_catalyst(li, lj, &elements, &nfas, &score)?
            {
                has_catalyst = true;
                break 'search;
            }
        }
    }

    if !has_catalyst {
        return Ok(vec![]);
    }

    Ok(vec![AmbiguityWarning {
        kind: AmbiguityKind::AmbiguousGrammar { ambiguous_pairs },
    }])
}

/// Build NFA content models for each element by re-walking the pattern tree.
fn build_content_nfas(
    start: &Pattern,
    elements: &[ElementInfo],
    ptr_to_label: &HashMap<usize, LabelId>,
) -> Result<Vec<Nfa>, StateSpaceOverflow> {
    let mut nfas: Vec<Option<Nfa>> = (0..elements.len()).map(|_| None).collect();
    let mut seen_refs = HashSet::new();
    build_nfas_walk(start, ptr_to_label, &mut nfas, &mut seen_refs)?;

    // Fill any missing NFAs with empty (shouldn't happen)
    Ok(nfas
        .into_iter()
        .map(|opt| opt.unwrap_or_else(Nfa::new_empty))
        .collect())
}

fn build_nfas_walk(
    pattern: &Pattern,
    ptr_to_label: &HashMap<usize, LabelId>,
    nfas: &mut [Option<Nfa>],
    seen_refs: &mut HashSet<usize>,
) -> Result<(), StateSpaceOverflow> {
    match pattern {
        Pattern::Element(_, body, _, _) => {
            let ptr = pattern as *const Pattern as usize;
            if let Some(&label) = ptr_to_label.get(&ptr) {
                let idx = label.0 as usize;
                if nfas[idx].is_none() {
                    let mut nfa_seen = HashSet::new();
                    nfas[idx] = Some(pattern_to_nfa(body, ptr_to_label, &mut nfa_seen)?);
                    // Continue into body to build NFAs for nested elements
                    build_nfas_walk(body, ptr_to_label, nfas, seen_refs)?;
                }
            }
        }
        Pattern::Choice(children, _)
        | Pattern::Interleave(children, _)
        | Pattern::Group(children, _) => {
            for child in children {
                build_nfas_walk(child, ptr_to_label, nfas, seen_refs)?;
            }
        }
        Pattern::OneOrMore(body, _)
        | Pattern::ZeroOrMore(body, _)
        | Pattern::Optional(body, _)
        | Pattern::Mixed(body, _)
        | Pattern::List(body, _) => {
            build_nfas_walk(body, ptr_to_label, nfas, seen_refs)?;
        }
        Pattern::Attribute(_, body, _, _) => {
            build_nfas_walk(body, ptr_to_label, nfas, seen_refs)?;
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    build_nfas_walk(rule.pattern(), ptr_to_label, nfas, seen_refs)?;
                }
            }
        }
        Pattern::DatatypeName { except, .. } => {
            if let Some(e) = except {
                build_nfas_walk(e, ptr_to_label, nfas, seen_refs)?;
            }
        }
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. } => {}
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::NameClass;

    // -- NFA operation tests --

    #[test]
    fn nfa_empty_accepts_epsilon() {
        let nfa = Nfa::new_empty();
        assert!(nfa.accepts(&[]));
        assert!(!nfa.accepts(&[LabelId(0)]));
    }

    #[test]
    fn nfa_dead_accepts_nothing() {
        let nfa = Nfa::new_dead();
        assert!(!nfa.accepts(&[]));
        assert!(!nfa.accepts(&[LabelId(0)]));
        assert!(nfa.is_empty());
    }

    #[test]
    fn nfa_single_accepts_one_symbol() {
        let nfa = Nfa::new_single(LabelId(0));
        assert!(!nfa.accepts(&[]));
        assert!(nfa.accepts(&[LabelId(0)]));
        assert!(!nfa.accepts(&[LabelId(1)]));
        assert!(!nfa.accepts(&[LabelId(0), LabelId(0)]));
    }

    #[test]
    fn nfa_concatenation() {
        let a = Nfa::new_single(LabelId(0));
        let b = Nfa::new_single(LabelId(1));
        let ab = a.concatenate(b).unwrap();
        assert!(!ab.accepts(&[]));
        assert!(!ab.accepts(&[LabelId(0)]));
        assert!(!ab.accepts(&[LabelId(1)]));
        assert!(ab.accepts(&[LabelId(0), LabelId(1)]));
        assert!(!ab.accepts(&[LabelId(1), LabelId(0)]));
    }

    #[test]
    fn nfa_alternation() {
        let a = Nfa::new_single(LabelId(0));
        let b = Nfa::new_single(LabelId(1));
        let alt = Nfa::alternate(vec![a, b]).unwrap();
        assert!(!alt.accepts(&[]));
        assert!(alt.accepts(&[LabelId(0)]));
        assert!(alt.accepts(&[LabelId(1)]));
        assert!(!alt.accepts(&[LabelId(0), LabelId(1)]));
    }

    #[test]
    fn nfa_one_or_more() {
        let a = Nfa::new_single(LabelId(0));
        let plus = a.one_or_more();
        assert!(!plus.accepts(&[]));
        assert!(plus.accepts(&[LabelId(0)]));
        assert!(plus.accepts(&[LabelId(0), LabelId(0)]));
        assert!(plus.accepts(&[LabelId(0), LabelId(0), LabelId(0)]));
        assert!(!plus.accepts(&[LabelId(1)]));
    }

    #[test]
    fn nfa_zero_or_more() {
        let a = Nfa::new_single(LabelId(0));
        let star = a.zero_or_more().unwrap();
        assert!(star.accepts(&[]));
        assert!(star.accepts(&[LabelId(0)]));
        assert!(star.accepts(&[LabelId(0), LabelId(0)]));
        assert!(!star.accepts(&[LabelId(1)]));
    }

    #[test]
    fn nfa_optional() {
        let a = Nfa::new_single(LabelId(0));
        let opt = a.optional().unwrap();
        assert!(opt.accepts(&[]));
        assert!(opt.accepts(&[LabelId(0)]));
        assert!(!opt.accepts(&[LabelId(0), LabelId(0)]));
    }

    #[test]
    fn nfa_shuffle_two_symbols() {
        let a = Nfa::new_single(LabelId(0));
        let b = Nfa::new_single(LabelId(1));
        let shuf = a.shuffle(&b).unwrap();
        assert!(!shuf.accepts(&[]));
        assert!(!shuf.accepts(&[LabelId(0)]));
        assert!(!shuf.accepts(&[LabelId(1)]));
        assert!(shuf.accepts(&[LabelId(0), LabelId(1)]));
        assert!(shuf.accepts(&[LabelId(1), LabelId(0)]));
        assert!(!shuf.accepts(&[LabelId(0), LabelId(0)]));
    }

    #[test]
    fn nfa_shuffle_sequence_and_single() {
        // shuffle(a.b, c) should accept: a.b.c, a.c.b, c.a.b
        let ab = Nfa::new_single(LabelId(0))
            .concatenate(Nfa::new_single(LabelId(1)))
            .unwrap();
        let c = Nfa::new_single(LabelId(2));
        let shuf = ab.shuffle(&c).unwrap();
        assert!(shuf.accepts(&[LabelId(0), LabelId(1), LabelId(2)]));
        assert!(shuf.accepts(&[LabelId(0), LabelId(2), LabelId(1)]));
        assert!(shuf.accepts(&[LabelId(2), LabelId(0), LabelId(1)]));
        // Should NOT accept: b.a.c (a must come before b)
        assert!(!shuf.accepts(&[LabelId(1), LabelId(0), LabelId(2)]));
    }

    #[test]
    fn nfa_is_empty_checks() {
        assert!(Nfa::new_dead().is_empty());
        assert!(!Nfa::new_empty().is_empty());
        assert!(!Nfa::new_single(LabelId(0)).is_empty());

        // Concatenate with dead = empty
        let dead_concat = Nfa::new_single(LabelId(0))
            .concatenate(Nfa::new_dead())
            .unwrap();
        assert!(dead_concat.is_empty());
    }

    #[test]
    fn nfa_concatenation_with_empty() {
        // empty.a = a
        let ea = Nfa::new_empty()
            .concatenate(Nfa::new_single(LabelId(0)))
            .unwrap();
        assert!(ea.accepts(&[LabelId(0)]));
        assert!(!ea.accepts(&[]));

        // a.empty = a
        let ae = Nfa::new_single(LabelId(0))
            .concatenate(Nfa::new_empty())
            .unwrap();
        assert!(ae.accepts(&[LabelId(0)]));
        assert!(!ae.accepts(&[]));
    }

    // -- Product automaton tests --

    #[test]
    fn product_identical_nfas_nonempty() {
        // Two identical NFAs accepting [a] — product with allow(a,a) should be non-empty
        let nfa = Nfa::new_single(LabelId(0));
        assert!(product_nonempty(&nfa, &nfa, |a, b| a == b).unwrap());
    }

    #[test]
    fn product_disjoint_nfas_empty() {
        // NFA1 accepts [a], NFA2 accepts [b] — product with allow(x,y) only if x==y is empty
        let nfa1 = Nfa::new_single(LabelId(0));
        let nfa2 = Nfa::new_single(LabelId(1));
        assert!(!product_nonempty(&nfa1, &nfa2, |a, b| a == b).unwrap());
    }

    #[test]
    fn product_overlapping_nfas() {
        // NFA1 accepts [a, b], NFA2 accepts [a, c]
        // Product with allow(x,y) if x==y should be non-empty (both start with a)
        // Wait, actually the product requires both to accept the SAME sequence length.
        // [a,b] vs [a,c]: pair (a,a) is ok, then (b,c) — only allowed if allow(b,c).
        let nfa1 = Nfa::new_single(LabelId(0))
            .concatenate(Nfa::new_single(LabelId(1)))
            .unwrap();
        let nfa2 = Nfa::new_single(LabelId(0))
            .concatenate(Nfa::new_single(LabelId(2)))
            .unwrap();
        // Only allow equal pairs
        assert!(!product_nonempty(&nfa1, &nfa2, |a, b| a == b).unwrap());
        // Allow all pairs
        assert!(product_nonempty(&nfa1, &nfa2, |_, _| true).unwrap());
    }

    // -- Integration test: simple ambiguity detection --

    fn make_element(ns: &str, name: &str, body: Pattern) -> Pattern {
        Pattern::Element(
            NameClass::Named {
                namespace_uri: ns.to_string(),
                name: name.to_string(),
            },
            Box::new(body),
            None,
            None,
        )
    }

    #[test]
    fn unambiguous_different_element_names() {
        // element root { element a { empty } | element b { empty } }
        let root = make_element(
            "",
            "root",
            Pattern::Choice(
                vec![
                    make_element("", "a", Pattern::Empty(None)),
                    make_element("", "b", Pattern::Empty(None)),
                ],
                None,
            ),
        );
        let warnings = check_ambiguity(&root).unwrap();
        assert!(
            warnings.is_empty(),
            "Different element names should not be ambiguous"
        );
    }

    #[test]
    fn ambiguous_same_name_same_content() {
        // Two elements named "foo" with identical content (element bar { empty })
        // appearing as choices — this is ambiguous because a document with <foo><bar/></foo>
        // could match either definition.
        let root = make_element(
            "",
            "root",
            Pattern::Choice(
                vec![
                    make_element("", "foo", make_element("", "bar", Pattern::Empty(None))),
                    make_element("", "foo", make_element("", "bar", Pattern::Empty(None))),
                ],
                None,
            ),
        );
        let warnings = check_ambiguity(&root).unwrap();
        assert!(
            !warnings.is_empty(),
            "Same name, same content should be ambiguous"
        );
    }

    #[test]
    fn unambiguous_same_name_disjoint_content() {
        // Two elements named "foo" but with different required children:
        // one requires element x, the other requires element y.
        // No document can match both, so not ambiguous.
        let root = make_element(
            "",
            "root",
            Pattern::Choice(
                vec![
                    make_element("", "foo", make_element("", "x", Pattern::Empty(None))),
                    make_element("", "foo", make_element("", "y", Pattern::Empty(None))),
                ],
                None,
            ),
        );
        let warnings = check_ambiguity(&root).unwrap();
        assert!(
            warnings.is_empty(),
            "Same name but disjoint content should not be ambiguous"
        );
    }

    #[test]
    fn ambiguous_overlapping_content() {
        // element root { (element foo { element x { empty }+ }) |
        //                (element foo { element x { empty }, element x { empty } }) }
        // The second foo matches exactly two x's, which is also matched by the first foo's x+.
        let root = make_element(
            "",
            "root",
            Pattern::Choice(
                vec![
                    make_element(
                        "",
                        "foo",
                        Pattern::OneOrMore(
                            Box::new(make_element("", "x", Pattern::Empty(None))),
                            None,
                        ),
                    ),
                    make_element(
                        "",
                        "foo",
                        Pattern::Group(
                            vec![
                                make_element("", "x", Pattern::Empty(None)),
                                make_element("", "x", Pattern::Empty(None)),
                            ],
                            None,
                        ),
                    ),
                ],
                None,
            ),
        );
        let warnings = check_ambiguity(&root).unwrap();
        assert!(
            !warnings.is_empty(),
            "Overlapping content models should be ambiguous"
        );
    }

    fn optional_attr(name: &str) -> Pattern {
        Pattern::Optional(
            Box::new(Pattern::Attribute(
                NameClass::Named {
                    namespace_uri: String::new(),
                    name: name.to_string(),
                },
                Box::new(Pattern::Text(None)),
                None,
                None,
            )),
            None,
        )
    }

    #[test]
    fn attribute_interleave_does_not_inflate_nfa() {
        // interleave { (6 groups of 4 optional attributes) & element child { empty } }
        // Without collapsing label-free sub-NFAs, the shuffle product of the attribute
        // groups alone is thousands of epsilon-only states.
        let mut arms: Vec<Pattern> = (0..6)
            .map(|g| {
                Pattern::Group(
                    (0..4)
                        .map(|a| optional_attr(&format!("a{g}_{a}")))
                        .collect(),
                    None,
                )
            })
            .collect();
        let child = make_element("", "child", Pattern::Empty(None));
        arms.push(child);
        let body = Pattern::Interleave(arms, None);
        let root = make_element("", "root", body);

        let (elements, ptr_to_label) = enumerate_elements(&root);
        let nfas = build_content_nfas(&root, &elements, &ptr_to_label).unwrap();
        let root_nfa = &nfas[0];
        assert!(
            root_nfa.num_states <= 4,
            "root content NFA has {} states",
            root_nfa.num_states
        );
        assert!(root_nfa.accepts(&[LabelId(1)]));
        assert!(!root_nfa.accepts(&[]));
    }

    #[test]
    fn label_free_unsatisfiable_collapses_to_dead() {
        // group { attribute a { text }?, notAllowed } accepts nothing
        let pattern = Pattern::Group(vec![optional_attr("a"), Pattern::NotAllowed(None)], None);
        let nfa = pattern_to_nfa(&pattern, &HashMap::new(), &mut HashSet::new()).unwrap();
        assert_eq!(nfa.num_states, 1);
        assert!(nfa.is_empty());
    }

    /// An NFA with `n` states and no transitions, cheap to build even when large.
    fn wide_nfa(n: u32) -> Nfa {
        Nfa {
            num_states: n,
            transitions: vec![vec![]; n as usize],
            accept: vec![false; n as usize],
        }
    }

    /// An NFA that claims `n` states without allocating them. Only valid for
    /// operations that must reject it before touching its storage.
    fn phantom_nfa(n: u32) -> Nfa {
        Nfa {
            num_states: n,
            transitions: vec![],
            accept: vec![],
        }
    }

    #[test]
    fn concatenate_overflow_is_an_error() {
        let result = phantom_nfa(u32::MAX).concatenate(Nfa::new_single(LabelId(0)));
        assert_eq!(result.unwrap_err(), StateSpaceOverflow);
    }

    #[test]
    fn alternate_overflow_is_an_error() {
        // The new start state plus u32::MAX states does not fit.
        let result = Nfa::alternate(vec![phantom_nfa(u32::MAX), Nfa::new_empty()]);
        assert_eq!(result.unwrap_err(), StateSpaceOverflow);
    }

    #[test]
    fn shuffle_overflow_is_an_error() {
        // 65536 * 65536 = 2^32, which wraps to 0 in unchecked u32 arithmetic.
        let a = wide_nfa(1 << 16);
        let b = wide_nfa(1 << 16);
        assert_eq!(a.shuffle(&b).unwrap_err(), StateSpaceOverflow);
    }

    #[test]
    fn product_overflow_is_an_error() {
        // The product state space 2^32 must be rejected before it is allocated
        // or indexed.
        let a = wide_nfa(1 << 16);
        let b = wide_nfa(1 << 16);
        assert_eq!(
            product_nonempty(&a, &b, |_, _| true).unwrap_err(),
            StateSpaceOverflow
        );
        let score = ScoreMatrix::new(1);
        assert_eq!(
            catalyst_check(&a, &b, &score).unwrap_err(),
            StateSpaceOverflow
        );
    }

    // -- reduce() tests --

    /// Every word over labels 0..alphabet up to length max_len.
    fn all_words(alphabet: u32, max_len: usize) -> Vec<Vec<LabelId>> {
        let mut words = vec![vec![]];
        let mut frontier = vec![vec![]];
        for _ in 0..max_len {
            let mut next = Vec::new();
            for w in &frontier {
                for l in 0..alphabet {
                    let mut w2: Vec<LabelId> = w.clone();
                    w2.push(LabelId(l));
                    next.push(w2);
                }
            }
            words.extend(next.iter().cloned());
            frontier = next;
        }
        words
    }

    fn assert_same_language(original: &Nfa, reduced: &Nfa) {
        for w in all_words(3, 6) {
            assert_eq!(
                original.accepts(&w),
                reduced.accepts(&w),
                "reduce() changed acceptance of {w:?}"
            );
        }
    }

    fn sym(l: u32) -> Nfa {
        Nfa::new_single(LabelId(l))
    }

    fn sample_nfas() -> Vec<Nfa> {
        let ab = || sym(0).concatenate(sym(1)).unwrap();
        vec![
            Nfa::new_empty(),
            Nfa::new_dead(),
            sym(0).zero_or_more().unwrap(),
            sym(0).one_or_more(),
            ab().zero_or_more().unwrap(),
            ab().one_or_more().optional().unwrap(),
            Nfa::alternate(vec![ab(), sym(0).concatenate(sym(2)).unwrap()]).unwrap(),
            sym(0)
                .concatenate(sym(1).concatenate(sym(0)).unwrap().zero_or_more().unwrap())
                .unwrap(),
            sym(0)
                .optional()
                .unwrap()
                .shuffle(&sym(1).zero_or_more().unwrap())
                .unwrap(),
            ab().shuffle(&sym(2).optional().unwrap()).unwrap(),
            sym(0).concatenate(Nfa::new_dead()).unwrap(),
            Nfa::alternate(vec![sym(0).concatenate(Nfa::new_dead()).unwrap(), sym(1)]).unwrap(),
        ]
    }

    #[test]
    fn reduce_preserves_language() {
        for nfa in sample_nfas() {
            let reduced = nfa.clone().reduce();
            assert!(reduced.num_states <= nfa.num_states.max(1));
            assert_same_language(&nfa, &reduced);
        }
    }

    #[test]
    fn repetition_of_reduced_nfas_preserves_language() {
        // Applying optional/zero_or_more/one_or_more/concatenate to reduced NFAs,
        // whose start state may have been merged with a later state, must give
        // the same language as applying them to the unreduced originals.
        for nfa in sample_nfas() {
            let reduced = nfa.clone().reduce();
            assert_same_language(
                &nfa.clone().optional().unwrap(),
                &reduced.clone().optional().unwrap(),
            );
            assert_same_language(
                &nfa.clone().zero_or_more().unwrap(),
                &reduced.clone().zero_or_more().unwrap(),
            );
            assert_same_language(&nfa.clone().one_or_more(), &reduced.clone().one_or_more());
            assert_same_language(
                &nfa.clone().concatenate(sym(2)).unwrap(),
                &reduced.clone().concatenate(sym(2)).unwrap(),
            );
        }
    }

    #[test]
    fn optional_after_reduce_does_not_accept_through_merged_start() {
        // a, (b, a)*  reduces by merging the start state with the state reached
        // after "a b"; making that merged state accepting would accept "a b".
        let nfa = sym(0)
            .concatenate(sym(1).concatenate(sym(0)).unwrap().zero_or_more().unwrap())
            .unwrap()
            .reduce();
        assert_eq!(nfa.num_states, 2, "expected the start state to be merged");
        let opt = nfa.optional().unwrap();
        assert!(opt.accepts(&[]));
        assert!(opt.accepts(&[LabelId(0)]));
        assert!(!opt.accepts(&[LabelId(0), LabelId(1)]));
        assert!(opt.accepts(&[LabelId(0), LabelId(1), LabelId(0)]));
    }

    fn named_element(name: &str) -> Pattern {
        make_element("", name, Pattern::Empty(None))
    }

    fn content_nfa_states(body: Pattern) -> u32 {
        let root = make_element("", "root", body);
        let (elements, ptr_to_label) = enumerate_elements(&root);
        let nfas = build_content_nfas(&root, &elements, &ptr_to_label).unwrap();
        nfas[0].num_states
    }

    #[test]
    fn interleaved_zero_or_more_arms_reduce_to_one_state() {
        // interleave { element e0 {empty}* & ... & element e9 {empty}* } is (e0|...|e9)*
        let arms = (0..10)
            .map(|i| Pattern::ZeroOrMore(Box::new(named_element(&format!("e{i}"))), None))
            .collect();
        assert_eq!(content_nfa_states(Pattern::Interleave(arms, None)), 1);
    }

    #[test]
    fn interleaved_optional_arms_need_one_bit_each() {
        // Each optional arm must remember whether it has been used: 2^k states.
        let arms = (0..6)
            .map(|i| Pattern::Optional(Box::new(named_element(&format!("e{i}"))), None))
            .collect();
        assert_eq!(content_nfa_states(Pattern::Interleave(arms, None)), 1 << 6);
    }
}
