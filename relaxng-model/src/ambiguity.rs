//! Ambiguity detection for RELAX NG grammars.
//!
//! Implements the algorithm from Kawaguchi's "Ambiguity Detection Of RELAX Grammars" (2001).
//! A grammar is ambiguous when there exists a valid XML instance that can be interpreted
//! (definitions mapped to elements) in more than one way.

use crate::model::{NameClass, Pattern};
use crate::restrictions;
use std::collections::{HashMap, HashSet, VecDeque};

/// Unique identifier for an Element pattern in the grammar.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LabelId(u32);

/// NFA state identifier.
type StateId = u32;

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
    fn concatenate(mut self, other: Nfa) -> Nfa {
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
        self.num_states += other.num_states;
        // Accept states are now only other's accept states
        let mut new_accept = vec![false; self.num_states as usize];
        for (s, &is_acc) in other.accept.iter().enumerate() {
            if is_acc {
                new_accept[(s as u32 + offset) as usize] = true;
            }
        }
        self.accept = new_accept;
        self
    }

    /// Choice: accepts any sequence accepted by any of the given NFAs.
    fn alternate(nfas: Vec<Nfa>) -> Nfa {
        if nfas.is_empty() {
            return Nfa::new_dead();
        }
        if nfas.len() == 1 {
            return nfas.into_iter().next().unwrap();
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
        result
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
    fn zero_or_more(mut self) -> Nfa {
        // Epsilon from each accept state back to start
        for acc in 0..self.num_states {
            if self.accept[acc as usize] {
                self.transitions[acc as usize].push((None, 0));
            }
        }
        // Start state is also accepting
        self.accept[0] = true;
        self
    }

    /// Optional: zero or one occurrence.
    fn optional(mut self) -> Nfa {
        self.accept[0] = true;
        self
    }

    /// Shuffle product (for interleave): accepts all interleavings of sequences
    /// from `self` and `other`.
    fn shuffle(self, other: &Nfa) -> Nfa {
        // States are (state_in_self, state_in_other) pairs.
        // We map pair (a, b) -> a * other.num_states + b
        let total_states = self.num_states * other.num_states;
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

        Nfa {
            num_states: total_states,
            transitions,
            accept,
        }
    }

    /// Returns true if the NFA accepts no strings (language is empty).
    #[allow(dead_code)]
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
fn pattern_to_nfa(
    pattern: &Pattern,
    ptr_to_label: &HashMap<usize, LabelId>,
    seen_refs: &mut HashSet<usize>,
) -> Nfa {
    match pattern {
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
            let nfas: Vec<Nfa> = children
                .iter()
                .map(|c| pattern_to_nfa(c, ptr_to_label, seen_refs))
                .collect();
            Nfa::alternate(nfas)
        }
        Pattern::Group(children, _) => {
            let mut result = Nfa::new_empty();
            for child in children {
                result = result.concatenate(pattern_to_nfa(child, ptr_to_label, seen_refs));
            }
            result
        }
        Pattern::Interleave(children, _) => {
            let mut iter = children.iter();
            let first = iter.next();
            match first {
                None => Nfa::new_empty(),
                Some(first) => {
                    let mut result = pattern_to_nfa(first, ptr_to_label, seen_refs);
                    for child in iter {
                        let child_nfa = pattern_to_nfa(child, ptr_to_label, seen_refs);
                        result = result.shuffle(&child_nfa);
                    }
                    result
                }
            }
        }
        Pattern::Mixed(body, _) => {
            // Mixed = interleave(text, body). Text contributes no element children,
            // so the NFA is just the body's NFA.
            pattern_to_nfa(body, ptr_to_label, seen_refs)
        }
        Pattern::OneOrMore(body, _) => pattern_to_nfa(body, ptr_to_label, seen_refs).one_or_more(),
        Pattern::ZeroOrMore(body, _) => {
            pattern_to_nfa(body, ptr_to_label, seen_refs).zero_or_more()
        }
        Pattern::Optional(body, _) => pattern_to_nfa(body, ptr_to_label, seen_refs).optional(),
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if !seen_refs.insert(ptr) {
                // Cycle — RELAX NG spec requires recursion through Element,
                // so this shouldn't happen in valid schemas. Return dead NFA.
                return Nfa::new_dead();
            }
            let result = {
                let borrowed = pat_ref.0.borrow();
                match borrowed.as_ref() {
                    Some(rule) => pattern_to_nfa(rule.pattern(), ptr_to_label, seen_refs),
                    None => Nfa::new_dead(),
                }
            };
            seen_refs.remove(&ptr);
            result
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
    }
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
) -> DtlaResult {
    let e1 = &elements[l1.0 as usize];
    let e2 = &elements[l2.0 as usize];

    // Different element names can never be confused
    if !restrictions::name_class_overlaps(&e1.name_class, &e2.name_class) {
        return DtlaResult::Unambiguous;
    }

    let nfa1 = &nfas[l1.0 as usize];
    let nfa2 = &nfas[l2.0 as usize];

    // Check M*: alphabet restricted to pairs where a==b or score(a,b)==Ambiguous
    let m_star_nonempty = product_nonempty(nfa1, nfa2, |a, b| {
        a == b || score.get(a, b) == Some(Score::Ambiguous)
    });

    if m_star_nonempty {
        return DtlaResult::Ambiguous;
    }

    // Check M+: alphabet includes undecided pairs too
    let m_plus_nonempty = product_nonempty(nfa1, nfa2, |a, b| {
        a == b || score.get(a, b) != Some(Score::Unambiguous)
    });

    if !m_plus_nonempty {
        return DtlaResult::Unambiguous;
    }

    DtlaResult::Undecidable
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
fn product_nonempty(nfa1: &Nfa, nfa2: &Nfa, allow: impl Fn(LabelId, LabelId) -> bool) -> bool {
    let n2 = nfa2.num_states;

    // Precompute epsilon closures for all states
    let eps1: Vec<Vec<StateId>> = (0..nfa1.num_states).map(|s| eps_closure(nfa1, s)).collect();
    let eps2: Vec<Vec<StateId>> = (0..nfa2.num_states).map(|s| eps_closure(nfa2, s)).collect();

    let trans1 = non_eps_transitions(nfa1);
    let trans2 = non_eps_transitions(nfa2);

    // Flat visited array indexed by s1 * n2 + s2
    let mut visited = vec![false; (nfa1.num_states * n2) as usize];
    let mut queue: VecDeque<(StateId, StateId)> = VecDeque::new();

    // Initialize with all pairs in eps_closure(start1) x eps_closure(start2)
    for &s1 in &eps1[0] {
        for &s2 in &eps2[0] {
            let idx = (s1 * n2 + s2) as usize;
            if !visited[idx] {
                visited[idx] = true;
                if nfa1.accept[s1 as usize] && nfa2.accept[s2 as usize] {
                    return true;
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
                                    return true;
                                }
                                queue.push_back((t1, t2));
                            }
                        }
                    }
                }
            }
        }
    }

    false
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
) -> bool {
    let e1 = &elements[l1.0 as usize];
    let e2 = &elements[l2.0 as usize];

    if !restrictions::name_class_overlaps(&e1.name_class, &e2.name_class) {
        return false;
    }

    let nfa1 = &nfas[l1.0 as usize];
    let nfa2 = &nfas[l2.0 as usize];

    // Build product automaton M* with the fully-resolved score matrix.
    // Check if there exists a reachable transition using (a, b) where a~b (ambiguous).
    catalyst_check(nfa1, nfa2, score)
}

/// Build the product automaton, trim right-unreachable states, and check
/// whether any transition uses a pair (a, b) where a != b and score(a,b) == Ambiguous.
fn catalyst_check(nfa1: &Nfa, nfa2: &Nfa, score: &ScoreMatrix) -> bool {
    let n2 = nfa2.num_states;
    let product_size = (nfa1.num_states as usize) * (n2 as usize);

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
        forward_edges_by_source[from_idx as usize].push(edge_idx as u32);
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
                return true;
            }
        }
    }

    false
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
pub fn check_ambiguity(start: &Pattern) -> Vec<AmbiguityWarning> {
    // Phase 1: Enumerate all Element patterns
    let (elements, ptr_to_label) = enumerate_elements(start);
    let n = elements.len();

    if n == 0 {
        return vec![];
    }

    // Phase 2: Build NFAs for each element's content model
    let nfas = build_content_nfas(start, &elements, &ptr_to_label);

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
                match dtla(li, lj, &elements, &nfas, &score) {
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
        return vec![];
    }

    // A grammar is only truly ambiguous if a catalyst exists — an element
    // whose content model can exercise an ambiguous pair.
    let has_catalyst = (0..n).any(|i| {
        (i..n).any(|j| {
            let li = LabelId(i as u32);
            let lj = LabelId(j as u32);
            (i == j
                || restrictions::name_class_overlaps(
                    &elements[i].name_class,
                    &elements[j].name_class,
                ))
                && is_catalyst(li, lj, &elements, &nfas, &score)
        })
    });

    if !has_catalyst {
        return vec![];
    }

    vec![AmbiguityWarning {
        kind: AmbiguityKind::AmbiguousGrammar { ambiguous_pairs },
    }]
}

/// Build NFA content models for each element by re-walking the pattern tree.
fn build_content_nfas(
    start: &Pattern,
    elements: &[ElementInfo],
    ptr_to_label: &HashMap<usize, LabelId>,
) -> Vec<Nfa> {
    let mut nfas: Vec<Option<Nfa>> = (0..elements.len()).map(|_| None).collect();
    let mut seen_refs = HashSet::new();
    build_nfas_walk(start, ptr_to_label, &mut nfas, &mut seen_refs);

    // Fill any missing NFAs with empty (shouldn't happen)
    nfas.into_iter()
        .map(|opt| opt.unwrap_or_else(Nfa::new_empty))
        .collect()
}

fn build_nfas_walk(
    pattern: &Pattern,
    ptr_to_label: &HashMap<usize, LabelId>,
    nfas: &mut [Option<Nfa>],
    seen_refs: &mut HashSet<usize>,
) {
    match pattern {
        Pattern::Element(_, body, _, _) => {
            let ptr = pattern as *const Pattern as usize;
            if let Some(&label) = ptr_to_label.get(&ptr) {
                let idx = label.0 as usize;
                if nfas[idx].is_none() {
                    let mut nfa_seen = HashSet::new();
                    nfas[idx] = Some(pattern_to_nfa(body, ptr_to_label, &mut nfa_seen));
                    // Continue into body to build NFAs for nested elements
                    build_nfas_walk(body, ptr_to_label, nfas, seen_refs);
                }
            }
        }
        Pattern::Choice(children, _)
        | Pattern::Interleave(children, _)
        | Pattern::Group(children, _) => {
            for child in children {
                build_nfas_walk(child, ptr_to_label, nfas, seen_refs);
            }
        }
        Pattern::OneOrMore(body, _)
        | Pattern::ZeroOrMore(body, _)
        | Pattern::Optional(body, _)
        | Pattern::Mixed(body, _)
        | Pattern::List(body, _) => {
            build_nfas_walk(body, ptr_to_label, nfas, seen_refs);
        }
        Pattern::Attribute(_, body, _, _) => {
            build_nfas_walk(body, ptr_to_label, nfas, seen_refs);
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    build_nfas_walk(rule.pattern(), ptr_to_label, nfas, seen_refs);
                }
            }
        }
        Pattern::DatatypeName { except, .. } => {
            if let Some(e) = except {
                build_nfas_walk(e, ptr_to_label, nfas, seen_refs);
            }
        }
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. } => {}
    }
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
        let ab = a.concatenate(b);
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
        let alt = Nfa::alternate(vec![a, b]);
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
        let star = a.zero_or_more();
        assert!(star.accepts(&[]));
        assert!(star.accepts(&[LabelId(0)]));
        assert!(star.accepts(&[LabelId(0), LabelId(0)]));
        assert!(!star.accepts(&[LabelId(1)]));
    }

    #[test]
    fn nfa_optional() {
        let a = Nfa::new_single(LabelId(0));
        let opt = a.optional();
        assert!(opt.accepts(&[]));
        assert!(opt.accepts(&[LabelId(0)]));
        assert!(!opt.accepts(&[LabelId(0), LabelId(0)]));
    }

    #[test]
    fn nfa_shuffle_two_symbols() {
        let a = Nfa::new_single(LabelId(0));
        let b = Nfa::new_single(LabelId(1));
        let shuf = a.shuffle(&b);
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
        let ab = Nfa::new_single(LabelId(0)).concatenate(Nfa::new_single(LabelId(1)));
        let c = Nfa::new_single(LabelId(2));
        let shuf = ab.shuffle(&c);
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
        let dead_concat = Nfa::new_single(LabelId(0)).concatenate(Nfa::new_dead());
        assert!(dead_concat.is_empty());
    }

    #[test]
    fn nfa_concatenation_with_empty() {
        // empty.a = a
        let ea = Nfa::new_empty().concatenate(Nfa::new_single(LabelId(0)));
        assert!(ea.accepts(&[LabelId(0)]));
        assert!(!ea.accepts(&[]));

        // a.empty = a
        let ae = Nfa::new_single(LabelId(0)).concatenate(Nfa::new_empty());
        assert!(ae.accepts(&[LabelId(0)]));
        assert!(!ae.accepts(&[]));
    }

    // -- Product automaton tests --

    #[test]
    fn product_identical_nfas_nonempty() {
        // Two identical NFAs accepting [a] — product with allow(a,a) should be non-empty
        let nfa = Nfa::new_single(LabelId(0));
        assert!(product_nonempty(&nfa, &nfa, |a, b| a == b));
    }

    #[test]
    fn product_disjoint_nfas_empty() {
        // NFA1 accepts [a], NFA2 accepts [b] — product with allow(x,y) only if x==y is empty
        let nfa1 = Nfa::new_single(LabelId(0));
        let nfa2 = Nfa::new_single(LabelId(1));
        assert!(!product_nonempty(&nfa1, &nfa2, |a, b| a == b));
    }

    #[test]
    fn product_overlapping_nfas() {
        // NFA1 accepts [a, b], NFA2 accepts [a, c]
        // Product with allow(x,y) if x==y should be non-empty (both start with a)
        // Wait, actually the product requires both to accept the SAME sequence length.
        // [a,b] vs [a,c]: pair (a,a) is ok, then (b,c) — only allowed if allow(b,c).
        let nfa1 = Nfa::new_single(LabelId(0)).concatenate(Nfa::new_single(LabelId(1)));
        let nfa2 = Nfa::new_single(LabelId(0)).concatenate(Nfa::new_single(LabelId(2)));
        // Only allow equal pairs
        assert!(!product_nonempty(&nfa1, &nfa2, |a, b| a == b));
        // Allow all pairs
        assert!(product_nonempty(&nfa1, &nfa2, |_, _| true));
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
        let warnings = check_ambiguity(&root);
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
        let warnings = check_ambiguity(&root);
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
        let warnings = check_ambiguity(&root);
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
        let warnings = check_ambiguity(&root);
        assert!(
            !warnings.is_empty(),
            "Overlapping content models should be ambiguous"
        );
    }
}
