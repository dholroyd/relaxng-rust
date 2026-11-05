use crate::model::datatype::Datatypes;
use fnv::FnvHashMap;
use std::collections::HashMap;

use crate::validator::infoset::NamespaceScope;
use crate::validator::nameclass::NameKey;
use crate::validator::schema::{Pat, PatId, Schema};

fn is_whitespace_char(c: char) -> bool {
    ['\x20', '\x09', '\x0d', '\x0a'].contains(&c)
}

pub(crate) fn is_whitespace_str(s: &str) -> bool {
    s.chars().all(is_whitespace_char)
}

/// Checks `text` against `dt`, resolving any QName prefix against the declarations in `ns`.
fn datatype_is_valid(dt: &Datatypes, text: &str, ns: &NamespaceScope<'_>) -> bool {
    dt.is_valid_with_ns(text, |p| ns.lookup_prefix(p).map(str::to_owned))
}

/// The state of one [`Schema::text_deriv`] call.
struct TextDerivCx<'t, 'n, 's> {
    text: &'t str,
    /// Whether `text` is whitespace-only (or empty).
    whitespace: bool,
    ns: &'n NamespaceScope<'s>,
    /// This call's memo: input PatId -> (result PatId, whether the result depends on the text).
    memo: FnvHashMap<PatId, (PatId, bool)>,
    /// Whether the derivative being computed depends on the text, beyond whether it is
    /// whitespace-only.  Only datatypes tell whitespace strings apart (e.g. by a length facet),
    /// so they set this.
    depends: bool,
}

impl Schema {
    pub(crate) fn text_deriv(&mut self, pid: PatId, text: &str, ns: &NamespaceScope<'_>) -> PatId {
        let mut cx = TextDerivCx {
            text,
            whitespace: is_whitespace_str(text),
            ns,
            memo: FnvHashMap::default(),
            depends: false,
        };
        self.text_deriv_memo(pid, &mut cx)
    }

    // Memoizes the recursion in Self::text_deriv_uncached.  Sub-patterns are shared, so
    // without this each one is re-derived once per path that reaches it, which is exponential
    // in the nesting depth.
    //
    // Results go into `whitespace_text_cache` when they hold for every whitespace-only text,
    // since that case recurs in nearly every context and so is worth keeping across calls.
    // Everything else goes into `cx.memo`, which lasts for this `text_deriv` call only.  See
    // [`TextDerivCx::depends`] for how the two cases are told apart.
    fn text_deriv_memo(&mut self, pid: PatId, cx: &mut TextDerivCx<'_, '_, '_>) -> PatId {
        let pat = self.patt(pid);
        // Only patterns that recurse into sub-patterns are memoized.  Since every such parent is
        // memoized, a leaf (including Element and Attribute, whose derivative never looks inside
        // them) is derived at most once per distinct parent anyway, too little to repay an insert.
        if !Self::is_recursive_pattern(pat) {
            return self.text_deriv_uncached(pid, pat, cx);
        }
        if cx.whitespace
            && let Some(cached) = self.whitespace_text_cache_entry(pid)
        {
            return cached;
        }
        if let Some(&(cached, depends)) = cx.memo.get(&pid) {
            // Re-apply the flag so every parent of a shared child sees the dependency, not just
            // the first one to derive it.
            cx.depends |= depends;
            return cached;
        }
        // Find whether this pattern's own derivative depends on the text, then fold that into
        // the enclosing pattern's.
        let outer = std::mem::replace(&mut cx.depends, false);
        let result = self.text_deriv_uncached(pid, pat, cx);
        let depends = cx.depends;
        cx.depends |= outer;
        if cx.whitespace && !depends {
            // Keyed on the pattern alone, so only results that are the same for every
            // whitespace-only text can go here.
            let mut inner = self.inner.borrow_mut();
            let idx = pid.0 as usize;
            if idx >= inner.whitespace_text_cache.len() {
                inner.whitespace_text_cache.resize(idx + 1, None);
            }
            inner.whitespace_text_cache[idx] = Some(result);
        } else {
            cx.memo.insert(pid, (result, depends));
        }
        result
    }

    fn whitespace_text_cache_entry(&self, pid: PatId) -> Option<PatId> {
        self.inner
            .borrow()
            .whitespace_text_cache
            .get(pid.0 as usize)
            .copied()
            .flatten()
    }

    fn is_recursive_pattern(pat: Pat) -> bool {
        matches!(
            pat,
            Pat::Choice(..)
                | Pat::Interleave(..)
                | Pat::Group(..)
                | Pat::After(..)
                | Pat::OneOrMore(..)
                | Pat::DatatypeExcept(..)
                | Pat::List(..)
        )
    }

    /// Returns the derivative of `pid` (whose pattern is `pat`) with respect to `cx.text`,
    /// setting `cx.depends` if a datatype was consulted.
    fn text_deriv_uncached(
        &mut self,
        pid: PatId,
        pat: Pat,
        cx: &mut TextDerivCx<'_, '_, '_>,
    ) -> PatId {
        let text = cx.text;
        let ns = cx.ns;
        // Where the text leaves every sub-pattern unchanged (as whitespace usually does), the
        // composite arms return `pid` itself rather than rebuilding an equivalent pattern.  A
        // rebuilt pattern would be new to every cache, and the states reached from it multiply
        // with each text node until the pattern table overflows.
        match pat {
            Pat::Choice(p1, p2, _) => {
                let a = self.text_deriv_memo(p1, cx);
                let b = self.text_deriv_memo(p2, cx);
                if a == p1 && b == p2 {
                    return pid;
                }
                self.choice(a, b)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = self.text_deriv_memo(p1, cx);
                let d2 = self.text_deriv_memo(p2, cx);
                if d1 == p1 && d2 == p2 {
                    return pid;
                }
                let a = self.interleave(d1, p2);
                let b = self.interleave(p1, d2);
                self.choice(a, b)
            }
            Pat::Group(p1, p2, _) => {
                let nullable = self.patt(p1).is_nullable();
                let d1 = self.text_deriv_memo(p1, cx);
                let p = self.group(d1, p2);
                if nullable {
                    let d2 = self.text_deriv_memo(p2, cx);
                    // p1 is nullable, so `p1, p2` already matches everything `p2` does.
                    if d1 == p1 && d2 == p2 {
                        return pid;
                    }
                    self.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::After(p1, p2) => {
                let d = self.text_deriv_memo(p1, cx);
                self.after(d, p2)
            }
            Pat::OneOrMore(p, _) => {
                let d = self.text_deriv_memo(p, cx);
                // `p, (p+ | empty)` matches the same as `p+`.
                if d == p {
                    return pid;
                }
                self.group(d, self.choice(self.one_or_more(p), self.empty()))
            }
            Pat::Text => {
                self.mark_covered(pid);
                self.text()
            }
            Pat::Datatype(dt) => {
                cx.depends = true;
                if datatype_is_valid(self.datatype(dt), text, ns) {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::DatatypeValue(dt) => {
                cx.depends = true;
                let dt = self.datatype_value(dt);
                let valid = dt.is_valid_with_ns(text, ns.default_namespace().unwrap_or(""), |p| {
                    ns.lookup_prefix(p).map(str::to_owned)
                });
                if valid {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::DatatypeExcept(dt, except) => {
                cx.depends = true;
                let d = self.text_deriv_memo(except, cx);
                let pat2 = self.patt(d);
                if datatype_is_valid(self.datatype(dt), text, ns) && !pat2.is_nullable() {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::List(p) => {
                let mut p = p;
                for item in text.split_whitespace() {
                    p = self.text_deriv(p, item, ns);
                    if let Pat::NotAllowed = self.patt(p) {
                        return p;
                    }
                }
                let last_patt = self.patt(p);
                if let Pat::Empty = last_patt {
                    p
                } else if last_patt.is_nullable() {
                    // List is not able to be nullable per https://relaxng.org/jclark/derivative.html
                    // but that definition assumes that we can see all text content up-front
                    // whereas processing instructions CDATA sections etc may mean we see
                    // text children piecemeal here.  To accommodate this, we make the list
                    // optional here (TODO: should we rather adjust List to be nullable?)
                    self.choice(self.list(p), self.empty())
                } else {
                    self.list(p)
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
                if cx.whitespace {
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::NotAllowed | Pat::Attribute(_, _) => self.not_allowed(),
            Pat::Element(_, _) => {
                if cx.whitespace {
                    pid
                } else {
                    self.not_allowed()
                }
            }
        }
    }

    // Per https://relaxng.org/jclark/derivative.html — text nodes in mixed content can only
    // match Text patterns (RELAX NG spec §7.2).  This lets us memoize on PatId alone, ignoring
    // the actual text value.  Returns the same PatId for patterns where text is a fixed-point
    // (e.g. After(Text, cont)), enabling a fast skip of text_deriv at call sites.
    pub(crate) fn mixed_text_deriv(&mut self, pid: PatId) -> PatId {
        let idx = pid.0 as usize;
        {
            let inner = self.inner.borrow();
            if let Some(Some(cached)) = inner.mixed_text_cache.get(idx) {
                return *cached;
            }
        }
        let pat = self.patt(pid);
        let result = match pat {
            Pat::Choice(p1, p2, _) => {
                let c1 = self.mixed_text_deriv(p1);
                let c2 = self.mixed_text_deriv(p2);
                self.choice(c1, c2)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = self.mixed_text_deriv(p1);
                let c1 = self.interleave(d1, p2);
                let d2 = self.mixed_text_deriv(p2);
                let c2 = self.interleave(p1, d2);
                self.choice(c1, c2)
            }
            Pat::After(p1, p2) => {
                let d = self.mixed_text_deriv(p1);
                self.after(d, p2)
            }
            Pat::Group(p1, p2, _) => {
                let nullable = self.patt(p1).is_nullable();
                let d1 = self.mixed_text_deriv(p1);
                let p = self.group(d1, p2);
                if nullable {
                    let d2 = self.mixed_text_deriv(p2);
                    self.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::OneOrMore(p, _) => {
                let d = self.mixed_text_deriv(p);
                self.group(d, self.choice(self.one_or_more(p), self.empty()))
            }
            Pat::Text => {
                self.mark_covered(pid);
                pid
            }
            _ => self.not_allowed(),
        };
        let mut inner = self.inner.borrow_mut();
        if idx >= inner.mixed_text_cache.len() {
            inner.mixed_text_cache.resize(idx + 1, None);
        }
        inner.mixed_text_cache[idx] = Some(result);
        result
    }

    pub(crate) fn start_tag_open_deriv(&mut self, pid: PatId, name: NameKey) -> PatId {
        // Cache check (borrow released before any mutation)
        {
            let inner = self.inner.borrow();
            if let Some(&cached) = inner.start_tag_open_cache.get(&(pid, name)) {
                return cached;
            }
        }

        let current = self.patt(pid);
        let result = match current {
            Pat::Choice(l, r, _) => {
                let d1 = self.start_tag_open_deriv(l, name);
                let d2 = self.start_tag_open_deriv(r, name);
                self.choice(d1, d2)
            }
            Pat::OneOrMore(inner_pid, _) => {
                let deriv = self.start_tag_open_deriv(inner_pid, name);
                self.apply_after(self.patt(deriv), |pat, s| {
                    s.group(pat, s.choice(s.one_or_more(inner_pid), s.empty()))
                })
            }
            Pat::Interleave(pid1, pid2, _) => {
                let d1 = self.start_tag_open_deriv(pid1, name);
                let c1 = self.apply_after(self.patt(d1), |pat, s| s.interleave(pat, pid2));
                let d2 = self.start_tag_open_deriv(pid2, name);
                let c2 = self.apply_after(self.patt(d2), |pat, s| s.interleave(pid1, pat));
                self.choice(c1, c2)
            }
            Pat::Group(pid1, pid2, _) => {
                let nullable = self.patt(pid1).is_nullable();
                let d1 = self.start_tag_open_deriv(pid1, name);
                let x = self.apply_after(self.patt(d1), |pat, s| s.group(pat, pid2));
                if nullable {
                    let d2 = self.start_tag_open_deriv(pid2, name);
                    self.choice(x, d2)
                } else {
                    x
                }
            }
            Pat::Element(nc, inner_pat) => {
                if self.name_matcher(nc).matches(name) {
                    self.mark_covered(pid);
                    let empty = self.empty();
                    self.after(inner_pat, empty)
                } else {
                    self.not_allowed()
                }
            }
            Pat::After(pid1, pid2) => {
                let d = self.start_tag_open_deriv(pid1, name);
                self.apply_after(self.patt(d), |pat, s| s.after(pat, pid2))
            }

            Pat::Empty
            | Pat::Text
            | Pat::NotAllowed
            | Pat::Attribute(_, _)
            | Pat::Datatype(_)
            | Pat::DatatypeValue(_)
            | Pat::DatatypeExcept(_, _)
            | Pat::List(_) => self.not_allowed(),
        };

        self.inner
            .borrow_mut()
            .start_tag_open_cache
            .insert((pid, name), result);
        result
    }

    pub(crate) fn start_att_deriv(&mut self, pid: PatId, name: NameKey) -> PatId {
        {
            let inner = self.inner.borrow();
            if let Some(&cached) = inner.start_att_cache.get(&(pid, name)) {
                return cached;
            }
        }

        let current = self.patt(pid);
        let result = match current {
            Pat::Choice(l, r, _) => {
                let d1 = self.start_att_deriv(l, name);
                let d2 = self.start_att_deriv(r, name);
                self.choice(d1, d2)
            }
            Pat::OneOrMore(inner_pid, _) => {
                let deriv = self.start_att_deriv(inner_pid, name);
                self.apply_after(self.patt(deriv), |pat, s| {
                    s.group(pat, s.choice(s.one_or_more(inner_pid), s.empty()))
                })
            }
            Pat::Interleave(pid1, pid2, _) => {
                let d1 = self.start_att_deriv(pid1, name);
                let c1 = self.apply_after(self.patt(d1), |pat, s| s.interleave(pat, pid2));
                let d2 = self.start_att_deriv(pid2, name);
                let c2 = self.apply_after(self.patt(d2), |pat, s| s.interleave(pid1, pat));
                self.choice(c1, c2)
            }
            Pat::Group(pid1, pid2, _) => {
                // Attributes may appear in any order, so unlike start_tag_open_deriv
                // we always try both branches unconditionally.
                let d1 = self.start_att_deriv(pid1, name);
                let x = self.apply_after(self.patt(d1), |pat, s| s.group(pat, pid2));
                let d2 = self.start_att_deriv(pid2, name);
                let y = self.apply_after(self.patt(d2), |pat, s| s.group(pid1, pat));
                self.choice(x, y)
            }
            Pat::Attribute(nc, val_pat) => {
                if self.name_matcher(nc).matches(name) {
                    self.mark_covered(pid);
                    let empty = self.empty();
                    self.after(val_pat, empty)
                } else {
                    self.not_allowed()
                }
            }
            Pat::After(pid1, pid2) => {
                let d = self.start_att_deriv(pid1, name);
                self.apply_after(self.patt(d), |pat, s| s.after(pat, pid2))
            }
            _ => self.not_allowed(),
        };

        self.inner
            .borrow_mut()
            .start_att_cache
            .insert((pid, name), result);
        result
    }

    pub(crate) fn att_value_deriv(
        &mut self,
        pid: PatId,
        value: &str,
        ns: &NamespaceScope<'_>,
    ) -> PatId {
        let pat = self.patt(pid);
        match pat {
            Pat::After(val_pat, cont) => {
                if self.value_match(val_pat, value, ns) {
                    cont
                } else {
                    self.not_allowed()
                }
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = self.att_value_deriv(p1, value, ns);
                let c2 = self.att_value_deriv(p2, value, ns);
                self.choice(c1, c2)
            }
            _ => self.not_allowed(),
        }
    }

    // in the spec, the applyAfter() 'f' argument comes before the pattern, in rust it's more
    // convenient if the 'f' argument is last in the list
    pub(crate) fn apply_after<F>(&mut self, pat: Pat, f: F) -> PatId
    where
        F: Fn(PatId, &mut Schema) -> PatId + Clone,
    {
        match pat {
            Pat::After(p1, p2) => {
                let p2 = f(p2, self);
                self.after(p1, p2)
            }
            Pat::Choice(p1, p2, _) => {
                let p1 = self.patt(p1);
                let p2 = self.patt(p2);
                let c1 = self.apply_after(p1, f.clone());
                let c2 = self.apply_after(p2, f);
                self.choice(c1, c2)
            }
            Pat::NotAllowed => self.not_allowed(),
            _ => panic!(
                "Only 'Choice', 'Interleave' or 'NotAllowed' patterns may be passed to apply_after(): {pat:?}"
            ),
        }
    }

    pub(crate) fn value_match(&mut self, pid: PatId, val: &str, ns: &NamespaceScope<'_>) -> bool {
        let pat = self.patt(pid);
        if pat.is_nullable() && is_whitespace_str(val) {
            true
        } else {
            let d = self.text_deriv(pid, val, ns);
            self.patt(d).is_nullable()
        }
    }

    /// See [`Self::force_start_tag_close_deriv`] for the error-recovery counterpart of this
    /// function — keep the `Attribute` arm of the two in sync; every other arm must stay
    /// structurally identical.
    pub(crate) fn start_tag_close_deriv(&mut self, pid: PatId) -> PatId {
        {
            let inner = self.inner.borrow();
            if let Some(&cached) = inner.start_tag_close_cache.get(&pid) {
                return cached;
            }
        }
        let pat = self.patt(pid);
        let result = match pat {
            Pat::After(p1, p2) => {
                let a1 = self.start_tag_close_deriv(p1);
                self.after(a1, p2)
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = self.start_tag_close_deriv(p1);
                let c2 = self.start_tag_close_deriv(p2);
                self.choice(c1, c2)
            }
            Pat::Group(p1, p2, _) => {
                let c1 = self.start_tag_close_deriv(p1);
                let c2 = self.start_tag_close_deriv(p2);
                self.group(c1, c2)
            }
            Pat::Interleave(p1, p2, _) => {
                let c1 = self.start_tag_close_deriv(p1);
                let c2 = self.start_tag_close_deriv(p2);
                self.interleave(c1, c2)
            }
            Pat::OneOrMore(p, _) => {
                let _o = self.start_tag_close_deriv(p);
                self.one_or_more(p)
            }
            Pat::Attribute(_, _) => self.not_allowed(),
            _ => pid,
        };
        self.inner
            .borrow_mut()
            .start_tag_close_cache
            .insert(pid, result);
        result
    }

    /// As [`Self::start_tag_close_deriv`], but treats still-outstanding required attributes as
    /// satisfied rather than as an error. Keep the `Attribute` arm of the two functions in
    /// sync with each other; every other arm must stay structurally identical.
    ///
    /// This is only used to recover a usable content-model position after an error has
    /// already been reported for this start tag, so that the element's children can still be
    /// checked. It deliberately shares no cache with `start_tag_close_deriv`: that function is
    /// always called first and will have memoized `NotAllowed` for these same pattern ids, so a
    /// shared cache would short-circuit straight back to `NotAllowed`. It keeps its own
    /// call-local memo instead — required because, unlike `start_tag_close_deriv`, this
    /// function recurses into *both* children of `Group`/`Interleave`/`Choice` regardless of
    /// nullability, so an unmemoized walk of a heavily-shared pattern DAG can be exponential in
    /// the DAG's depth.
    pub(crate) fn force_start_tag_close_deriv(&mut self, pid: PatId) -> PatId {
        self.force_start_tag_close_deriv_memo(pid, &mut HashMap::new())
    }

    fn force_start_tag_close_deriv_memo(
        &mut self,
        pid: PatId,
        memo: &mut HashMap<PatId, PatId>,
    ) -> PatId {
        if let Some(&cached) = memo.get(&pid) {
            return cached;
        }
        let result = match self.patt(pid) {
            Pat::After(p1, p2) => {
                let a1 = self.force_start_tag_close_deriv_memo(p1, memo);
                self.after(a1, p2)
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = self.force_start_tag_close_deriv_memo(p1, memo);
                let c2 = self.force_start_tag_close_deriv_memo(p2, memo);
                self.choice(c1, c2)
            }
            Pat::Group(p1, p2, _) => {
                let c1 = self.force_start_tag_close_deriv_memo(p1, memo);
                let c2 = self.force_start_tag_close_deriv_memo(p2, memo);
                self.group(c1, c2)
            }
            Pat::Interleave(p1, p2, _) => {
                let c1 = self.force_start_tag_close_deriv_memo(p1, memo);
                let c2 = self.force_start_tag_close_deriv_memo(p2, memo);
                self.interleave(c1, c2)
            }
            // Mirrors `start_tag_close_deriv`, which likewise leaves the body untouched; an
            // attribute nested under a repeat is therefore not force-matched here either.
            Pat::OneOrMore(p, _) => self.one_or_more(p),
            Pat::Attribute(_, _) => self.empty(),
            _ => pid,
        };
        memo.insert(pid, result);
        result
    }

    // Note: the spec lists endTagDeriv as efficiently memoizable, but benchmarking showed
    // that both HashMap and Vec caches regressed performance by 7-15%. The function body
    // is only ~3 ops (RefCell borrow + array index + match), so the RefCell borrow overhead
    // of any cache lookup exceeds the savings from avoiding recomputation.
    //
    // See `force_end_tag_deriv` below for the error-recovery counterpart of this function —
    // keep the two structurally in sync.
    pub(crate) fn end_tag_deriv(&mut self, pid: PatId) -> PatId {
        let pat = self.patt(pid);
        match pat {
            Pat::Choice(p1, p2, _) => {
                let c1 = self.end_tag_deriv(p1);
                let c2 = self.end_tag_deriv(p2);
                self.choice(c1, c2)
            }
            Pat::After(p1, p2) => {
                if self.patt(p1).is_nullable() {
                    p2
                } else {
                    self.not_allowed()
                }
            }
            _ => self.not_allowed(),
        }
    }

    /// As [`Self::end_tag_deriv`], but treats the element's outstanding content requirements
    /// (e.g. a missing required child) as already satisfied instead of leaving it unclosed.
    /// Mirrors `end_tag_deriv`'s structure with only the `After` arm's nullability check
    /// dropped — keep the two in sync.
    ///
    /// Used only to recover a continuation position for validating what follows a self-closing
    /// element that failed to close, after that element's own error has already been reported.
    /// Never used on the happy path. Not memoized, matching `end_tag_deriv` above: by this
    /// point the pattern is Choice/After nesting only (no Group/Interleave/OneOrMore, which are
    /// already gone by the time a start tag has closed), so an unmemoized walk stays bounded by
    /// the same reasoning that already applies to `end_tag_deriv`.
    pub(crate) fn force_end_tag_deriv(&mut self, pid: PatId) -> PatId {
        match self.patt(pid) {
            Pat::Choice(p1, p2, _) => {
                let c1 = self.force_end_tag_deriv(p1);
                let c2 = self.force_end_tag_deriv(p2);
                self.choice(c1, c2)
            }
            Pat::After(_, p2) => p2,
            _ => self.not_allowed(),
        }
    }

    /// Complete a self-closing element from `pid`.
    ///
    /// Per <https://relaxng.org/jclark/derivative.html>, "The case where the list of children
    /// is empty is treated as if there were a text node whose value were the empty string" —
    /// required for a pattern like `element foo { token }` to match `<foo/>` or `<foo></foo>`.
    ///
    /// When `force` is true and the strict derivation still doesn't close, additionally tries
    /// [`Self::force_end_tag_deriv`] so that outstanding content requirements are treated as
    /// satisfied. Only meaningful on the error-recovery path, once this element's own error has
    /// already been reported — forcing it on the happy path would let genuinely-incomplete
    /// content through undetected.
    pub(crate) fn close_empty_element(
        &mut self,
        pid: PatId,
        force: bool,
        ns: &NamespaceScope<'_>,
    ) -> PatId {
        let p = self.text_deriv(pid, "", ns);
        let closed = self.end_tag_deriv(p);
        if force && matches!(self.patt(closed), Pat::NotAllowed) {
            self.force_end_tag_deriv(p)
        } else {
            closed
        }
    }
}
