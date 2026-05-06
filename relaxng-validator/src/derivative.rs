use relaxng_model::datatype::Datatype;

use crate::nameclass::{QualifiedName, contains};
use crate::schema::{Pat, PatId, Schema};

fn is_whitespace_char(c: char) -> bool {
    ['\x20', '\x09', '\x0d', '\x0a'].contains(&c)
}

pub(crate) fn is_whitespace_str(s: &str) -> bool {
    s.chars().all(is_whitespace_char)
}

impl Schema {
    pub(crate) fn text_deriv(&mut self, pid: PatId, text: &str) -> PatId {
        let current = self.patt(pid);
        match current {
            Pat::Choice(p1, p2, _) => {
                let a = self.text_deriv(p1, text);
                let b = self.text_deriv(p2, text);
                self.choice(a, b)
            }
            Pat::Interleave(p1, p2, _) => {
                let d1 = self.text_deriv(p1, text);
                let a = self.interleave(d1, p2);

                let d2 = self.text_deriv(p2, text);
                let b = self.interleave(p1, d2);
                self.choice(a, b)
            }
            Pat::Group(p1, p2, _) => {
                let nullable = self.patt(p1).is_nullable();
                let d1 = self.text_deriv(p1, text);
                let p = self.group(d1, p2);
                if nullable {
                    let d2 = self.text_deriv(p2, text);
                    self.choice(p, d2)
                } else {
                    p
                }
            }
            Pat::After(p1, p2) => {
                let d = self.text_deriv(p1, text);
                self.after(d, p2)
            }
            Pat::OneOrMore(p, _) => {
                let d = self.text_deriv(p, text);
                self.group(d, self.choice(self.one_or_more(p), self.empty()))
            }
            Pat::Text => {
                self.mark_covered(pid);
                self.text()
            }
            Pat::Datatype(dt) => {
                if dt.is_valid(text) {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::DatatypeValue(dt) => {
                let valid = if let Some(ref ns_ctx) = self.ns_context {
                    let ns_ctx = ns_ctx.clone();
                    dt.is_valid_with_ns(text, &ns_ctx.default_ns, |p| ns_ctx.lookup(p))
                } else {
                    dt.is_valid(text)
                };
                if valid {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::DatatypeExcept(dt, except) => {
                let d = self.text_deriv(except, text);
                let pat2 = self.patt(d);
                if dt.is_valid(text) && !pat2.is_nullable() {
                    self.mark_covered(pid);
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::List(p) => {
                let mut p = p;
                for item in text.split_whitespace() {
                    p = self.text_deriv(p, item);
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
                if is_whitespace_str(text) {
                    self.empty()
                } else {
                    self.not_allowed()
                }
            }
            Pat::NotAllowed | Pat::Attribute(_, _) => self.not_allowed(),
            Pat::Element(_, _) => {
                if is_whitespace_str(text) {
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

    pub(crate) fn start_tag_open_deriv(&mut self, pid: PatId, name: QualifiedName) -> PatId {
        let local_key: Box<[u8]> = name.local_name.into();
        let ns_key = self.ns_interner.intern_opt(name.namespace_uri);

        // Cache check (borrow released before any mutation)
        {
            let inner = self.inner.borrow();
            if let Some(&cached) = inner
                .start_tag_open_cache
                .get(&(pid, local_key.clone(), ns_key))
            {
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
            Pat::Element(ref nc, inner_pat) => {
                if contains(nc, &name) {
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
            .insert((pid, local_key, ns_key), result);
        result
    }

    pub(crate) fn start_att_deriv(&mut self, pid: PatId, name: QualifiedName) -> PatId {
        let local_key: Box<[u8]> = name.local_name.into();
        let ns_key = self.ns_interner.intern_opt(name.namespace_uri);

        {
            let inner = self.inner.borrow();
            if let Some(&cached) = inner.start_att_cache.get(&(pid, local_key.clone(), ns_key)) {
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
            Pat::Attribute(ref nc, val_pat) => {
                if contains(nc, &name) {
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
            .insert((pid, local_key, ns_key), result);
        result
    }

    pub(crate) fn att_value_deriv(&mut self, pid: PatId, value: &str) -> PatId {
        let pat = self.patt(pid);
        match pat {
            Pat::After(val_pat, cont) => {
                if self.value_match(val_pat, value) {
                    cont
                } else {
                    self.not_allowed()
                }
            }
            Pat::Choice(p1, p2, _) => {
                let c1 = self.att_value_deriv(p1, value);
                let c2 = self.att_value_deriv(p2, value);
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

    pub(crate) fn value_match(&mut self, pid: PatId, val: &str) -> bool {
        let pat = self.patt(pid);
        if pat.is_nullable() && is_whitespace_str(val) {
            true
        } else {
            let d = self.text_deriv(pid, val);
            self.patt(d).is_nullable()
        }
    }

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

    // Note: the spec lists endTagDeriv as efficiently memoizable, but benchmarking showed
    // that both HashMap and Vec caches regressed performance by 7-15%. The function body
    // is only ~3 ops (RefCell borrow + array index + match), so the RefCell borrow overhead
    // of any cache lookup exceeds the savings from avoiding recomputation.
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
}
