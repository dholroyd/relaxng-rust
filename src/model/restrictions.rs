use crate::model::{NameClass, Pattern};
use codemap::Span;
use std::collections::{HashMap, HashSet};

/// Content type per RelaxNG spec section 7.2
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ContentType {
    Empty,
    Simple,
    Complex,
}

/// Tracks ancestor context for section 7.1 prohibited-path checks
#[derive(Clone, Copy)]
struct Flags {
    in_attribute: bool,
    in_list: bool,
    in_data_except: bool,
    in_start: bool,
    in_one_or_more: bool,
    in_one_or_more_group: bool,
}
impl Flags {
    fn start() -> Flags {
        Flags {
            in_attribute: false,
            in_list: false,
            in_data_except: false,
            in_start: true,
            in_one_or_more: false,
            in_one_or_more_group: false,
        }
    }

    fn enter_attribute(self) -> Flags {
        Flags {
            in_attribute: true,
            ..self
        }
    }

    fn enter_list(self) -> Flags {
        Flags {
            in_list: true,
            ..self
        }
    }

    fn enter_data_except(self) -> Flags {
        Flags {
            in_data_except: true,
            ..self
        }
    }

    fn enter_one_or_more(self) -> Flags {
        Flags {
            in_one_or_more: true,
            ..self
        }
    }

    fn enter_group_or_interleave(self) -> Flags {
        Flags {
            in_one_or_more_group: self.in_one_or_more || self.in_one_or_more_group,
            ..self
        }
    }

    fn enter_element(self) -> Flags {
        Flags {
            in_attribute: false,
            in_list: false,
            in_data_except: false,
            in_start: false,
            in_one_or_more: false,
            in_one_or_more_group: false,
        }
    }
}

/// Description of what restriction was violated
#[derive(Debug)]
pub enum RestrictionKind {
    /// 7.1.1: attribute//attribute
    AttributeContainsAttribute,
    /// 7.1.2: oneOrMore//group//attribute or oneOrMore//interleave//attribute
    DuplicateAttribute,
    /// 7.1.3: list restrictions
    ListContainsList,
    ListContainsAttribute,
    ListContainsText,
    ListContainsInterleave,
    ListContainsElement,
    /// 7.1.4: data/except restrictions
    DataExceptContainsAttribute,
    DataExceptContainsText,
    DataExceptContainsList,
    DataExceptContainsGroup,
    DataExceptContainsInterleave,
    DataExceptContainsOneOrMore,
    DataExceptContainsEmpty,
    DataExceptContainsElement,
    /// 7.1.5: start restrictions
    StartContainsAttribute,
    StartContainsData,
    StartContainsValue,
    StartContainsText,
    StartContainsList,
    StartContainsGroup,
    StartContainsInterleave,
    StartContainsOneOrMore,
    StartContainsEmpty,
    /// 7.2: string sequence (group/interleave of incompatible content types)
    StringSequence,
    /// 7.3: attributes in interleave/group with overlapping names
    AttributeNameOverlap,
    /// 7.3: attribute with infinite name class not in oneOrMore
    AttributeInfiniteNameClass,
    /// 7.4: interleave of elements with overlapping names
    InterleaveElementOverlap,
    /// 7.4: interleave of two text patterns
    InterleaveTextOverlap,
    /// Name class restriction: anyName/except contains anyName
    AnyNameExceptContainsAnyName,
    /// Name class restriction: nsName/except contains nsName or anyName
    NsNameExceptContainsNsNameOrAnyName,
    /// Attribute with ns="" has name "xmlns"
    AttributeNameXmlns,
    /// Attribute name class has ns="http://www.w3.org/2000/xmlns"
    AttributeNamespaceXmlns,
}

impl std::fmt::Display for RestrictionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RestrictionKind::AttributeContainsAttribute => {
                write!(f, "attribute must not contain attribute (section 7.1.1)")
            }
            RestrictionKind::DuplicateAttribute => {
                write!(f, "duplicate attribute in oneOrMore (section 7.1.2)")
            }
            RestrictionKind::ListContainsList => {
                write!(f, "list must not contain list (section 7.1.3)")
            }
            RestrictionKind::ListContainsAttribute => {
                write!(f, "list must not contain attribute (section 7.1.3)")
            }
            RestrictionKind::ListContainsText => {
                write!(f, "list must not contain text (section 7.1.3)")
            }
            RestrictionKind::ListContainsInterleave => {
                write!(f, "list must not contain interleave (section 7.1.3)")
            }
            RestrictionKind::ListContainsElement => {
                write!(f, "list must not contain element (section 7.1.3)")
            }
            RestrictionKind::DataExceptContainsAttribute => {
                write!(f, "data/except must not contain attribute (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsText => {
                write!(f, "data/except must not contain text (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsList => {
                write!(f, "data/except must not contain list (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsGroup => {
                write!(f, "data/except must not contain group (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsInterleave => {
                write!(f, "data/except must not contain interleave (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsOneOrMore => {
                write!(f, "data/except must not contain oneOrMore (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsEmpty => {
                write!(f, "data/except must not contain empty (section 7.1.4)")
            }
            RestrictionKind::DataExceptContainsElement => {
                write!(f, "data/except must not contain element (section 7.1.4)")
            }
            RestrictionKind::StartContainsAttribute => {
                write!(f, "start must not contain attribute (section 7.1.5)")
            }
            RestrictionKind::StartContainsData => {
                write!(f, "start must not contain data (section 7.1.5)")
            }
            RestrictionKind::StartContainsValue => {
                write!(f, "start must not contain value (section 7.1.5)")
            }
            RestrictionKind::StartContainsText => {
                write!(f, "start must not contain text (section 7.1.5)")
            }
            RestrictionKind::StartContainsList => {
                write!(f, "start must not contain list (section 7.1.5)")
            }
            RestrictionKind::StartContainsGroup => {
                write!(f, "start must not contain group (section 7.1.5)")
            }
            RestrictionKind::StartContainsInterleave => {
                write!(f, "start must not contain interleave (section 7.1.5)")
            }
            RestrictionKind::StartContainsOneOrMore => {
                write!(f, "start must not contain oneOrMore (section 7.1.5)")
            }
            RestrictionKind::StartContainsEmpty => {
                write!(f, "start must not contain empty (section 7.1.5)")
            }
            RestrictionKind::StringSequence => write!(
                f,
                "incompatible content types in group/interleave (section 7.2)"
            ),
            RestrictionKind::AttributeNameOverlap => {
                write!(f, "overlapping attribute names (section 7.3)")
            }
            RestrictionKind::AttributeInfiniteNameClass => {
                write!(f, "attribute with infinite name class (section 7.3)")
            }
            RestrictionKind::InterleaveElementOverlap => write!(
                f,
                "interleave contains elements with overlapping names (section 7.4)"
            ),
            RestrictionKind::InterleaveTextOverlap => write!(
                f,
                "interleave contains multiple text patterns (section 7.4)"
            ),
            RestrictionKind::AnyNameExceptContainsAnyName => {
                write!(f, "anyName/except must not contain anyName")
            }
            RestrictionKind::NsNameExceptContainsNsNameOrAnyName => {
                write!(f, "nsName/except must not contain nsName or anyName")
            }
            RestrictionKind::AttributeNameXmlns => write!(
                f,
                "attribute with empty namespace must not be named 'xmlns'"
            ),
            RestrictionKind::AttributeNamespaceXmlns => {
                write!(f, "attribute name must not use XMLNS namespace")
            }
        }
    }
}

/// A restriction violation, with the location of the offending pattern
#[derive(Debug)]
pub struct Violation {
    pub kind: RestrictionKind,
    /// The pattern that breaks the restriction
    pub span: Span,
    /// For overlap restrictions, the earlier pattern that the one at `span` overlaps with
    pub other_span: Option<Span>,
}

impl Violation {
    fn at(kind: RestrictionKind, span: Span) -> Violation {
        Violation {
            kind,
            span,
            other_span: None,
        }
    }
}

/// A pattern found within some branch of a group or interleave, by name class where it has one
struct Found<T> {
    item: T,
    span: Span,
}

/// Check all section 7 restrictions on a compiled schema.
pub fn check_restrictions(start: &Pattern) -> Result<(), Violation> {
    let mut checker = Checker {
        content_types: HashMap::new(),
        not_allowed: HashMap::new(),
    };
    checker.walk(Flags::start(), start)?;
    Ok(())
}

/// Check if a name class is "infinite" (contains AnyName or NsName)
fn is_infinite_name_class(nc: &NameClass) -> bool {
    match nc {
        NameClass::AnyName { .. } | NameClass::NsName { .. } => true,
        NameClass::Alt { a, b } => is_infinite_name_class(a) || is_infinite_name_class(b),
        NameClass::Named { .. } => false,
    }
}

fn ref_key(pat_ref: &crate::model::PatRef) -> usize {
    pat_ref.0.as_ptr() as usize
}

struct Checker {
    /// Content type of each define reached so far, keyed by `ref_key()`; `None` while that
    /// define is still being walked
    content_types: HashMap<usize, Option<ContentType>>,
    /// Whether each define simplifies to notAllowed, keyed by `ref_key()`
    not_allowed: HashMap<usize, bool>,
}

impl Checker {
    /// Whether the pattern would become notAllowed under the simplification rules of
    /// section 4.20, which the restrictions of section 7 are defined to apply after.
    ///
    /// `optional` and `zeroOrMore` are absent because they simplify to a choice with `empty`,
    /// so `notAllowed?` becomes `empty`, not `notAllowed`.
    fn is_not_allowed(&mut self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::NotAllowed(_) => true,
            Pattern::Attribute(_, body, _, _)
            | Pattern::OneOrMore(body, _)
            | Pattern::List(body, _)
            | Pattern::Mixed(body, _) => self.is_not_allowed(body),
            Pattern::Group(children, _) | Pattern::Interleave(children, _) => {
                children.iter().any(|c| self.is_not_allowed(c))
            }
            Pattern::Choice(children, _) => children.iter().all(|c| self.is_not_allowed(c)),
            Pattern::Ref(_, _, pat_ref) => {
                let key = ref_key(pat_ref);
                if let Some(&result) = self.not_allowed.get(&key) {
                    return result;
                }
                // Recursion that doesn't pass through an element has already been rejected,
                // and we don't descend into elements, so this placeholder is never read back.
                self.not_allowed.insert(key, false);
                let result = pat_ref
                    .0
                    .borrow()
                    .as_ref()
                    .is_some_and(|rule| self.is_not_allowed(rule.pattern()));
                self.not_allowed.insert(key, result);
                result
            }
            Pattern::Optional(_, _)
            | Pattern::ZeroOrMore(_, _)
            | Pattern::Element(_, _, _, _)
            | Pattern::Empty(_)
            | Pattern::Text(_)
            | Pattern::DatatypeValue { .. }
            | Pattern::DatatypeName { .. } => false,
        }
    }

    /// Main recursive walk. Returns the content type of the pattern.
    fn walk(&mut self, flags: Flags, pattern: &Pattern) -> Result<ContentType, Violation> {
        let at = |kind| Violation::at(kind, pattern.span());
        // A pattern that simplifies to notAllowed is removed (or makes its parent notAllowed),
        // so no restriction applies within it
        if self.is_not_allowed(pattern) {
            return Ok(ContentType::Empty);
        }

        match pattern {
            Pattern::Empty(_) => {
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsEmpty));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsEmpty));
                }
                Ok(ContentType::Empty)
            }

            Pattern::NotAllowed(_) => Ok(ContentType::Empty),

            Pattern::Text(_) => {
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsText));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsText));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsText));
                }
                Ok(ContentType::Complex)
            }

            Pattern::Ref(_, _, pat_ref) => {
                let key = ref_key(pat_ref);
                match self.content_types.get(&key) {
                    Some(Some(ct)) => Ok(*ct),
                    Some(None) => {
                        // Currently being computed — recursive reference.
                        Ok(ContentType::Complex)
                    }
                    None => {
                        // First visit: insert sentinel, walk, cache result.
                        self.content_types.insert(key, None);
                        let ct = {
                            let borrowed = pat_ref.0.borrow();
                            if let Some(rule) = borrowed.as_ref() {
                                self.walk(flags, rule.pattern())?
                            } else {
                                ContentType::Empty
                            }
                        };
                        self.content_types.insert(key, Some(ct));
                        Ok(ct)
                    }
                }
            }

            Pattern::Element(_, body, _, _) => {
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsElement));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsElement));
                }
                self.walk(flags.enter_element(), body)?;
                Ok(ContentType::Complex)
            }

            Pattern::Attribute(nc, body, _, _) => {
                if flags.in_attribute {
                    return Err(at(RestrictionKind::AttributeContainsAttribute));
                }
                if flags.in_one_or_more_group {
                    return Err(at(RestrictionKind::DuplicateAttribute));
                }
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsAttribute));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsAttribute));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsAttribute));
                }

                // Check attribute name class restrictions
                check_attribute_name_class(nc).map_err(at)?;

                // 7.3: attribute with infinite name class must be inside oneOrMore
                if is_infinite_name_class(nc)
                    && !flags.in_one_or_more
                    && !flags.in_one_or_more_group
                {
                    return Err(at(RestrictionKind::AttributeInfiniteNameClass));
                }

                self.walk(flags.enter_attribute(), body)?;
                Ok(ContentType::Empty)
            }

            Pattern::DatatypeValue { .. } => {
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsValue));
                }
                Ok(ContentType::Simple)
            }

            Pattern::DatatypeName { except, .. } => {
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsData));
                }
                if let Some(except) = except {
                    self.walk(flags.enter_data_except(), except)?;
                }
                Ok(ContentType::Simple)
            }

            Pattern::List(body, _) => {
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsList));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsList));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsList));
                }
                self.walk(flags.enter_list(), body)?;
                Ok(ContentType::Simple)
            }

            Pattern::OneOrMore(body, _) | Pattern::ZeroOrMore(body, _) => {
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsOneOrMore));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsOneOrMore));
                }
                self.walk(flags.enter_one_or_more(), body)
            }

            Pattern::Optional(body, _) => self.walk(flags, body),

            Pattern::Mixed(body, _) => {
                // Mixed = interleave(text, body)
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsInterleave));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsInterleave));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsInterleave));
                }

                let ct = self.walk(flags.enter_group_or_interleave(), body)?;

                // Check 7.2: text is Complex, so if body is Simple that's an error
                if ct == ContentType::Simple {
                    return Err(at(RestrictionKind::StringSequence));
                }

                // Check 7.4: text overlap - if body contains text, there are two text patterns
                if let Some(text) = self.find_text(body, &mut HashSet::new()) {
                    return Err(Violation {
                        kind: RestrictionKind::InterleaveTextOverlap,
                        span: text.span,
                        other_span: Some(pattern.span()),
                    });
                }

                Ok(ContentType::Complex)
            }

            Pattern::Group(children, _) => {
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsGroup));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsGroup));
                }
                self.walk_group_or_interleave(flags, pattern.span(), children, false)
            }

            Pattern::Interleave(children, _) => {
                if flags.in_list {
                    return Err(at(RestrictionKind::ListContainsInterleave));
                }
                if flags.in_data_except {
                    return Err(at(RestrictionKind::DataExceptContainsInterleave));
                }
                if flags.in_start {
                    return Err(at(RestrictionKind::StartContainsInterleave));
                }
                self.walk_group_or_interleave(flags, pattern.span(), children, true)
            }

            Pattern::Choice(children, _) => {
                let mut max_ct = ContentType::Empty;
                for child in children {
                    let ct = self.walk(flags, child)?;
                    max_ct = std::cmp::max(max_ct, ct);
                }
                Ok(max_ct)
            }
        }
    }

    /// Walk a group or interleave, checking 7.2, 7.3, 7.4
    fn walk_group_or_interleave(
        &mut self,
        flags: Flags,
        span: Span,
        children: &[Pattern],
        is_interleave: bool,
    ) -> Result<ContentType, Violation> {
        // Count non-empty children (after effective NotAllowed/Empty filtering)
        // If only one non-empty child, this group/interleave is effectively transparent
        let non_trivial = children
            .iter()
            .filter(|c| !matches!(c, Pattern::Empty(_) | Pattern::NotAllowed(_)))
            .count();
        let is_real_group = non_trivial > 1;

        let inner_flags = if is_real_group {
            flags.enter_group_or_interleave()
        } else {
            flags
        };

        // Walk children and collect content types
        let mut content_types = Vec::new();
        for child in children {
            let ct = self.walk(inner_flags, child)?;
            content_types.push(ct);
        }

        // 7.2: check content type compatibility (does not apply inside list)
        if !flags.in_list {
            let has_simple = content_types.contains(&ContentType::Simple);
            let non_empty_count = content_types
                .iter()
                .filter(|ct| **ct != ContentType::Empty)
                .count();
            if has_simple && non_empty_count > 1 {
                return Err(Violation::at(RestrictionKind::StringSequence, span));
            }
        }

        // 7.3: check attribute name overlap between branches
        let attrs: Vec<_> = children
            .iter()
            .map(|child| {
                let mut found = Vec::new();
                self.collect_attributes(child, &mut found, &mut HashSet::new());
                found
            })
            .collect();
        check_name_overlap(&attrs, RestrictionKind::AttributeNameOverlap)?;

        // 7.4: interleave-specific checks
        if is_interleave {
            let elems: Vec<_> = children
                .iter()
                .map(|child| {
                    let mut found = Vec::new();
                    self.collect_elements(child, &mut found, &mut HashSet::new());
                    found
                })
                .collect();
            check_name_overlap(&elems, RestrictionKind::InterleaveElementOverlap)?;

            let mut texts = children
                .iter()
                .filter_map(|child| self.find_text(child, &mut HashSet::new()));
            if let (Some(first), Some(second)) = (texts.next(), texts.next()) {
                return Err(Violation {
                    kind: RestrictionKind::InterleaveTextOverlap,
                    span: second.span,
                    other_span: Some(first.span),
                });
            }
        }

        // Content type is max of children
        Ok(content_types
            .into_iter()
            .max()
            .unwrap_or(ContentType::Empty))
    }

    /// Collect attribute name-classes from a pattern tree, stopping at Element boundaries
    /// and skipping anything that simplifies to notAllowed.
    fn collect_attributes(
        &mut self,
        pattern: &Pattern,
        attrs: &mut Vec<Found<NameClass>>,
        seen_refs: &mut HashSet<usize>,
    ) {
        if self.is_not_allowed(pattern) {
            return;
        }
        match pattern {
            Pattern::Attribute(nc, _, span, _) => {
                attrs.push(Found {
                    item: nc.clone(),
                    span: *span,
                });
            }
            Pattern::Element(_, _, _, _) => {}
            Pattern::Ref(_, _, pat_ref) => {
                if seen_refs.insert(ref_key(pat_ref)) {
                    let borrowed = pat_ref.0.borrow();
                    if let Some(rule) = borrowed.as_ref() {
                        self.collect_attributes(rule.pattern(), attrs, seen_refs);
                    }
                }
            }
            Pattern::Group(children, _)
            | Pattern::Interleave(children, _)
            | Pattern::Choice(children, _) => {
                for child in children {
                    self.collect_attributes(child, attrs, seen_refs);
                }
            }
            Pattern::OneOrMore(body, _)
            | Pattern::ZeroOrMore(body, _)
            | Pattern::Optional(body, _)
            | Pattern::Mixed(body, _)
            | Pattern::List(body, _) => {
                self.collect_attributes(body, attrs, seen_refs);
            }
            Pattern::Empty(_)
            | Pattern::Text(_)
            | Pattern::NotAllowed(_)
            | Pattern::DatatypeValue { .. }
            | Pattern::DatatypeName { .. } => {}
        }
    }

    /// Collect element name-classes from a pattern tree, stopping at Element body boundaries
    /// and skipping anything that simplifies to notAllowed.
    fn collect_elements(
        &mut self,
        pattern: &Pattern,
        elems: &mut Vec<Found<NameClass>>,
        seen_refs: &mut HashSet<usize>,
    ) {
        if self.is_not_allowed(pattern) {
            return;
        }
        match pattern {
            Pattern::Element(nc, _, span, _) => {
                elems.push(Found {
                    item: nc.clone(),
                    span: *span,
                });
            }
            Pattern::Ref(_, _, pat_ref) => {
                if seen_refs.insert(ref_key(pat_ref)) {
                    let borrowed = pat_ref.0.borrow();
                    if let Some(rule) = borrowed.as_ref() {
                        self.collect_elements(rule.pattern(), elems, seen_refs);
                    }
                }
            }
            Pattern::Group(children, _)
            | Pattern::Interleave(children, _)
            | Pattern::Choice(children, _) => {
                for child in children {
                    self.collect_elements(child, elems, seen_refs);
                }
            }
            Pattern::OneOrMore(body, _)
            | Pattern::ZeroOrMore(body, _)
            | Pattern::Optional(body, _)
            | Pattern::Mixed(body, _)
            | Pattern::List(body, _) => {
                self.collect_elements(body, elems, seen_refs);
            }
            Pattern::Attribute(_, _, _, _) => {}
            Pattern::Empty(_)
            | Pattern::Text(_)
            | Pattern::NotAllowed(_)
            | Pattern::DatatypeValue { .. }
            | Pattern::DatatypeName { .. } => {}
        }
    }

    /// Find a Text node in a pattern tree (for the interleave text overlap check), skipping
    /// anything that simplifies to notAllowed.
    fn find_text(
        &mut self,
        pattern: &Pattern,
        seen_refs: &mut HashSet<usize>,
    ) -> Option<Found<()>> {
        if self.is_not_allowed(pattern) {
            return None;
        }
        match pattern {
            Pattern::Text(span) | Pattern::Mixed(_, span) => Some(Found {
                item: (),
                span: *span,
            }),
            Pattern::Element(_, _, _, _) | Pattern::Attribute(_, _, _, _) => None,
            Pattern::Ref(_, _, pat_ref) => {
                if seen_refs.insert(ref_key(pat_ref)) {
                    let borrowed = pat_ref.0.borrow();
                    if let Some(rule) = borrowed.as_ref() {
                        return self.find_text(rule.pattern(), seen_refs);
                    }
                }
                None
            }
            Pattern::Group(children, _)
            | Pattern::Interleave(children, _)
            | Pattern::Choice(children, _) => {
                children.iter().find_map(|c| self.find_text(c, seen_refs))
            }
            Pattern::OneOrMore(body, _)
            | Pattern::ZeroOrMore(body, _)
            | Pattern::Optional(body, _)
            | Pattern::List(body, _) => self.find_text(body, seen_refs),
            Pattern::Empty(_)
            | Pattern::NotAllowed(_)
            | Pattern::DatatypeValue { .. }
            | Pattern::DatatypeName { .. } => None,
        }
    }
}

/// Check that no name class collected from one branch overlaps one from a different branch
fn check_name_overlap(
    branches: &[Vec<Found<NameClass>>],
    kind: RestrictionKind,
) -> Result<(), Violation> {
    for (i, earlier) in branches.iter().enumerate() {
        for later in &branches[i + 1..] {
            for a in earlier {
                for b in later {
                    if name_class_overlaps(&a.item, &b.item) {
                        return Err(Violation {
                            kind,
                            span: b.span,
                            other_span: Some(a.span),
                        });
                    }
                }
            }
        }
    }
    Ok(())
}

/// Check if two name classes can accept the same name.
pub(crate) fn name_class_overlaps(a: &NameClass, b: &NameClass) -> bool {
    match (a, b) {
        (
            NameClass::Named {
                namespace_uri: ns_a,
                name: name_a,
            },
            NameClass::Named {
                namespace_uri: ns_b,
                name: name_b,
            },
        ) => ns_a == ns_b && name_a == name_b,

        (
            NameClass::Named {
                namespace_uri,
                name,
            },
            NameClass::NsName {
                namespace_uri: ns,
                except,
            },
        )
        | (
            NameClass::NsName {
                namespace_uri: ns,
                except,
            },
            NameClass::Named {
                namespace_uri,
                name,
            },
        ) => {
            namespace_uri == ns
                && !except
                    .as_ref()
                    .is_some_and(|e| name_class_contains(e, namespace_uri, name))
        }

        (
            NameClass::Named {
                namespace_uri,
                name,
            },
            NameClass::AnyName { except },
        )
        | (
            NameClass::AnyName { except },
            NameClass::Named {
                namespace_uri,
                name,
            },
        ) => !except
            .as_ref()
            .is_some_and(|e| name_class_contains(e, namespace_uri, name)),

        (
            NameClass::NsName {
                namespace_uri: ns_a,
                ..
            },
            NameClass::NsName {
                namespace_uri: ns_b,
                ..
            },
        ) => ns_a == ns_b,

        (NameClass::NsName { .. }, NameClass::AnyName { .. })
        | (NameClass::AnyName { .. }, NameClass::NsName { .. }) => true,

        (NameClass::AnyName { .. }, NameClass::AnyName { .. }) => true,

        (NameClass::Alt { a: a1, b: b1 }, other) | (other, NameClass::Alt { a: a1, b: b1 }) => {
            name_class_overlaps(a1, other) || name_class_overlaps(b1, other)
        }
    }
}

/// Check if a name class contains a specific (namespace, localname) pair.
fn name_class_contains(nc: &NameClass, ns: &str, name: &str) -> bool {
    match nc {
        NameClass::Named {
            namespace_uri,
            name: n,
        } => namespace_uri == ns && n == name,
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            namespace_uri == ns
                && !except
                    .as_ref()
                    .is_some_and(|e| name_class_contains(e, ns, name))
        }
        NameClass::AnyName { except } => !except
            .as_ref()
            .is_some_and(|e| name_class_contains(e, ns, name)),
        NameClass::Alt { a, b } => {
            name_class_contains(a, ns, name) || name_class_contains(b, ns, name)
        }
    }
}

/// Check attribute name class restrictions.
fn check_attribute_name_class(nc: &NameClass) -> Result<(), RestrictionKind> {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => {
            if namespace_uri.is_empty() && name == "xmlns" {
                return Err(RestrictionKind::AttributeNameXmlns);
            }
            if namespace_uri == "http://www.w3.org/2000/xmlns" {
                return Err(RestrictionKind::AttributeNamespaceXmlns);
            }
            Ok(())
        }
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            if namespace_uri == "http://www.w3.org/2000/xmlns" {
                return Err(RestrictionKind::AttributeNamespaceXmlns);
            }
            if let Some(except) = except {
                check_attribute_name_class(except)?;
            }
            Ok(())
        }
        NameClass::AnyName { except } => {
            if let Some(except) = except {
                check_attribute_name_class(except)?;
            }
            Ok(())
        }
        NameClass::Alt { a, b } => {
            check_attribute_name_class(a)?;
            check_attribute_name_class(b)?;
            Ok(())
        }
    }
}

/// Check name class constraints from the spec:
/// - anyName/except must not contain anyName descendants
/// - nsName/except must not contain nsName or anyName descendants
///
/// `span` is the location of the element or attribute pattern that has this name class.
pub fn check_name_class_restrictions(nc: &NameClass, span: Span) -> Result<(), Violation> {
    name_class_restriction(nc).map_err(|kind| Violation::at(kind, span))
}

fn name_class_restriction(nc: &NameClass) -> Result<(), RestrictionKind> {
    match nc {
        NameClass::AnyName {
            except: Some(except),
        } => {
            if contains_any_name(except) {
                return Err(RestrictionKind::AnyNameExceptContainsAnyName);
            }
            name_class_restriction(except)
        }
        NameClass::NsName {
            except: Some(except),
            ..
        } => {
            if contains_ns_name_or_any_name(except) {
                return Err(RestrictionKind::NsNameExceptContainsNsNameOrAnyName);
            }
            name_class_restriction(except)
        }
        NameClass::Alt { a, b } => {
            name_class_restriction(a)?;
            name_class_restriction(b)?;
            Ok(())
        }
        _ => Ok(()),
    }
}

fn contains_any_name(nc: &NameClass) -> bool {
    match nc {
        NameClass::AnyName { .. } => true,
        NameClass::Alt { a, b } => contains_any_name(a) || contains_any_name(b),
        _ => false,
    }
}

fn contains_ns_name_or_any_name(nc: &NameClass) -> bool {
    match nc {
        NameClass::AnyName { .. } | NameClass::NsName { .. } => true,
        NameClass::Alt { a, b } => {
            contains_ns_name_or_any_name(a) || contains_ns_name_or_any_name(b)
        }
        _ => false,
    }
}
