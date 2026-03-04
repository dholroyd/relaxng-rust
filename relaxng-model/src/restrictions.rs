use crate::model::{NameClass, Pattern};
use std::collections::HashSet;

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
            RestrictionKind::AttributeContainsAttribute => write!(f, "attribute must not contain attribute (section 7.1.1)"),
            RestrictionKind::DuplicateAttribute => write!(f, "duplicate attribute in oneOrMore (section 7.1.2)"),
            RestrictionKind::ListContainsList => write!(f, "list must not contain list (section 7.1.3)"),
            RestrictionKind::ListContainsAttribute => write!(f, "list must not contain attribute (section 7.1.3)"),
            RestrictionKind::ListContainsText => write!(f, "list must not contain text (section 7.1.3)"),
            RestrictionKind::ListContainsInterleave => write!(f, "list must not contain interleave (section 7.1.3)"),
            RestrictionKind::ListContainsElement => write!(f, "list must not contain element (section 7.1.3)"),
            RestrictionKind::DataExceptContainsAttribute => write!(f, "data/except must not contain attribute (section 7.1.4)"),
            RestrictionKind::DataExceptContainsText => write!(f, "data/except must not contain text (section 7.1.4)"),
            RestrictionKind::DataExceptContainsList => write!(f, "data/except must not contain list (section 7.1.4)"),
            RestrictionKind::DataExceptContainsGroup => write!(f, "data/except must not contain group (section 7.1.4)"),
            RestrictionKind::DataExceptContainsInterleave => write!(f, "data/except must not contain interleave (section 7.1.4)"),
            RestrictionKind::DataExceptContainsOneOrMore => write!(f, "data/except must not contain oneOrMore (section 7.1.4)"),
            RestrictionKind::DataExceptContainsEmpty => write!(f, "data/except must not contain empty (section 7.1.4)"),
            RestrictionKind::DataExceptContainsElement => write!(f, "data/except must not contain element (section 7.1.4)"),
            RestrictionKind::StartContainsAttribute => write!(f, "start must not contain attribute (section 7.1.5)"),
            RestrictionKind::StartContainsData => write!(f, "start must not contain data (section 7.1.5)"),
            RestrictionKind::StartContainsValue => write!(f, "start must not contain value (section 7.1.5)"),
            RestrictionKind::StartContainsText => write!(f, "start must not contain text (section 7.1.5)"),
            RestrictionKind::StartContainsList => write!(f, "start must not contain list (section 7.1.5)"),
            RestrictionKind::StartContainsGroup => write!(f, "start must not contain group (section 7.1.5)"),
            RestrictionKind::StartContainsInterleave => write!(f, "start must not contain interleave (section 7.1.5)"),
            RestrictionKind::StartContainsOneOrMore => write!(f, "start must not contain oneOrMore (section 7.1.5)"),
            RestrictionKind::StartContainsEmpty => write!(f, "start must not contain empty (section 7.1.5)"),
            RestrictionKind::StringSequence => write!(f, "incompatible content types in group/interleave (section 7.2)"),
            RestrictionKind::AttributeNameOverlap => write!(f, "overlapping attribute names (section 7.3)"),
            RestrictionKind::AttributeInfiniteNameClass => write!(f, "attribute with infinite name class (section 7.3)"),
            RestrictionKind::InterleaveElementOverlap => write!(f, "interleave contains elements with overlapping names (section 7.4)"),
            RestrictionKind::InterleaveTextOverlap => write!(f, "interleave contains multiple text patterns (section 7.4)"),
            RestrictionKind::AnyNameExceptContainsAnyName => write!(f, "anyName/except must not contain anyName"),
            RestrictionKind::NsNameExceptContainsNsNameOrAnyName => write!(f, "nsName/except must not contain nsName or anyName"),
            RestrictionKind::AttributeNameXmlns => write!(f, "attribute with empty namespace must not be named 'xmlns'"),
            RestrictionKind::AttributeNamespaceXmlns => write!(f, "attribute name must not use XMLNS namespace"),
        }
    }
}

/// Check all section 7 restrictions on a compiled schema.
pub fn check_restrictions(start: &Pattern) -> Result<(), RestrictionKind> {
    let mut seen = HashSet::new();
    walk(&mut seen, Flags::start(), start)?;
    Ok(())
}

/// Check if a pattern is effectively NotAllowed after simplification.
/// attribute(_, notAllowed) = notAllowed
/// group/interleave with any notAllowed child = notAllowed
/// oneOrMore(notAllowed) = notAllowed
fn is_effectively_not_allowed(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::NotAllowed => true,
        Pattern::Attribute(_, body, _, _) => is_effectively_not_allowed(body),
        Pattern::Group(children) | Pattern::Interleave(children) => {
            children.iter().any(|c| is_effectively_not_allowed(c))
        }
        Pattern::OneOrMore(body) | Pattern::ZeroOrMore(body) | Pattern::Optional(body) => {
            is_effectively_not_allowed(body)
        }
        Pattern::List(body) => is_effectively_not_allowed(body),
        _ => false,
    }
}

/// Check if a name class is "infinite" (contains AnyName or NsName)
fn is_infinite_name_class(nc: &NameClass) -> bool {
    match nc {
        NameClass::AnyName { .. } | NameClass::NsName { .. } => true,
        NameClass::Alt { a, b } => is_infinite_name_class(a) || is_infinite_name_class(b),
        NameClass::Named { .. } => false,
    }
}

/// Main recursive walk. Returns the content type of the pattern.
fn walk(
    seen: &mut HashSet<usize>,
    flags: Flags,
    pattern: &Pattern,
) -> Result<ContentType, RestrictionKind> {
    // Short-circuit for effectively-not-allowed patterns
    if is_effectively_not_allowed(pattern) {
        return Ok(ContentType::Empty);
    }

    match pattern {
        Pattern::Empty => {
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsEmpty);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsEmpty);
            }
            Ok(ContentType::Empty)
        }

        Pattern::NotAllowed => Ok(ContentType::Empty),

        Pattern::Text(_) => {
            if flags.in_list {
                return Err(RestrictionKind::ListContainsText);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsText);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsText);
            }
            Ok(ContentType::Complex)
        }

        Pattern::Ref(_, _, pat_ref) => {
            // Refs are allowed in lists, attributes, and data/except — the spec says
            // to check what the ref expands to, not reject refs outright.
            // walk() follows the ref with the same flags, so forbidden content
            // (element, text, list, etc.) will be caught in the expanded pattern.
            let ptr = pat_ref.0.as_ptr() as usize;
            if !seen.contains(&ptr) {
                seen.insert(ptr);
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    walk(seen, flags, rule.pattern())?;
                }
            }
            Ok(ContentType::Complex)
        }

        Pattern::Element(_, body, _, _) => {
            if flags.in_list {
                return Err(RestrictionKind::ListContainsElement);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsElement);
            }
            let inner_flags = flags.enter_element();
            walk(seen, inner_flags, body)?;
            Ok(ContentType::Complex)
        }

        Pattern::Attribute(nc, body, _, _) => {
            if flags.in_attribute {
                return Err(RestrictionKind::AttributeContainsAttribute);
            }
            if flags.in_one_or_more_group {
                return Err(RestrictionKind::DuplicateAttribute);
            }
            if flags.in_list {
                return Err(RestrictionKind::ListContainsAttribute);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsAttribute);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsAttribute);
            }

            // Check attribute name class restrictions
            check_attribute_name_class(nc)?;

            // 7.3: attribute with infinite name class must be inside oneOrMore
            if is_infinite_name_class(nc) && !flags.in_one_or_more && !flags.in_one_or_more_group {
                return Err(RestrictionKind::AttributeInfiniteNameClass);
            }

            let inner_flags = flags.enter_attribute();
            walk(seen, inner_flags, body)?;
            Ok(ContentType::Empty)
        }

        Pattern::DatatypeValue { .. } => {
            if flags.in_start {
                return Err(RestrictionKind::StartContainsValue);
            }
            Ok(ContentType::Simple)
        }

        Pattern::DatatypeName { except, .. } => {
            if flags.in_start {
                return Err(RestrictionKind::StartContainsData);
            }
            if let Some(except) = except {
                let inner_flags = flags.enter_data_except();
                walk(seen, inner_flags, except)?;
            }
            Ok(ContentType::Simple)
        }

        Pattern::List(body) => {
            if flags.in_list {
                return Err(RestrictionKind::ListContainsList);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsList);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsList);
            }
            let inner_flags = flags.enter_list();
            walk(seen, inner_flags, body)?;
            Ok(ContentType::Simple)
        }

        Pattern::OneOrMore(body) => {
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsOneOrMore);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsOneOrMore);
            }
            let inner_flags = flags.enter_one_or_more();
            walk(seen, inner_flags, body)
        }

        Pattern::ZeroOrMore(body) => {
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsOneOrMore);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsOneOrMore);
            }
            let inner_flags = flags.enter_one_or_more();
            walk(seen, inner_flags, body)
        }

        Pattern::Optional(body) => {
            walk(seen, flags, body)
        }

        Pattern::Mixed(body) => {
            // Mixed = interleave(text, body)
            if flags.in_list {
                return Err(RestrictionKind::ListContainsInterleave);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsInterleave);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsInterleave);
            }

            let inner_flags = flags.enter_group_or_interleave();
            let ct = walk(seen, inner_flags, body)?;

            // Check 7.2: text is Complex, so if body is Simple that's an error
            if ct == ContentType::Simple {
                return Err(RestrictionKind::StringSequence);
            }

            // Check 7.4: text overlap - if body contains text, there are two text patterns
            if contains_text(body, &mut HashSet::new()) {
                return Err(RestrictionKind::InterleaveTextOverlap);
            }

            Ok(ContentType::Complex)
        }

        Pattern::Group(children) => {
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsGroup);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsGroup);
            }
            walk_group_or_interleave(seen, flags, children, false)
        }

        Pattern::Interleave(children) => {
            if flags.in_list {
                return Err(RestrictionKind::ListContainsInterleave);
            }
            if flags.in_data_except {
                return Err(RestrictionKind::DataExceptContainsInterleave);
            }
            if flags.in_start {
                return Err(RestrictionKind::StartContainsInterleave);
            }
            walk_group_or_interleave(seen, flags, children, true)
        }

        Pattern::Choice(children) => {
            let mut max_ct = ContentType::Empty;
            for child in children {
                // In choice, NotAllowed branches are just skipped
                if is_effectively_not_allowed(child) {
                    continue;
                }
                let ct = walk(seen, flags, child)?;
                max_ct = std::cmp::max(max_ct, ct);
            }
            Ok(max_ct)
        }
    }
}

/// Walk a group or interleave, checking 7.2, 7.3, 7.4
fn walk_group_or_interleave(
    seen: &mut HashSet<usize>,
    flags: Flags,
    children: &[Pattern],
    is_interleave: bool,
) -> Result<ContentType, RestrictionKind> {
    // Count non-empty children (after effective NotAllowed/Empty filtering)
    // If only one non-empty child, this group/interleave is effectively transparent
    let non_trivial: Vec<&Pattern> = children
        .iter()
        .filter(|c| !matches!(c, Pattern::Empty | Pattern::NotAllowed))
        .collect();
    let is_real_group = non_trivial.len() > 1;

    let inner_flags = if is_real_group {
        flags.enter_group_or_interleave()
    } else {
        flags
    };

    // Walk children and collect content types
    let mut content_types = Vec::new();
    for child in children {
        let ct = walk(seen, inner_flags, child)?;
        content_types.push(ct);
    }

    // 7.2: check content type compatibility (does not apply inside list)
    if !flags.in_list {
        let has_simple = content_types.iter().any(|ct| *ct == ContentType::Simple);
        let non_empty_count = content_types
            .iter()
            .filter(|ct| **ct != ContentType::Empty)
            .count();
        if has_simple && non_empty_count > 1 {
            return Err(RestrictionKind::StringSequence);
        }
    }

    // 7.3: check attribute name overlap between branches
    check_attribute_overlap(children)?;

    // 7.4: interleave-specific checks
    if is_interleave {
        check_interleave_overlap(children)?;
    }

    // Content type is max of children
    Ok(content_types.into_iter().max().unwrap_or(ContentType::Empty))
}

/// Section 7.3: check that attributes from different branches don't have overlapping names
fn check_attribute_overlap(children: &[Pattern]) -> Result<(), RestrictionKind> {
    let mut all_attrs: Vec<Vec<NameClass>> = Vec::new();
    for child in children {
        let mut attrs = Vec::new();
        collect_attributes(child, &mut attrs, &mut HashSet::new());
        all_attrs.push(attrs);
    }

    for i in 0..all_attrs.len() {
        for j in (i + 1)..all_attrs.len() {
            for a in &all_attrs[i] {
                for b in &all_attrs[j] {
                    if name_class_overlaps(a, b) {
                        return Err(RestrictionKind::AttributeNameOverlap);
                    }
                }
            }
        }
    }
    Ok(())
}

/// Section 7.4: check interleave-specific overlap constraints
fn check_interleave_overlap(children: &[Pattern]) -> Result<(), RestrictionKind> {
    let mut all_elems: Vec<Vec<NameClass>> = Vec::new();
    let mut text_branches = 0;
    for child in children {
        let mut elems = Vec::new();
        collect_elements(child, &mut elems, &mut HashSet::new());
        all_elems.push(elems);
        if contains_text(child, &mut HashSet::new()) {
            text_branches += 1;
        }
    }

    for i in 0..all_elems.len() {
        for j in (i + 1)..all_elems.len() {
            for a in &all_elems[i] {
                for b in &all_elems[j] {
                    if name_class_overlaps(a, b) {
                        return Err(RestrictionKind::InterleaveElementOverlap);
                    }
                }
            }
        }
    }

    if text_branches > 1 {
        return Err(RestrictionKind::InterleaveTextOverlap);
    }

    Ok(())
}

/// Collect attribute name-classes from a pattern tree, stopping at Element boundaries.
fn collect_attributes(
    pattern: &Pattern,
    attrs: &mut Vec<NameClass>,
    seen_refs: &mut HashSet<usize>,
) {
    match pattern {
        Pattern::Attribute(nc, _, _, _) => {
            attrs.push(nc.clone());
        }
        Pattern::Element(_, _, _, _) => {}
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    collect_attributes(rule.pattern(), attrs, seen_refs);
                }
            }
        }
        Pattern::Group(children) | Pattern::Interleave(children) | Pattern::Choice(children) => {
            for child in children {
                collect_attributes(child, attrs, seen_refs);
            }
        }
        Pattern::OneOrMore(body)
        | Pattern::ZeroOrMore(body)
        | Pattern::Optional(body)
        | Pattern::Mixed(body)
        | Pattern::List(body) => {
            collect_attributes(body, attrs, seen_refs);
        }
        Pattern::Empty
        | Pattern::Text(_)
        | Pattern::NotAllowed
        | Pattern::DatatypeValue { .. }
        | Pattern::DatatypeName { .. } => {}
    }
}

/// Collect element name-classes from a pattern tree, stopping at Element body boundaries.
fn collect_elements(
    pattern: &Pattern,
    elems: &mut Vec<NameClass>,
    seen_refs: &mut HashSet<usize>,
) {
    match pattern {
        Pattern::Element(nc, _, _, _) => {
            elems.push(nc.clone());
        }
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    collect_elements(rule.pattern(), elems, seen_refs);
                }
            }
        }
        Pattern::Group(children) | Pattern::Interleave(children) | Pattern::Choice(children) => {
            for child in children {
                collect_elements(child, elems, seen_refs);
            }
        }
        Pattern::OneOrMore(body)
        | Pattern::ZeroOrMore(body)
        | Pattern::Optional(body)
        | Pattern::Mixed(body)
        | Pattern::List(body) => {
            collect_elements(body, elems, seen_refs);
        }
        Pattern::Attribute(_, _, _, _) => {}
        Pattern::Empty
        | Pattern::Text(_)
        | Pattern::NotAllowed
        | Pattern::DatatypeValue { .. }
        | Pattern::DatatypeName { .. } => {}
    }
}

/// Check if a pattern tree contains a Text node (for interleave text overlap check).
fn contains_text(pattern: &Pattern, seen_refs: &mut HashSet<usize>) -> bool {
    match pattern {
        Pattern::Text(_) => true,
        Pattern::Mixed(_) => true,
        Pattern::Element(_, _, _, _) | Pattern::Attribute(_, _, _, _) => false,
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if seen_refs.insert(ptr) {
                let borrowed = pat_ref.0.borrow();
                if let Some(rule) = borrowed.as_ref() {
                    return contains_text(rule.pattern(), seen_refs);
                }
            }
            false
        }
        Pattern::Group(children) | Pattern::Interleave(children) | Pattern::Choice(children) => {
            children.iter().any(|c| contains_text(c, seen_refs))
        }
        Pattern::OneOrMore(body)
        | Pattern::ZeroOrMore(body)
        | Pattern::Optional(body)
        | Pattern::List(body) => contains_text(body, seen_refs),
        Pattern::Empty
        | Pattern::NotAllowed
        | Pattern::DatatypeValue { .. }
        | Pattern::DatatypeName { .. } => false,
    }
}

/// Check if two name classes can accept the same name.
fn name_class_overlaps(a: &NameClass, b: &NameClass) -> bool {
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
            NameClass::NsName { namespace_uri: ns, except },
        )
        | (
            NameClass::NsName { namespace_uri: ns, except },
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
        NameClass::Alt { a, b } => name_class_contains(a, ns, name) || name_class_contains(b, ns, name),
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
pub fn check_name_class_restrictions(nc: &NameClass) -> Result<(), RestrictionKind> {
    match nc {
        NameClass::AnyName { except: Some(except) } => {
            if contains_any_name(except) {
                return Err(RestrictionKind::AnyNameExceptContainsAnyName);
            }
            check_name_class_restrictions(except)
        }
        NameClass::NsName { except: Some(except), .. } => {
            if contains_ns_name_or_any_name(except) {
                return Err(RestrictionKind::NsNameExceptContainsNsNameOrAnyName);
            }
            check_name_class_restrictions(except)
        }
        NameClass::Alt { a, b } => {
            check_name_class_restrictions(a)?;
            check_name_class_restrictions(b)?;
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
