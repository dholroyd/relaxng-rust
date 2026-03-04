use crate::types::*;
use roxmltree::{Attribute, Node};
use std::ops::Range;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Error {
    /// Expected value was not present in the input
    Expected(Span, &'static str),
    /// An unexpected value was present in the input
    Unexpected(Span, &'static str),
    Xml(Span, String),
    // TODO: remove this variant once all syntax is supported
    Todo(&'static str),
}

pub fn parse(text: &str) -> Result<Schema> {
    let doc = roxmltree::Document::parse(text).map_err(|e| {
        // Gah!  The interface we want to expose is in terms of byte-offset + len pairs, but
        // the errors from roxmltree give us
        let (off, len) = text
            .lines()
            .take(e.pos().row as _)
            .fold((0, 0), |(acc, _), line| (acc + line.len(), line.len()));

        let start = off + e.pos().col as usize;
        let end = len - e.pos().col as usize;
        Error::Xml(start..end, e.to_string())
    })?;
    Ok(Schema {
        decls: vec![],
        pattern_or_grammar: pattern_or_grammar(doc.root_element())?,
    })
}

fn pattern_or_grammar(root: Node) -> Result<PatternOrGrammar> {
    match root.tag_name().namespace() {
        Some(NS) => {
            if root.tag_name().name() == "grammar" {
                Ok(PatternOrGrammar::Grammar(grammar(root)?))
            } else {
                Ok(PatternOrGrammar::Pattern(pattern(root)?))
            }
        }
        _ => Err(Error::Expected(
            root.range(),
            "Expected root element with http://relaxng.org/ns/structure/1.0 namespace",
        )),
    }
}

type Result<T> = std::result::Result<T, Error>;

const NS: &str = "http://relaxng.org/ns/structure/1.0";

fn pattern(node: Node) -> Result<Pattern> {
    if node.is_element() && node.tag_name().namespace() == Some(NS) {
        check_standard_attrs(node)?;
        match node.tag_name().name() {
            "element" => element(node).map(Pattern::Element),
            "attribute" => attribute(node).map(Pattern::Attribute),
            "group" => group(node),
            "interleave" => interleave(node),
            "choice" => choice(node),
            "optional" => optional(node),
            "zeroOrMore" => zero_or_more(node),
            "oneOrMore" => one_or_more(node),
            "list" => list(node).map(Pattern::List),
            "mixed" => mixed(node).map(Pattern::Mixed),
            "ref" => ref_patt(node).map(Pattern::Identifier),
            "parentRef" => parent_ref(node).map(Pattern::Parent),
            "empty" => empty(node),
            "text" => text(node),
            "value" => value(node).map(Pattern::DatatypeValue),
            "data" => data(node).map(Pattern::DatatypeName),
            "notAllowed" => not_allowed(node),
            "externalRef" => external_ref(node).map(Pattern::External),
            "grammar" => grammar(node).map(Pattern::Grammar),
            _ => Err(Error::Expected(node.range(), "pattern")),
        }
    } else {
        Err(Error::Expected(node.range(), "pattern"))
    }
}

fn check_standard_attrs(node: Node) -> Result<()> {
    if let Some(dt_lib) = node.attribute_node("datatypeLibrary") {
        let val = dt_lib.value();
        if val.is_empty() {
            Ok(())
        } else {
            // TODO: move these checks into relaxng-model crate
            match fluent_uri::Uri::parse(val) {
                Ok(uri) => {
                    if uri.fragment().is_some() {
                        Err(Error::Unexpected(
                            dt_lib.range_value(),
                            "Datatype library URI must not include a fragment identifier",
                        ))
                    } else if uri.authority().is_none()
                        && uri.path().as_str().is_empty()
                        && uri.query().is_none()
                    {
                        // RFC 2396 requires scheme-specific part to be non-empty
                        Err(Error::Unexpected(
                            dt_lib.range_value(),
                            "Datatype library URI has empty scheme-specific part",
                        ))
                    } else {
                        Ok(())
                    }
                }
                Err(_) => Err(Error::Unexpected(
                    dt_lib.range_value(),
                    "Invalid datatype library URI",
                )),
            }
        }
    } else {
        Ok(())
    }
}

fn first_rng_child<'a, 'input: 'a>(node: Node<'a, 'input>) -> Option<Node<'a, 'input>> {
    let mut child = node.first_element_child();
    // TODO: error on non-whitespace text nodes?
    while let Some(node) = child {
        if is_rng_node(node) {
            return Some(node);
        }
        child = node.next_sibling_element();
    }
    None
}

fn next_rng_sibling<'a, 'input: 'a>(node: Node<'a, 'input>) -> Option<Node<'a, 'input>> {
    let mut child = node.next_sibling_element();
    // TODO: error on non-whitespace text nodes?
    while let Some(node) = child {
        if is_rng_node(node) {
            return Some(node);
        }
        child = node.next_sibling_element();
    }
    None
}
fn element(node: Node) -> Result<ElementPattern> {
    no_attrs_except(node, &["name", "ns", "datatypeLibrary"])?;
    let (name_class, pattern) = if let Some(name) = node.attribute_node("name") {
        let name_class = NameClass::Name(qname_att(node, &name, true)?);
        let pat_el = first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?;
        let pattern = single_pattern_or_group(pat_el)?;
        (name_class, pattern)
    } else {
        let name_el = first_rng_child(node)
            .ok_or(Error::Expected(node.range(), "name and pattern children"))?;
        let name_class = name_class(name_el, InExcept::OutsideExcept)?;
        let pat_el =
            next_rng_sibling(name_el).ok_or(Error::Expected(node.range(), "pattern child"))?;
        let pattern = single_pattern_or_group(pat_el)?;
        (name_class, pattern)
    };
    Ok(ElementPattern {
        span: node.range().start + 1..node.range().start + 8,
        name_class,
        pattern: Box::new(pattern),
    })
}

fn no_attrs_except(node: Node, names: &[&str]) -> std::result::Result<(), Error> {
    if let Some(a) = node
        .attributes()
        .find(|a| is_rng_att(a) && !names.contains(&a.name()))
    {
        return Err(Error::Unexpected(a.range(), "Unexpected attribute"));
    }
    Ok(())
}

#[derive(PartialEq, Clone, Copy)]
enum InExcept {
    InsideExcept,
    OutsideExcept,
}

fn name_class(node: Node, in_except: InExcept) -> Result<NameClass> {
    if node.is_element()
        && node.tag_name().namespace() == Some("http://relaxng.org/ns/structure/1.0")
    {
        match node.tag_name().name() {
            "name" => qname_el(node).map(NameClass::Name),
            "anyName" => {
                if in_except == InExcept::InsideExcept {
                    return Err(Error::Unexpected(
                        node.range(),
                        "<anyName> not allowed in <except>",
                    ));
                }
                any_name(node).map(NameClass::AnyName)
            }
            "nsName" => ns_name(node).map(NameClass::NsName),
            "choice" => name_class_choice(node, in_except),
            _ => Err(Error::Expected(
                node.range(),
                "Either <name> <anyName> <nsName> or <choice>",
            )),
        }
    } else {
        Err(Error::Expected(
            node.range(),
            "Either <name> <anyName> <nsName> or <choice>",
        ))
    }
}

fn any_name(node: Node) -> Result<AnyName> {
    let except = if let Some(ex) = first_rng_child(node) {
        if is_el(ex, "except") {
            if let Some(next) = next_rng_sibling(ex) {
                return Err(Error::Unexpected(
                    next.range(),
                    "No other elements allowed here",
                ));
            }
            Some(except_nameclass(ex)?)
        } else {
            return Err(Error::Unexpected(ex.range(), "Only <except> allowed here"));
        }
    } else {
        None
    };
    Ok(AnyName(except.map(Box::new)))
}

fn except_nameclass(node: Node) -> Result<NameClass> {
    let mut child =
        first_rng_child(node).ok_or(Error::Expected(node.range(), "Expected name-class child"))?;
    let mut pat = name_class(child, InExcept::InsideExcept)?;
    while let Some(node) = next_rng_sibling(child) {
        let this = name_class(node, InExcept::InsideExcept)?;
        pat = NameClass::Alt(AltName(Box::new(pat), Box::new(this)));
        child = node;
    }
    Ok(pat)
    // TODO: check that no child is <anyName>
}

fn ns_name(node: Node) -> Result<NsName> {
    let ns = get_ns(node);
    let except = if let Some(ex) = first_rng_child(node) {
        if is_el(ex, "except") {
            if let Some(next) = next_rng_sibling(ex) {
                return Err(Error::Unexpected(
                    next.range(),
                    "No other elements allowed here",
                ));
            }
            Some(except_nameclass(ex)?)
        } else {
            return Err(Error::Unexpected(ex.range(), "Only <except> allowed here"));
        }
    } else {
        None
    };
    Ok(NsName {
        // TODO: the NsName type was defined to match compact-syntax and doesn't really work with
        //       xml syntax; we need a representation that works for xml-syntax too
        name: NamespaceOrPrefix::NamespaceUri(ns.unwrap_or(Literal::new(0..0, String::new()))),
        except: except.map(Box::new),
    })
}

fn get_ns(node: Node) -> Option<Literal> {
    get_ns_att(node).map(|ns| Literal::new(ns.range_value(), ns.value().to_string()))
}

/// Find the 'ns' attribute on the element, or its nearest parent element, or None if there is no
/// ns attribute on any parent element
fn get_ns_att<'a, 'input: 'a>(start: Node<'a, 'input>) -> Option<Attribute<'a, 'input>> {
    let mut this = Some(start);
    while let Some(node) = this {
        if let Some(ns) = node.attribute_node("ns") {
            return Some(ns);
        }
        this = node.parent_element();
    }
    None
}

fn get_datatype_lib(node: Node) -> Option<Literal> {
    get_dt_lib_att(node).map(|ns| Literal::new(ns.range_value(), ns.value().to_string()))
}

/// Find the 'datatypeLibrary' attribute on the element, or its nearest parent element, or None if there is no
/// ns attribute on any parent element
fn get_dt_lib_att<'a, 'input: 'a>(start: Node<'a, 'input>) -> Option<Attribute<'a, 'input>> {
    let mut this = Some(start);
    while let Some(node) = this {
        if let Some(ns) = node.attribute_node("datatypeLibrary") {
            return Some(ns);
        }
        this = node.parent_element();
    }
    None
}

fn name_class_choice(node: Node, in_except: InExcept) -> Result<NameClass> {
    let mut child = first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?;
    let mut name = name_class(child, in_except)?;
    while let Some(node) = next_rng_sibling(child) {
        let this = name_class(node, in_except)?;
        name = NameClass::Alt(AltName(Box::new(name), Box::new(this)));
        child = node;
    }
    Ok(name)
}

fn single_pattern_or_group(mut node: Node) -> Result<Pattern> {
    let mut pat = pattern(node)?;
    while let Some(next) = next_rng_sibling(node) {
        let this = pattern(next)?;
        node = next;
        pat = Pattern::ListPair(Box::new(pat), Box::new(this));
    }
    Ok(pat)
}

fn attribute(node: Node) -> Result<AttributePattern> {
    no_attrs_except(node, &["name", "ns", "datatypeLibrary"])?;
    // For <attribute>, ns defaults to "" and does NOT inherit from ancestors (spec section 4.3)
    let ns = node.attribute_node("ns");
    let (name_class, rest) = if let Some(name) = node.attribute_node("name") {
        if name.value() == "xmlns" && (ns.is_none() || ns.unwrap().value() == "") {
            return Err(Error::Unexpected(
                name.range_value(),
                "Schemas may not define the xmlns attribute",
            ));
        }
        if ns.is_some() && ns.unwrap().value() == "http://www.w3.org/2000/xmlns" {
            // TODO: additionally supply the range where the ns was declared
            return Err(Error::Unexpected(
                name.range(),
                "Attributes may not use the http://www.w3.org/2000/xmlns namespace",
            ));
        }
        (
            NameClass::Name(qname_att(node, &name, false)?),
            first_rng_child(node),
        )
    } else {
        let name_el = first_rng_child(node)
            .ok_or(Error::Expected(node.range(), "name and pattern children"))?;
        let name_class = name_class(name_el, InExcept::OutsideExcept)?;
        (name_class, next_rng_sibling(name_el))
    };
    let (pattern, rest) = if let Some(child) = rest {
        (pattern(child)?, next_rng_sibling(child))
    } else {
        (Pattern::Text(None), None)
    };
    if let Some(rest) = rest {
        return Err(Error::Unexpected(
            rest.range(),
            "Unexpected additional child element of <attribute>",
        ));
    }
    Ok(AttributePattern {
        span: node.range().start + 1..node.range().start + 10,
        name_class,
        pattern: Box::new(pattern),
    })
}

fn group(node: Node) -> Result<Pattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(Pattern::Group(Box::new(patt)))
}
fn interleave(node: Node) -> Result<Pattern> {
    let mut child = first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?;
    let mut pat = pattern(child)?;
    while let Some(next) = next_rng_sibling(child) {
        let this = pattern(next)?;
        child = next;
        pat = Pattern::InterleavePair(Box::new(pat), Box::new(this));
    }
    Ok(pat)
}

fn choice(node: Node) -> Result<Pattern> {
    let mut child = first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?;
    let mut pat = pattern(child)?;
    while let Some(node) = next_rng_sibling(child) {
        let this = pattern(node)?;
        pat = Pattern::ChoicePair(Box::new(pat), Box::new(this));
        child = node;
    }
    Ok(pat)
}

fn optional(node: Node) -> Result<Pattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(Pattern::Optional(Box::new(patt)))
}

fn zero_or_more(node: Node) -> Result<Pattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(Pattern::ZeroOrMore(Box::new(patt)))
}

fn one_or_more(node: Node) -> Result<Pattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(Pattern::OneOrMore(Box::new(patt)))
}

fn list(node: Node) -> Result<ListPattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(ListPattern(Box::new(patt)))
}

fn mixed(node: Node) -> Result<MixedPattern> {
    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "pattern child"))?,
    )?;
    Ok(MixedPattern(Box::new(patt)))
}

fn ref_patt(node: Node) -> Result<Identifier> {
    no_rng_element_children(node)?;
    if let Some(name) = node.attribute_node("name") {
        attr_ncname(&name)
    } else {
        Err(Error::Expected(node.range(), "name attribute"))
    }
}

fn parent_ref(node: Node) -> Result<Identifier> {
    no_rng_element_children(node)?;
    if let Some(name) = node.attribute_node("name") {
        attr_ncname(&name)
    } else {
        Err(Error::Expected(node.range(), "name attribute"))
    }
}

fn empty(node: Node) -> Result<Pattern> {
    no_attrs(node)?;
    no_rng_element_children(node)?;
    Ok(Pattern::Empty)
}

fn text(node: Node) -> Result<Pattern> {
    no_attrs(node)?;
    no_rng_element_children(node)?;
    Ok(Pattern::Text(Some(node.range())))
}

fn value(node: Node) -> Result<DatatypeValuePattern> {
    no_element_children(node)?;
    // TODO: have a better representation for 'unspecified' (default) datatype namespace that
    // doesn't need a bogus 0..0 span,
    let datatype_ns = get_datatype_lib(node).unwrap_or(Literal::new(0..0, "".to_string()));
    let type_name = node
        .attribute_node("type")
        .map(|attr| ncname(attr.range_value(), attr.value()))
        .transpose()?;
    let val = if let Some(child) = node.first_child() {
        let seg = LiteralSegment {
            body: child
                .text()
                .ok_or(Error::Expected(node.range(), "Text content"))?
                .to_string(),
        };
        Literal(child.range(), vec![seg])
    } else {
        // treat the value as ""
        let seg = LiteralSegment {
            body: "".to_string(),
        };
        Literal(node.range(), vec![seg])
    };
    let ns = get_ns_att(node).map(|a| a.value().to_string());
    // Capture in-scope namespace prefix bindings for resolving prefixed values (e.g., QNames)
    let ns_bindings: Vec<(String, String)> = node
        .namespaces()
        .filter_map(|n| {
            n.name()
                .map(|prefix| (prefix.to_string(), n.uri().to_string()))
        })
        .collect();
    Ok(DatatypeValuePattern(
        node.range(),
        type_name.map(|name| {
            DatatypeName::NamespacedName(NamespacedName {
                namespace_uri: datatype_ns,
                localname: name,
            })
        }),
        val,
        ns,
        ns_bindings,
    ))
}

fn data(node: Node) -> Result<DatatypeNamePattern> {
    // TODO: have a better representation for 'unspecified' (default) datatype namespace that
    // doesn't need a bogus 0..0 span,
    let datatype_ns = get_datatype_lib(node).unwrap_or(Literal::new(0..0, "".to_string()));
    let type_attr = node
        .attribute_node("type")
        .ok_or(Error::Expected(node.range(), "type attribute"))?;
    let type_name = match type_attr.value().trim() {
        // TODO: check datatypeLibrary namespace!
        "token" => DatatypeName::Token,
        val => {
            let name = ncname(type_attr.range_value(), val)?;
            DatatypeName::NamespacedName(NamespacedName {
                namespace_uri: datatype_ns,
                localname: name,
            })
        }
    };
    if let Some(a) = node
        .attributes()
        .find(|a| is_rng_att(a) && a.name() != "type" && a.name() != "datatypeLibrary")
    {
        return Err(Error::Unexpected(a.range(), "Unexpected attribute"));
    }

    let (params, rest) = if let Some(child) = first_rng_child(node) {
        params(child)?
    } else {
        (vec![], None)
    };
    let (except, rest) = if let Some(node) = rest {
        if !is_el(node, "except") {
            return Err(Error::Unexpected(node.range(), "Unexpected element"));
        }
        let except_patt =
            first_rng_child(node).ok_or(Error::Expected(node.range(), "child pattern element"))?;
        (
            Some(single_pattern_or_choice(except_patt)?),
            next_rng_sibling(node),
        )
    } else {
        (None, rest)
    };
    if let Some(rest) = rest {
        return Err(Error::Unexpected(rest.range(), "Unexpected element"));
    }
    Ok(DatatypeNamePattern(
        node.range(),
        type_name,
        Some(params),
        except.map(Box::new),
    ))
}

fn single_pattern_or_choice(mut node: Node) -> Result<Pattern> {
    let mut pat = pattern(node)?;
    while let Some(next) = next_rng_sibling(node) {
        let this = pattern(next)?;
        node = next;
        pat = Pattern::ChoicePair(Box::new(pat), Box::new(this));
    }
    Ok(pat)
}

fn params<'a, 'input: 'a>(
    node: Node<'a, 'input>,
) -> Result<(Vec<Param>, Option<Node<'a, 'input>>)> {
    let mut result = vec![];
    let mut next = Some(node);
    while let Some(node) = next {
        if is_el(node, "param") {
            result.push(param(node)?);
        } else {
            break;
        }
        next = next_rng_sibling(node);
    }
    Ok((result, next))
}

fn param(node: Node) -> Result<Param> {
    let name = node
        .attribute_node("name")
        .ok_or(Error::Expected(node.range(), "name attribute"))?;
    let name = attr_ncname(&name)?;

    let child = node
        .first_child()
        .ok_or(Error::Expected(node.range(), "Text content"))?;
    let seg = LiteralSegment {
        body: child
            .text()
            .ok_or(Error::Expected(node.range(), "Text content"))?
            .to_string(),
    };
    let val = Literal(child.range(), vec![seg]);

    Ok(Param {
        span: node.range(),
        annotations: None,
        name: IdentifierOrKeyword::Identifier(name),
        value: val,
    })
}

fn not_allowed(node: Node) -> Result<Pattern> {
    no_attrs(node)?;
    no_rng_element_children(node)?;
    Ok(Pattern::NotAllowed)
}

fn external_ref(node: Node) -> Result<ExternalPattern> {
    let href = node
        .attribute_node("href")
        .ok_or(Error::Expected(node.range(), "href attribute"))?;

    // TODO: ExternalPattern's requirement for a 'literal' here is inconvenient, and its API should
    //       be altered to make using it here simpler
    let seg = LiteralSegment {
        body: rebase_path(node, href.value())?,
    };
    let val = Literal(href.range_value(), vec![seg]);

    let ns = get_ns_att(node).map(|a| a.value().to_string());

    Ok(ExternalPattern(val, None, ns))
}

fn grammar(node: Node) -> Result<GrammarPattern> {
    let mut next = first_rng_child(node);
    let mut content = vec![];
    while let Some(child) = next {
        content.push(grammar_content(child)?);
        next = next_rng_sibling(child);
    }

    Ok(GrammarPattern {
        span: node.range(),
        content,
    })
}

fn grammar_content(node: Node) -> Result<GrammarContent> {
    if node.is_element()
        && node.tag_name().namespace() == Some("http://relaxng.org/ns/structure/1.0")
    {
        match node.tag_name().name() {
            "start" => start(node).map(GrammarContent::Define),
            "define" => define(node).map(GrammarContent::Define),
            "div" => div_grammar_content(node),
            "include" => include(node).map(GrammarContent::Include),
            _ => Err(Error::Expected(
                node.range(),
                "Expected <start> <define> <div> or <include> element",
            )),
        }
    } else {
        Err(Error::Expected(
            node.range(),
            "Expected <start> <define> <div> or <include> element",
        ))
    }
}

fn start(node: Node) -> Result<Define> {
    let combine = if let Some(combine) = node.attribute_node("combine") {
        match combine.value().trim() {
            "choice" => AssignMethod::Choice,
            "interleave" => AssignMethod::Interleave,
            _ => {
                return Err(Error::Expected(
                    combine.range_value(),
                    "Expected either \"choice\" or \"interleave\"",
                ));
            }
        }
    } else {
        AssignMethod::Assign
    };
    if let Some(a) = node
        .attributes()
        .find(|a| is_rng_att(a) && a.name() != "combine")
    {
        return Err(Error::Unexpected(a.range(), "Unexpected attribute"));
    }
    // we just produce another 'Define' named "start", rather than using a dedicated 'Start' type,
    // so as to avoid duplication of code handling 'start' definitions and other definitions
    // TODO: revisit this design later

    let start_span = node.range().start + 1..node.range().start + 6;
    let name = Identifier(start_span, "start".to_string());

    let child =
        first_rng_child(node).ok_or(Error::Expected(node.range(), "Child pattern element"))?;
    let patt = pattern(child)?;
    if let Some(rest) = next_rng_sibling(child) {
        return Err(Error::Unexpected(rest.range(), "Unexpected element"));
    }
    Ok(Define {
        span: node.range(),
        identifier: name,
        assign_method: combine,
        pattern: patt,
        annotations: None,
    })
}

fn define(node: Node) -> Result<Define> {
    let combine = if let Some(combine) = node.attribute_node("combine") {
        match combine.value().trim() {
            "choice" => AssignMethod::Choice,
            "interleave" => AssignMethod::Interleave,
            _ => {
                return Err(Error::Expected(
                    combine.range_value(),
                    "Expected either \"choice\" or \"interleave\"",
                ));
            }
        }
    } else {
        AssignMethod::Assign
    };

    let name = node
        .attribute_node("name")
        .ok_or(Error::Expected(node.range(), "name attribute"))?;
    let name = attr_ncname(&name)?;

    let patt = single_pattern_or_group(
        first_rng_child(node).ok_or(Error::Expected(node.range(), "Child pattern element"))?,
    )?;
    Ok(Define {
        span: node.range(),
        identifier: name,
        assign_method: combine,
        pattern: patt,
        annotations: None,
    })
}

fn div_grammar_content(node: Node) -> Result<GrammarContent> {
    let mut next = first_rng_child(node);
    let mut content = vec![];
    while let Some(child) = next {
        content.push(grammar_content(child)?);
        next = next_rng_sibling(child);
    }

    Ok(GrammarContent::Div(content))
}

fn attr_ncname(attr: &Attribute) -> Result<Identifier> {
    // TODO: further checks
    if attr.value().contains(':') {
        Err(Error::Unexpected(attr.range_value(), "Colon in NCName"))
    } else {
        ident(attr.range_value(), attr.value().trim())
    }
}

fn rebase_path(node: Node, href: &str) -> Result<String> {
    let bases = node
        .ancestors()
        .filter_map(|node| node.attribute_node(("http://www.w3.org/XML/1998/namespace", "base")))
        .collect::<Vec<_>>();
    let mut result: Option<PathBuf> = None;
    for base in bases.iter().rev() {
        let new = base.value();
        resolve(&mut result, new);
    }
    resolve(&mut result, href);
    Ok(result.unwrap().to_str().unwrap().to_string())
}

fn resolve(result: &mut Option<PathBuf>, new: &str) {
    *result = Some(if new.starts_with("/") {
        PathBuf::from(new)
    } else {
        match *result {
            Some(ref mut old) => {
                if !old.to_str().unwrap().ends_with("/") {
                    old.pop();
                }
                old.push(new);
                old.clone()
            }
            None => PathBuf::from(new),
        }
    });
}

fn include(node: Node) -> Result<Include> {
    let href = node
        .attribute_node("href")
        .ok_or(Error::Expected(node.range(), "href attribute"))?;

    let seg = LiteralSegment {
        body: rebase_path(node, href.value())?,
    };
    let val = Literal(href.range_value(), vec![seg]);

    let mut next = first_rng_child(node);
    let mut content = vec![];
    while let Some(child) = next {
        content.push(include_content(child)?);
        next = next_rng_sibling(child);
    }

    let ns = get_ns_att(node).map(|a| a.value().to_string());

    Ok(Include {
        uri: val,
        inherit: None,
        content: Some(content),
        annotations: None,
        ns,
    })
}

fn include_content(node: Node) -> Result<IncludeContent> {
    if node.is_element()
        && node.tag_name().namespace() == Some("http://relaxng.org/ns/structure/1.0")
    {
        match node.tag_name().name() {
            "start" => start(node).map(IncludeContent::Define),
            "define" => define(node).map(IncludeContent::Define),
            "div" => div_include_content(node),
            _ => Err(Error::Expected(
                node.range(),
                "Expected <start> <define> or <div> element",
            )),
        }
    } else {
        Err(Error::Expected(
            node.range(),
            "Expected <start> <define> or <div> element",
        ))
    }
}

fn div_include_content(node: Node) -> Result<IncludeContent> {
    let mut next = first_rng_child(node);
    let mut content = vec![];
    while let Some(child) = next {
        content.push(include_content(child)?);
        next = next_rng_sibling(child);
    }

    Ok(IncludeContent::Div(content))
}

/// Resolve a QName from a `name` attribute on an `<element>` or `<attribute>` element.
/// For `<attribute>`, the `ns` attribute defaults to "" (does not inherit from ancestors)
/// per RELAX NG spec section 4.3.
fn qname_att(node: Node, name: &Attribute, inherit_ns: bool) -> Result<Name> {
    let val = name.value();
    if let Some(pos) = val.find(':') {
        let start = name.range_value().start;
        let end = name.range_value().end;
        let prefix = &val[0..pos];
        let prefix_span = start..(start + pos);
        let namespace = lookup_namespace_def(node, Some(prefix.trim()))
            .ok_or_else(|| Error::Unexpected(prefix_span.clone(), "Unknown namespace prefix"))?;
        let localname = ncname(start + pos + 1..end, &val[pos + 1..])?;
        let ns = Literal::new(prefix_span, namespace.to_string());
        Ok(Name::NamespacedName(NamespacedName {
            namespace_uri: ns,
            localname,
        }))
    } else {
        let ns = if inherit_ns {
            // For <element>: inherit ns from nearest ancestor
            get_ns(node).unwrap_or(Literal::new(0..0, String::new()))
        } else {
            // For <attribute>: only use ns if directly on this element, default to ""
            node.attribute_node("ns")
                .map(|a| Literal::new(a.range_value(), a.value().to_string()))
                .unwrap_or(Literal::new(0..0, String::new()))
        };
        let localname = ncname(name.range_value(), val)?;
        Ok(Name::NamespacedName(NamespacedName {
            namespace_uri: ns,
            localname,
        }))
    }
}

fn qname_el(name: Node) -> Result<Name> {
    no_element_children(name)?;
    if let Some(val) = name.text() {
        if let Some(pos) = val.find(':') {
            let start = name.range().start;
            let end = name.range().end;
            let prefix = ncname(start..(start + pos), val[0..pos].trim())?;
            let localname = ncname(start + pos + 1..end, val[pos + 1..].trim())?;
            Ok(Name::CName(QName(prefix, localname)))
        } else {
            let ns = get_ns(name).unwrap_or(Literal::new(0..0, "".to_string())); // TODO allow None or something rather than inventing an 'empty' NcName
            let localname = ncname(name.first_child().unwrap().range(), val)?;
            Ok(Name::NamespacedName(NamespacedName {
                namespace_uri: ns,
                localname,
            }))
        }
    } else {
        Err(Error::Expected(name.range(), "Text contents"))
    }
}

fn ncname(range: Range<usize>, val: &str) -> Result<NcName> {
    let val = val.trim();
    if val.is_empty() {
        return Err(Error::Expected(range, "Expected identifier"));
    }
    for (i, c) in val.char_indices() {
        if i == 0 {
            if !is_nc_name_start_char(c) {
                return Err(Error::Unexpected(
                    range.start..range.start + 1,
                    "Unexpected character for NcName",
                ));
            }
        } else if !is_nc_name_char(c) {
            println!("NcName Urk! {val:?}");
            return Err(Error::Unexpected(
                range.start + i..range.start + i + 1,
                "Unexpected character for NcName",
            ));
        }
    }
    Ok(NcName(range, val.to_string()))
}

fn lookup_namespace_def<'a, 'input: 'a>(
    node: Node<'a, 'input>,
    prefix: Option<&str>,
) -> Option<&'a str> {
    if prefix == Some("xml") {
        Some("http://www.w3.org/XML/1998/namespace")
    } else {
        node.namespaces()
            .find(|ns| ns.name() == prefix)
            .map(|n| n.uri())
    }
}

fn ident(range: Range<usize>, val: &str) -> Result<Identifier> {
    let val = val.trim();
    if val.is_empty() {
        return Err(Error::Expected(range, "Expected identifier"));
    }
    for (i, c) in val.char_indices() {
        if i == 0 {
            if !is_nc_name_start_char(c) {
                return Err(Error::Unexpected(
                    range.start..range.start + 1,
                    "Unexpected character within identifier",
                ));
            }
        } else if !is_nc_name_char(c) {
            return Err(Error::Unexpected(
                range.start + i..range.start + i + 1,
                "Unexpected character within identifier",
            ));
        }
    }
    Ok(Identifier(range, val.to_string()))
}

/// XML 1.0 4th Edition BaseChar production (Appendix B)
fn is_base_char(c: char) -> bool {
    matches!(c,
        '\u{0041}'..='\u{005A}' | '\u{0061}'..='\u{007A}' | '\u{00C0}'..='\u{00D6}'
        | '\u{00D8}'..='\u{00F6}' | '\u{00F8}'..='\u{00FF}' | '\u{0100}'..='\u{0131}'
        | '\u{0134}'..='\u{013E}' | '\u{0141}'..='\u{0148}' | '\u{014A}'..='\u{017E}'
        | '\u{0180}'..='\u{01C3}' | '\u{01CD}'..='\u{01F0}' | '\u{01F4}'..='\u{01F5}'
        | '\u{01FA}'..='\u{0217}' | '\u{0250}'..='\u{02A8}' | '\u{02BB}'..='\u{02C1}'
        | '\u{0386}' | '\u{0388}'..='\u{038A}' | '\u{038C}'
        | '\u{038E}'..='\u{03A1}' | '\u{03A3}'..='\u{03CE}' | '\u{03D0}'..='\u{03D6}'
        | '\u{03DA}' | '\u{03DC}' | '\u{03DE}' | '\u{03E0}'
        | '\u{03E2}'..='\u{03F3}' | '\u{0401}'..='\u{040C}' | '\u{040E}'..='\u{044F}'
        | '\u{0451}'..='\u{045C}' | '\u{045E}'..='\u{0481}' | '\u{0490}'..='\u{04C4}'
        | '\u{04C7}'..='\u{04C8}' | '\u{04CB}'..='\u{04CC}' | '\u{04D0}'..='\u{04EB}'
        | '\u{04EE}'..='\u{04F5}' | '\u{04F8}'..='\u{04F9}' | '\u{0531}'..='\u{0556}'
        | '\u{0559}' | '\u{0561}'..='\u{0586}' | '\u{05D0}'..='\u{05EA}'
        | '\u{05F0}'..='\u{05F2}' | '\u{0621}'..='\u{063A}' | '\u{0641}'..='\u{064A}'
        | '\u{0671}'..='\u{06B7}' | '\u{06BA}'..='\u{06BE}' | '\u{06C0}'..='\u{06CE}'
        | '\u{06D0}'..='\u{06D3}' | '\u{06D5}' | '\u{06E5}'..='\u{06E6}'
        | '\u{0905}'..='\u{0939}' | '\u{093D}' | '\u{0958}'..='\u{0961}'
        | '\u{0985}'..='\u{098C}' | '\u{098F}'..='\u{0990}' | '\u{0993}'..='\u{09A8}'
        | '\u{09AA}'..='\u{09B0}' | '\u{09B2}' | '\u{09B6}'..='\u{09B9}'
        | '\u{09DC}'..='\u{09DD}' | '\u{09DF}'..='\u{09E1}' | '\u{09F0}'..='\u{09F1}'
        | '\u{0A05}'..='\u{0A0A}' | '\u{0A0F}'..='\u{0A10}' | '\u{0A13}'..='\u{0A28}'
        | '\u{0A2A}'..='\u{0A30}' | '\u{0A32}'..='\u{0A33}' | '\u{0A35}'..='\u{0A36}'
        | '\u{0A38}'..='\u{0A39}' | '\u{0A59}'..='\u{0A5C}' | '\u{0A5E}'
        | '\u{0A72}'..='\u{0A74}' | '\u{0A85}'..='\u{0A8B}' | '\u{0A8D}'
        | '\u{0A8F}'..='\u{0A91}' | '\u{0A93}'..='\u{0AA8}' | '\u{0AAA}'..='\u{0AB0}'
        | '\u{0AB2}'..='\u{0AB3}' | '\u{0AB5}'..='\u{0AB9}' | '\u{0ABD}' | '\u{0AE0}'
        | '\u{0B05}'..='\u{0B0C}' | '\u{0B0F}'..='\u{0B10}' | '\u{0B13}'..='\u{0B28}'
        | '\u{0B2A}'..='\u{0B30}' | '\u{0B32}'..='\u{0B33}' | '\u{0B36}'..='\u{0B39}'
        | '\u{0B3D}' | '\u{0B5C}'..='\u{0B5D}' | '\u{0B5F}'..='\u{0B61}'
        | '\u{0B85}'..='\u{0B8A}' | '\u{0B8E}'..='\u{0B90}' | '\u{0B92}'..='\u{0B95}'
        | '\u{0B99}'..='\u{0B9A}' | '\u{0B9C}' | '\u{0B9E}'..='\u{0B9F}'
        | '\u{0BA3}'..='\u{0BA4}' | '\u{0BA8}'..='\u{0BAA}' | '\u{0BAE}'..='\u{0BB5}'
        | '\u{0BB7}'..='\u{0BB9}' | '\u{0C05}'..='\u{0C0C}' | '\u{0C0E}'..='\u{0C10}'
        | '\u{0C12}'..='\u{0C28}' | '\u{0C2A}'..='\u{0C33}' | '\u{0C35}'..='\u{0C39}'
        | '\u{0C60}'..='\u{0C61}' | '\u{0C85}'..='\u{0C8C}' | '\u{0C8E}'..='\u{0C90}'
        | '\u{0C92}'..='\u{0CA8}' | '\u{0CAA}'..='\u{0CB3}' | '\u{0CB5}'..='\u{0CB9}'
        | '\u{0CDE}' | '\u{0CE0}'..='\u{0CE1}' | '\u{0D05}'..='\u{0D0C}'
        | '\u{0D0E}'..='\u{0D10}' | '\u{0D12}'..='\u{0D28}' | '\u{0D2A}'..='\u{0D39}'
        | '\u{0D60}'..='\u{0D61}' | '\u{0E01}'..='\u{0E2E}' | '\u{0E30}'
        | '\u{0E32}'..='\u{0E33}' | '\u{0E40}'..='\u{0E45}' | '\u{0E81}'..='\u{0E82}'
        | '\u{0E84}' | '\u{0E87}'..='\u{0E88}' | '\u{0E8A}' | '\u{0E8D}'
        | '\u{0E94}'..='\u{0E97}' | '\u{0E99}'..='\u{0E9F}' | '\u{0EA1}'..='\u{0EA3}'
        | '\u{0EA5}' | '\u{0EA7}' | '\u{0EAA}'..='\u{0EAB}'
        | '\u{0EAD}'..='\u{0EAE}' | '\u{0EB0}' | '\u{0EB2}'..='\u{0EB3}' | '\u{0EBD}'
        | '\u{0EC0}'..='\u{0EC4}' | '\u{0F40}'..='\u{0F47}' | '\u{0F49}'..='\u{0F69}'
        | '\u{10A0}'..='\u{10C5}' | '\u{10D0}'..='\u{10F6}' | '\u{1100}'
        | '\u{1102}'..='\u{1103}' | '\u{1105}'..='\u{1107}' | '\u{1109}'
        | '\u{110B}'..='\u{110C}' | '\u{110E}'..='\u{1112}' | '\u{113C}' | '\u{113E}'
        | '\u{1140}' | '\u{114C}' | '\u{114E}' | '\u{1150}'
        | '\u{1154}'..='\u{1155}' | '\u{1159}' | '\u{115F}'..='\u{1161}' | '\u{1163}'
        | '\u{1165}' | '\u{1167}' | '\u{1169}' | '\u{116D}'..='\u{116E}'
        | '\u{1172}'..='\u{1173}' | '\u{1175}' | '\u{119E}' | '\u{11A8}' | '\u{11AB}'
        | '\u{11AE}'..='\u{11AF}' | '\u{11B7}'..='\u{11B8}' | '\u{11BA}'
        | '\u{11BC}'..='\u{11C2}' | '\u{11EB}' | '\u{11F0}' | '\u{11F9}'
        | '\u{1E00}'..='\u{1E9B}' | '\u{1EA0}'..='\u{1EF9}' | '\u{1F00}'..='\u{1F15}'
        | '\u{1F18}'..='\u{1F1D}' | '\u{1F20}'..='\u{1F45}' | '\u{1F48}'..='\u{1F4D}'
        | '\u{1F50}'..='\u{1F57}' | '\u{1F59}' | '\u{1F5B}' | '\u{1F5D}'
        | '\u{1F5F}'..='\u{1F7D}' | '\u{1F80}'..='\u{1FB4}' | '\u{1FB6}'..='\u{1FBC}'
        | '\u{1FBE}' | '\u{1FC2}'..='\u{1FC4}' | '\u{1FC6}'..='\u{1FCC}'
        | '\u{1FD0}'..='\u{1FD3}' | '\u{1FD6}'..='\u{1FDB}' | '\u{1FE0}'..='\u{1FEC}'
        | '\u{1FF2}'..='\u{1FF4}' | '\u{1FF6}'..='\u{1FFC}' | '\u{2126}'
        | '\u{212A}'..='\u{212B}' | '\u{212E}' | '\u{2180}'..='\u{2182}'
        | '\u{3041}'..='\u{3094}' | '\u{30A1}'..='\u{30FA}' | '\u{3105}'..='\u{312C}'
        | '\u{AC00}'..='\u{D7A3}'
    )
}

/// XML 1.0 4th Edition Ideographic production (Appendix B)
fn is_ideographic(c: char) -> bool {
    matches!(c, '\u{4E00}'..='\u{9FA5}' | '\u{3007}' | '\u{3021}'..='\u{3029}')
}

/// XML 1.0 4th Edition CombiningChar production (Appendix B)
fn is_combining_char(c: char) -> bool {
    matches!(c,
        '\u{0300}'..='\u{0345}' | '\u{0360}'..='\u{0361}' | '\u{0483}'..='\u{0486}'
        | '\u{0591}'..='\u{05A1}' | '\u{05A3}'..='\u{05B9}' | '\u{05BB}'..='\u{05BD}'
        | '\u{05BF}' | '\u{05C1}'..='\u{05C2}' | '\u{05C4}'
        | '\u{064B}'..='\u{0652}' | '\u{0670}' | '\u{06D6}'..='\u{06DC}'
        | '\u{06DD}'..='\u{06DF}' | '\u{06E0}'..='\u{06E4}' | '\u{06E7}'..='\u{06E8}'
        | '\u{06EA}'..='\u{06ED}' | '\u{0901}'..='\u{0903}' | '\u{093C}'
        | '\u{093E}'..='\u{094C}' | '\u{094D}' | '\u{0951}'..='\u{0954}'
        | '\u{0962}'..='\u{0963}' | '\u{0981}'..='\u{0983}' | '\u{09BC}'
        | '\u{09BE}' | '\u{09BF}' | '\u{09C0}'..='\u{09C4}'
        | '\u{09C7}'..='\u{09C8}' | '\u{09CB}'..='\u{09CD}' | '\u{09D7}'
        | '\u{09E2}'..='\u{09E3}' | '\u{0A02}' | '\u{0A3C}' | '\u{0A3E}' | '\u{0A3F}'
        | '\u{0A40}'..='\u{0A42}' | '\u{0A47}'..='\u{0A48}' | '\u{0A4B}'..='\u{0A4D}'
        | '\u{0A70}'..='\u{0A71}' | '\u{0A81}'..='\u{0A83}' | '\u{0ABC}'
        | '\u{0ABE}'..='\u{0AC5}' | '\u{0AC7}'..='\u{0AC9}' | '\u{0ACB}'..='\u{0ACD}'
        | '\u{0B01}'..='\u{0B03}' | '\u{0B3C}' | '\u{0B3E}'..='\u{0B43}'
        | '\u{0B47}'..='\u{0B48}' | '\u{0B4B}'..='\u{0B4D}' | '\u{0B56}'..='\u{0B57}'
        | '\u{0B82}'..='\u{0B83}' | '\u{0BBE}'..='\u{0BC2}' | '\u{0BC6}'..='\u{0BC8}'
        | '\u{0BCA}'..='\u{0BCD}' | '\u{0BD7}' | '\u{0C01}'..='\u{0C03}'
        | '\u{0C3E}'..='\u{0C44}' | '\u{0C46}'..='\u{0C48}' | '\u{0C4A}'..='\u{0C4D}'
        | '\u{0C55}'..='\u{0C56}' | '\u{0C82}'..='\u{0C83}' | '\u{0CBE}'..='\u{0CC4}'
        | '\u{0CC6}'..='\u{0CC8}' | '\u{0CCA}'..='\u{0CCD}' | '\u{0CD5}'..='\u{0CD6}'
        | '\u{0D02}'..='\u{0D03}' | '\u{0D3E}'..='\u{0D43}' | '\u{0D46}'..='\u{0D48}'
        | '\u{0D4A}'..='\u{0D4D}' | '\u{0D57}' | '\u{0E31}' | '\u{0E34}'..='\u{0E3A}'
        | '\u{0E47}'..='\u{0E4E}' | '\u{0EB1}' | '\u{0EB4}'..='\u{0EB9}'
        | '\u{0EBB}'..='\u{0EBC}' | '\u{0EC8}'..='\u{0ECD}' | '\u{0F18}'..='\u{0F19}'
        | '\u{0F35}' | '\u{0F37}' | '\u{0F39}' | '\u{0F3E}' | '\u{0F3F}'
        | '\u{0F71}'..='\u{0F84}' | '\u{0F86}'..='\u{0F8B}' | '\u{0F90}'..='\u{0F95}'
        | '\u{0F97}' | '\u{0F99}'..='\u{0FAD}' | '\u{0FB1}'..='\u{0FB7}' | '\u{0FB9}'
        | '\u{20D0}'..='\u{20DC}' | '\u{20E1}' | '\u{302A}'..='\u{302F}'
        | '\u{3099}' | '\u{309A}'
    )
}

/// XML 1.0 4th Edition Digit production (Appendix B)
fn is_xml_digit(c: char) -> bool {
    matches!(c,
        '\u{0030}'..='\u{0039}' | '\u{0660}'..='\u{0669}' | '\u{06F0}'..='\u{06F9}'
        | '\u{0966}'..='\u{096F}' | '\u{09E6}'..='\u{09EF}' | '\u{0A66}'..='\u{0A6F}'
        | '\u{0AE6}'..='\u{0AEF}' | '\u{0B66}'..='\u{0B6F}' | '\u{0BE7}'..='\u{0BEF}'
        | '\u{0C66}'..='\u{0C6F}' | '\u{0CE6}'..='\u{0CEF}' | '\u{0D66}'..='\u{0D6F}'
        | '\u{0E50}'..='\u{0E59}' | '\u{0ED0}'..='\u{0ED9}' | '\u{0F20}'..='\u{0F29}'
    )
}

/// XML 1.0 4th Edition Extender production (Appendix B)
fn is_extender(c: char) -> bool {
    matches!(c,
        '\u{00B7}' | '\u{02D0}' | '\u{02D1}' | '\u{0387}' | '\u{0640}' | '\u{0E46}'
        | '\u{0EC6}' | '\u{3005}' | '\u{3031}'..='\u{3035}' | '\u{309D}'..='\u{309E}'
        | '\u{30FC}'..='\u{30FE}'
    )
}

/// NCName start character: Letter (BaseChar | Ideographic) | '_'
/// per XML 1.0 4th Edition + XML Namespaces
fn is_nc_name_start_char(c: char) -> bool {
    c == '_' || is_base_char(c) || is_ideographic(c)
}

/// NCName continuation character: Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
/// per XML 1.0 4th Edition + XML Namespaces (minus ':')
fn is_nc_name_char(c: char) -> bool {
    is_nc_name_start_char(c)
        || is_xml_digit(c)
        || is_combining_char(c)
        || is_extender(c)
        || c == '.'
        || c == '-'
}

fn is_el(node: Node, name: &'static str) -> bool {
    node.is_element() && node.tag_name().name() == name && is_rng_node(node)
}

fn is_rng_node(node: Node) -> bool {
    is_rng(node.tag_name().namespace())
}

fn is_rng_att(a: &Attribute) -> bool {
    // if an attribute has no namespace prefix, then it is not formally in the default namesapce
    // of the document (as an element with no namespace prefix would be).  Here we assume that we
    // only ever call this function on an attribute that was on an element within the relaxng
    // namespace, and that in the context of relaxng elements unprefixed attributes are relaxng
    // attributes :)
    a.namespace().is_none() || is_rng(a.namespace())
}

fn is_rng(ns: Option<&str>) -> bool {
    ns == Some("http://relaxng.org/ns/structure/1.0")
}

fn no_attrs(node: Node) -> Result<()> {
    if let Some(att) = node.attributes().find(|a| is_rng_att(a)) {
        Err(Error::Unexpected(att.range(), "Unexpected attribute"))
    } else {
        Ok(())
    }
}

fn no_rng_element_children(node: Node) -> Result<()> {
    if let Some(child) = first_rng_child(node) {
        Err(Error::Unexpected(child.range(), "Unexpected child element"))
    } else {
        Ok(())
    }
}

fn no_element_children(node: Node) -> Result<()> {
    if let Some(child) = node.first_element_child() {
        Err(Error::Unexpected(child.range(), "Unexpected child element"))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::types::*;
    use assert_matches::*;

    #[test]
    fn it_works() {
        let doc = roxmltree::Document::parse(
            "<?xml version = '1.0' encoding = 'utf-8' ?>
 <element xmlns=\"http://relaxng.org/ns/structure/1.0\" name=\"library\"><text/></element>",
        )
        .expect("Parsing XML");
        let result = super::pattern(doc.root_element()).unwrap();
        if let Pattern::Element(el) = result {
            assert_matches!(
                el.name_class,
                NameClass::Name(Name::NamespacedName(NamespacedName { namespace_uri: _, localname: NcName(_, name)})) if name == "library"
            )
        } else {
            panic!("Expected an <element>")
        }
    }
}
