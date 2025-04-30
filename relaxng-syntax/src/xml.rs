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
        if val == "" {
            Ok(())
        } else {
            // TODO: move these checks into relaxng-model crate
            if !rfc2396::validate(val) {
                Err(Error::Unexpected(
                    dt_lib.range_value(),
                    "Datatype library URI is invalid",
                ))
            } else {
                match url::Url::parse(val) {
                    Ok(url) => {
                        if url.fragment().is_some() {
                            Err(Error::Unexpected(
                                dt_lib.range_value(),
                                "Datatype library URI must not include a fragment identifier",
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
        let name_class = NameClass::Name(qname_att(node, &name)?);
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
    if let Some(ns) = get_ns_att(node) {
        Some(Literal::new(ns.range_value(), ns.value().to_string()))
    } else {
        None
    }
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
    if let Some(ns) = get_dt_lib_att(node) {
        Some(Literal::new(ns.range_value(), ns.value().to_string()))
    } else {
        None
    }
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
    let ns = get_ns_att(node);
    let (name_class, rest) = if let Some(name) = node.attribute_node("name") {
        if name.value() == "xmlns" && (ns == None || ns.unwrap().value() == "") {
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
            NameClass::Name(qname_att(node, &name)?),
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
        (Pattern::Text, None)
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
    Ok(Pattern::Text)
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
    Ok(DatatypeValuePattern(
        type_name.map(|name| {
            DatatypeName::NamespacedName(NamespacedName {
                namespace_uri: datatype_ns,
                localname: name,
            })
        }),
        val,
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

    Ok(Param(
        node.range(),
        None,
        IdentifierOrKeyword::Identifier(name),
        val,
    ))
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

    Ok(ExternalPattern(val, None))
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
                ))
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
    Ok(Define(node.range(), name, combine, patt))
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
                ))
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
    Ok(Define(node.range(), name, combine, patt))
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

    Ok(Include(val, None, Some(content)))
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

fn qname_att(node: Node, name: &Attribute) -> Result<Name> {
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
        let ns = get_ns(node).unwrap_or(Literal::new(0..0, String::new())); // TODO allow None or something rather than inventing an 'empty' NcName
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
            println!("NcName Urk! {:?}", val);
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

fn is_nc_name_start_char(c: char) -> bool {
    matches!(c, 'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}')
}
fn is_nc_name_char(c: char) -> bool {
    if is_nc_name_start_char(c) {
        true
    } else {
        matches!(c, '-'
            | '.'
            | '0'..='9'
            | '\u{B7}'
            | '\u{0300}'..='\u{036F}'
            | '\u{203F}'..='\u{2040}')
    }
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
