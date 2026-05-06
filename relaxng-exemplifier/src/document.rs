use arbitrary::Unstructured;
use relaxng_model::model::Pattern;
use std::collections::HashMap;

use crate::analysis::Analysis;
use crate::datatypes::{gen_ncname, gen_short_ascii, generate_datatype, generate_datatype_value};
use crate::names::{NameContext, Vocabulary, pick_name};

/// An XML attribute (namespace, local-name, value).
#[derive(Debug, Clone)]
pub struct XmlAttr {
    pub namespace_uri: String,
    pub local_name: String,
    pub value: String,
}

/// An XML content node (element or text).
#[derive(Debug, Clone)]
pub enum XmlContent {
    Element(XmlElement),
    Text(String),
}

/// An in-memory XML element.
#[derive(Debug, Clone)]
pub struct XmlElement {
    pub namespace_uri: String,
    pub local_name: String,
    pub attrs: Vec<XmlAttr>,
    pub children: Vec<XmlContent>,
}

/// The output of generating a pattern: pending attributes and child content.
#[derive(Debug, Default)]
pub struct Output {
    /// Attributes to be applied to the enclosing element.
    pub attrs: Vec<XmlAttr>,
    /// Child elements and text.
    pub children: Vec<XmlContent>,
}

impl Output {
    pub(crate) fn text(t: String) -> Self {
        Output {
            attrs: vec![],
            children: vec![XmlContent::Text(t)],
        }
    }

    pub(crate) fn element(elem: XmlElement) -> Self {
        Output {
            attrs: vec![],
            children: vec![XmlContent::Element(elem)],
        }
    }

    pub(crate) fn attr(a: XmlAttr) -> Self {
        Output {
            attrs: vec![a],
            children: vec![],
        }
    }

    pub(crate) fn merge(&mut self, other: Output) {
        self.attrs.extend(other.attrs);
        self.children.extend(other.children);
    }
}

/// Generate content matching `pattern`, consuming randomness from `u` and respecting fuel.
///
/// When `u` runs out of bytes, falls back to minimum-cost completions.
pub fn generate_pattern(
    pattern: &Pattern,
    u: &mut Unstructured,
    fuel: usize,
    analysis: &Analysis,
    vocab: &Vocabulary,
) -> Output {
    match pattern {
        Pattern::Empty(_) => Output::default(),

        Pattern::Text(_) => Output::text(gen_short_ascii(u)),

        Pattern::NotAllowed(_) => Output::default(),

        Pattern::DatatypeValue { datatype, .. } => Output::text(generate_datatype_value(datatype)),

        Pattern::DatatypeName {
            datatype,
            except: _,
            ..
        } => Output::text(generate_datatype(datatype, u)),

        Pattern::Choice(branches, _) => {
            // Filter to branches feasible within the current fuel budget
            let feasible: Vec<&Pattern> = branches
                .iter()
                .filter(|b| analysis.min_fuel(b) <= fuel)
                .collect();

            let chosen = if feasible.is_empty() {
                // All branches over budget: pick the cheapest nullable branch, or the cheapest
                branches
                    .iter()
                    .filter(|b| analysis.nullable(b))
                    .min_by_key(|b| analysis.min_fuel(b))
                    .or_else(|| branches.iter().min_by_key(|b| analysis.min_fuel(b)))
            } else {
                u.choose(&feasible).ok().copied()
            };

            if let Some(branch) = chosen {
                generate_pattern(branch, u, fuel, analysis, vocab)
            } else {
                Output::default()
            }
        }

        Pattern::Group(patterns, _) => {
            let mut output = Output::default();
            let mut remaining_fuel = fuel;
            let count = patterns.len();
            for (i, p) in patterns.iter().enumerate() {
                // Reserve fuel for remaining mandatory patterns
                let reserved: usize = patterns[i + 1..]
                    .iter()
                    .map(|pp| analysis.min_fuel(pp))
                    .fold(0, |a, b| a.saturating_add(b));
                let this_fuel = remaining_fuel.saturating_sub(reserved);
                let sub = generate_pattern(p, u, this_fuel, analysis, vocab);
                // Deduct used fuel (approximate: use min_fuel as the "used" amount)
                let used = analysis.min_fuel(p).min(remaining_fuel);
                remaining_fuel = remaining_fuel.saturating_sub(used);
                output.merge(sub);
                let _ = count; // suppress unused warning
            }
            output
        }

        Pattern::Interleave(patterns, _) => {
            // Generate each sub-pattern, then shuffle the children
            let mut all_attrs = Vec::new();
            let mut all_children: Vec<Vec<XmlContent>> = Vec::new();

            for p in patterns {
                let sub = generate_pattern(p, u, fuel, analysis, vocab);
                all_attrs.extend(sub.attrs);
                all_children.push(sub.children);
            }

            // Shuffle the child groups using available randomness
            if all_children.len() > 1 {
                // Simple Fisher-Yates using the Unstructured bytes
                let mut indices: Vec<usize> = (0..all_children.len()).collect();
                for i in (1..indices.len()).rev() {
                    let j = u.int_in_range(0usize..=i).unwrap_or(0);
                    indices.swap(i, j);
                }
                let shuffled: Vec<Vec<XmlContent>> = indices
                    .into_iter()
                    .map(|idx| all_children[idx].clone())
                    .collect();
                all_children = shuffled;
            }

            Output {
                attrs: all_attrs,
                children: all_children.into_iter().flatten().collect(),
            }
        }

        Pattern::Optional(p, _) => {
            if fuel == 0 || analysis.min_fuel(p) > fuel {
                return Output::default();
            }
            let take = u.arbitrary::<bool>().unwrap_or(false);
            if take {
                generate_pattern(p, u, fuel, analysis, vocab)
            } else {
                Output::default()
            }
        }

        Pattern::ZeroOrMore(p, _) => {
            let min_f = analysis.min_fuel(p).max(1);
            let max_reps = (fuel / min_f).min(8);
            if max_reps == 0 {
                return Output::default();
            }
            let count = u.int_in_range(0usize..=max_reps).unwrap_or(0);
            let mut output = Output::default();
            for _ in 0..count {
                output.merge(generate_pattern(p, u, fuel, analysis, vocab));
            }
            output
        }

        Pattern::OneOrMore(p, _) => {
            let min_f = analysis.min_fuel(p).max(1);
            let max_reps = (fuel / min_f).max(1).min(8);
            let count = u.int_in_range(1usize..=max_reps).unwrap_or(1);
            let mut output = Output::default();
            for _ in 0..count {
                output.merge(generate_pattern(p, u, fuel, analysis, vocab));
            }
            output
        }

        Pattern::Element(nc, p, _, _) => {
            if fuel == 0 {
                return Output::default();
            }
            let (local_name, namespace_uri) = pick_name(nc, u, vocab, NameContext::Element)
                .unwrap_or_else(|_| ("elem".to_string(), String::new()));
            let mut content = generate_pattern(p, u, fuel.saturating_sub(1), analysis, vocab);
            // XML does not allow two attributes with the same expanded name on one element.
            // Deduplicate by (local_name, namespace_uri), keeping the first occurrence.
            let mut seen = std::collections::HashSet::new();
            content
                .attrs
                .retain(|a| seen.insert((a.local_name.clone(), a.namespace_uri.clone())));
            Output::element(XmlElement {
                namespace_uri,
                local_name,
                attrs: content.attrs,
                children: content.children,
            })
        }

        Pattern::Attribute(nc, p, _, _) => {
            let (local_name, namespace_uri) = pick_name(nc, u, vocab, NameContext::Attribute)
                .unwrap_or_else(|_| ("attr".to_string(), String::new()));
            let value = generate_text_value(p, u, fuel, analysis);
            Output::attr(XmlAttr {
                namespace_uri,
                local_name,
                value,
            })
        }

        Pattern::Mixed(p, _) => {
            // Generate p; text nodes are allowed to appear alongside elements
            let mut sub = generate_pattern(p, u, fuel, analysis, vocab);
            // Optionally prepend/append some text
            if u.arbitrary::<bool>().unwrap_or(false) {
                let t = gen_short_ascii(u);
                sub.children.insert(0, XmlContent::Text(t));
            }
            sub
        }

        Pattern::List(p, _) => {
            // The content of p should produce text; join multiple items with spaces
            let inner = generate_text_value(p, u, fuel, analysis);
            Output::text(inner)
        }

        Pattern::Ref(_, _, pat_ref) => {
            let borrow = pat_ref.0.borrow();
            if let Some(rule) = borrow.as_ref() {
                // fuel-1 guarantees termination for cyclic schemas
                generate_pattern(rule.pattern(), u, fuel.saturating_sub(1), analysis, vocab)
            } else {
                Output::default()
            }
        }
    }
}

/// Generate a string value for patterns that appear as attribute values or list content.
fn generate_text_value(
    pattern: &Pattern,
    u: &mut Unstructured,
    fuel: usize,
    analysis: &Analysis,
) -> String {
    match pattern {
        Pattern::Text(_) => gen_short_ascii(u),
        Pattern::DatatypeValue { datatype, .. } => generate_datatype_value(datatype),
        Pattern::DatatypeName { datatype, .. } => generate_datatype(datatype, u),
        Pattern::Choice(branches, _) => {
            let feasible: Vec<&Pattern> = branches
                .iter()
                .filter(|b| analysis.min_fuel(b) <= fuel)
                .collect();
            let chosen = u
                .choose(&feasible)
                .ok()
                .copied()
                .or_else(|| branches.first());
            chosen
                .map(|b| generate_text_value(b, u, fuel, analysis))
                .unwrap_or_default()
        }
        Pattern::Optional(p, _) => {
            let take = u.arbitrary::<bool>().unwrap_or(false);
            if take {
                generate_text_value(p, u, fuel, analysis)
            } else {
                String::new()
            }
        }
        Pattern::Group(pats, _) => pats
            .iter()
            .map(|p| generate_text_value(p, u, fuel, analysis))
            .collect::<Vec<_>>()
            .join(""),
        Pattern::Ref(_, _, pat_ref) => {
            let borrow = pat_ref.0.borrow();
            if let Some(rule) = borrow.as_ref() {
                generate_text_value(rule.pattern(), u, fuel.saturating_sub(1), analysis)
            } else {
                String::new()
            }
        }
        Pattern::Empty(_) => String::new(),
        _ => gen_ncname(u),
    }
}

/// Serialize the generated output to a well-formed XML document string.
///
/// When `pretty` is `true` the output is indented with two-space indentation.
/// Elements whose children are all text are kept on one line; elements with
/// element children are formatted in block style.
pub fn serialize_document(output: &Output, pretty: bool) -> String {
    let mut buf = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

    // Collect all namespace URIs used anywhere in the tree
    let mut all_namespaces: Vec<String> = Vec::new();
    collect_namespaces_output(output, &mut all_namespaces);
    all_namespaces.sort();
    all_namespaces.dedup();

    // The root element's namespace is the XML default namespace: it is declared
    // with xmlns="..." and its elements are written unprefixed.  All other
    // non-empty namespaces get short nsN prefixes.
    let default_ns: Option<&str> = output.children.iter().find_map(|c| match c {
        XmlContent::Element(e) if !e.namespace_uri.is_empty() => Some(e.namespace_uri.as_str()),
        _ => None,
    });

    let mut ns_pairs: Vec<(String, String)> = all_namespaces
        .iter()
        .filter(|uri| !uri.is_empty() && Some(uri.as_str()) != default_ns)
        .enumerate()
        .map(|(i, uri)| (uri.clone(), format!("ns{}", i)))
        .collect();
    ns_pairs.sort_by(|a, b| a.1.cmp(&b.1));
    let ns_map: HashMap<String, String> = ns_pairs.into_iter().collect();

    if pretty {
        buf.push('\n');
    }
    // Serialize root children directly so we can pass default_ns for the
    // xmlns="..." declaration; recursive content calls never need it.
    for child in &output.children {
        match child {
            XmlContent::Text(t) => buf.push_str(&escape_text(t)),
            XmlContent::Element(elem) => {
                if pretty {
                    serialize_element_pretty(elem, &mut buf, &ns_map, true, 0, default_ns);
                } else {
                    serialize_element(elem, &mut buf, &ns_map, true, default_ns);
                }
            }
        }
    }
    buf
}

fn collect_namespaces_output(output: &Output, acc: &mut Vec<String>) {
    for attr in &output.attrs {
        if !attr.namespace_uri.is_empty() {
            acc.push(attr.namespace_uri.clone());
        }
    }
    for child in &output.children {
        collect_namespaces_content(child, acc);
    }
}

fn collect_namespaces_content(content: &XmlContent, acc: &mut Vec<String>) {
    match content {
        XmlContent::Text(_) => {}
        XmlContent::Element(elem) => {
            if !elem.namespace_uri.is_empty() {
                acc.push(elem.namespace_uri.clone());
            }
            for attr in &elem.attrs {
                if !attr.namespace_uri.is_empty() {
                    acc.push(attr.namespace_uri.clone());
                }
            }
            for child in &elem.children {
                collect_namespaces_content(child, acc);
            }
        }
    }
}

fn qualified_name(local: &str, ns: &str, ns_map: &HashMap<String, String>) -> String {
    if ns.is_empty() {
        local.to_string()
    } else if let Some(prefix) = ns_map.get(ns) {
        format!("{}:{}", prefix, local)
    } else {
        local.to_string()
    }
}

fn serialize_content(
    content: &XmlContent,
    buf: &mut String,
    ns_map: &HashMap<String, String>,
    is_root: bool,
) {
    match content {
        XmlContent::Text(t) => buf.push_str(&escape_text(t)),
        XmlContent::Element(elem) => serialize_element(elem, buf, ns_map, is_root, None),
    }
}

fn serialize_element(
    elem: &XmlElement,
    buf: &mut String,
    ns_map: &HashMap<String, String>,
    declare_namespaces: bool,
    default_ns: Option<&str>,
) {
    let qname = qualified_name(&elem.local_name, &elem.namespace_uri, ns_map);
    buf.push('<');
    buf.push_str(&qname);

    // On the root element, declare the default namespace and all prefixed namespaces
    if declare_namespaces {
        if let Some(dns) = default_ns {
            buf.push_str(&format!(" xmlns=\"{}\"", escape_attr(dns)));
        }
        let mut pairs: Vec<(&String, &String)> = ns_map.iter().collect();
        pairs.sort_by_key(|(_, prefix)| *prefix);
        for (uri, prefix) in pairs {
            buf.push_str(&format!(" xmlns:{}=\"{}\"", prefix, escape_attr(uri)));
        }
    }

    // Serialize attributes
    for attr in &elem.attrs {
        let attr_qname = qualified_name(&attr.local_name, &attr.namespace_uri, ns_map);
        buf.push(' ');
        buf.push_str(&attr_qname);
        buf.push_str("=\"");
        buf.push_str(&escape_attr(&attr.value));
        buf.push('"');
    }

    if elem.children.is_empty() {
        buf.push_str("/>");
    } else {
        buf.push('>');
        for child in &elem.children {
            serialize_content(child, buf, ns_map, false);
        }
        buf.push_str("</");
        buf.push_str(&qname);
        buf.push('>');
    }
}

fn serialize_content_pretty(
    content: &XmlContent,
    buf: &mut String,
    ns_map: &HashMap<String, String>,
    declare_namespaces: bool,
    depth: usize,
) {
    match content {
        XmlContent::Text(t) => buf.push_str(&escape_text(t)),
        XmlContent::Element(elem) => {
            // Recursive calls never re-declare namespaces, so default_ns is None here.
            serialize_element_pretty(elem, buf, ns_map, declare_namespaces, depth, None)
        }
    }
}

fn serialize_element_pretty(
    elem: &XmlElement,
    buf: &mut String,
    ns_map: &HashMap<String, String>,
    declare_namespaces: bool,
    depth: usize,
    default_ns: Option<&str>,
) {
    let indent = "  ".repeat(depth);
    let qname = qualified_name(&elem.local_name, &elem.namespace_uri, ns_map);

    buf.push_str(&indent);
    buf.push('<');
    buf.push_str(&qname);

    if declare_namespaces {
        if let Some(dns) = default_ns {
            buf.push_str(&format!(" xmlns=\"{}\"", escape_attr(dns)));
        }
        let mut pairs: Vec<(&String, &String)> = ns_map.iter().collect();
        pairs.sort_by_key(|(_, prefix)| *prefix);
        for (uri, prefix) in pairs {
            buf.push_str(&format!(" xmlns:{}=\"{}\"", prefix, escape_attr(uri)));
        }
    }

    for attr in &elem.attrs {
        let attr_qname = qualified_name(&attr.local_name, &attr.namespace_uri, ns_map);
        buf.push(' ');
        buf.push_str(&attr_qname);
        buf.push_str("=\"");
        buf.push_str(&escape_attr(&attr.value));
        buf.push('"');
    }

    let has_element_children = elem
        .children
        .iter()
        .any(|c| matches!(c, XmlContent::Element(_)));

    if elem.children.is_empty() {
        buf.push_str("/>\n");
    } else if has_element_children {
        // Block mode: each child on its own indented line.
        // Text nodes that are pure whitespace are dropped; others get their own line.
        buf.push_str(">\n");
        let child_indent = "  ".repeat(depth + 1);
        for child in &elem.children {
            match child {
                XmlContent::Text(t) => {
                    let escaped = escape_text(t);
                    if !escaped.trim().is_empty() {
                        buf.push_str(&child_indent);
                        buf.push_str(&escaped);
                        buf.push('\n');
                    }
                }
                XmlContent::Element(_) => {
                    serialize_content_pretty(child, buf, ns_map, false, depth + 1);
                }
            }
        }
        buf.push_str(&indent);
        buf.push_str("</");
        buf.push_str(&qname);
        buf.push_str(">\n");
    } else {
        // Inline mode: text-only content stays on one line.
        buf.push('>');
        for child in &elem.children {
            if let XmlContent::Text(t) = child {
                buf.push_str(&escape_text(t));
            }
        }
        buf.push_str("</");
        buf.push_str(&qname);
        buf.push_str(">\n");
    }
}

fn escape_text(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(c),
        }
    }
    out
}

fn escape_attr(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}
