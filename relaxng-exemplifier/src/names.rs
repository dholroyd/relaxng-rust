use arbitrary::Unstructured;
use relaxng_model::model::{NameClass, Pattern};
use std::collections::HashSet;

/// Names explicitly declared in the schema, used to bias wildcard name
/// resolution toward schema-native vocabulary.
///
/// Collected once at [`crate::Generator`] construction time by walking the
/// entire pattern tree.
#[derive(Debug, Default, Clone)]
pub struct Vocabulary {
    /// `(local_name, namespace_uri)` pairs from `Named` name classes in element position.
    pub element_names: Vec<(String, String)>,
    /// `(local_name, namespace_uri)` pairs from `Named` name classes in attribute position.
    pub attr_names: Vec<(String, String)>,
}

impl Vocabulary {
    /// Walk `pattern` and collect all explicitly-named element and attribute names.
    pub fn from_pattern(pattern: &Pattern) -> Self {
        let mut vocab = Vocabulary::default();
        collect_vocab(pattern, &mut vocab, &mut HashSet::new());
        vocab.element_names.sort();
        vocab.element_names.dedup();
        vocab.attr_names.sort();
        vocab.attr_names.dedup();
        vocab
    }
}

fn collect_vocab(pattern: &Pattern, vocab: &mut Vocabulary, visiting: &mut HashSet<usize>) {
    match pattern {
        Pattern::Element(nc, p, _, _) => {
            if let NameClass::Named {
                name,
                namespace_uri,
            } = nc
            {
                vocab
                    .element_names
                    .push((name.clone(), namespace_uri.clone()));
            }
            collect_vocab(p, vocab, visiting);
        }
        Pattern::Attribute(nc, p, _, _) => {
            if let NameClass::Named {
                name,
                namespace_uri,
            } = nc
            {
                vocab.attr_names.push((name.clone(), namespace_uri.clone()));
            }
            collect_vocab(p, vocab, visiting);
        }
        Pattern::Choice(branches, _) => {
            for b in branches {
                collect_vocab(b, vocab, visiting);
            }
        }
        Pattern::Group(pats, _) | Pattern::Interleave(pats, _) => {
            for p in pats {
                collect_vocab(p, vocab, visiting);
            }
        }
        Pattern::ZeroOrMore(p, _)
        | Pattern::OneOrMore(p, _)
        | Pattern::Optional(p, _)
        | Pattern::Mixed(p, _)
        | Pattern::List(p, _) => collect_vocab(p, vocab, visiting),
        Pattern::Ref(_, _, pat_ref) => {
            let ptr = pat_ref.0.as_ptr() as usize;
            if visiting.contains(&ptr) {
                return;
            }
            visiting.insert(ptr);
            let borrow = pat_ref.0.borrow();
            if let Some(rule) = borrow.as_ref() {
                collect_vocab(rule.pattern(), vocab, visiting);
            }
            drop(borrow);
            visiting.remove(&ptr);
        }
        Pattern::DatatypeName {
            except: Some(e), ..
        } => collect_vocab(e, vocab, visiting),
        Pattern::Empty(_)
        | Pattern::Text(_)
        | Pattern::NotAllowed(_)
        | Pattern::DatatypeValue { .. }
        | Pattern::DatatypeName { except: None, .. } => {}
    }
}

/// Whether a name is being resolved in element or attribute position.
/// Controls which slice of the [`Vocabulary`] is consulted.
#[derive(Copy, Clone)]
pub enum NameContext {
    Element,
    Attribute,
}

fn vocab_pool<'a>(vocab: &'a Vocabulary, ctx: NameContext) -> &'a [(String, String)] {
    match ctx {
        NameContext::Element => &vocab.element_names,
        NameContext::Attribute => &vocab.attr_names,
    }
}

/// Fallback pool used only when the schema declares no named elements/attributes.
const ANY_NAME_POOL: &[&str] = &["a", "b", "item", "value", "node", "data", "elem", "x"];

/// Resolve a `NameClass` to a concrete `(local_name, namespace_uri)` pair.
///
/// For `Named` classes the result is fixed.  For `AnyName` and `NsName` the
/// resolver first tries to pick from the set of names explicitly declared in
/// the schema (`vocab`), falling back to a small generic pool only when the
/// schema itself declares no names of the appropriate kind.
pub fn pick_name(
    nc: &NameClass,
    u: &mut Unstructured,
    vocab: &Vocabulary,
    ctx: NameContext,
) -> Result<(String, String), arbitrary::Error> {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => Ok((name.clone(), namespace_uri.clone())),

        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            let candidates: Vec<(String, String)> = vocab_pool(vocab, ctx)
                .iter()
                .filter(|(_, ns)| ns == namespace_uri)
                .filter(|(n, ns)| {
                    except
                        .as_ref()
                        .map_or(true, |e| !name_class_matches(e, n, ns))
                })
                .cloned()
                .collect();

            if !candidates.is_empty() {
                if let Ok(chosen) = u.choose(&candidates) {
                    return Ok(chosen.clone());
                }
            }
            // Fallback: generate a random NCName in this namespace.
            let candidate = gen_ncname(u)?;
            if let Some(exc) = except {
                if name_class_matches(exc, &candidate, namespace_uri) {
                    return Ok((format!("{}_gen", candidate), namespace_uri.clone()));
                }
            }
            Ok((candidate, namespace_uri.clone()))
        }

        NameClass::AnyName { except } => {
            let candidates: Vec<(String, String)> = vocab_pool(vocab, ctx)
                .iter()
                .filter(|(n, ns)| {
                    except
                        .as_ref()
                        .map_or(true, |e| !name_class_matches(e, n, ns))
                })
                .cloned()
                .collect();

            if !candidates.is_empty() {
                if let Ok(chosen) = u.choose(&candidates) {
                    return Ok(chosen.clone());
                }
            }
            // Fallback: generic pool when the schema has no named elements/attributes.
            let name = if let Ok(n) = u.choose(ANY_NAME_POOL) {
                n.to_string()
            } else {
                "elem".to_string()
            };
            if let Some(exc) = except {
                if name_class_matches(exc, &name, "") {
                    return Ok((format!("{}_gen", name), String::new()));
                }
            }
            Ok((name, String::new()))
        }

        NameClass::Alt { a, b } => {
            let choice = u.arbitrary::<bool>().unwrap_or(false);
            if choice {
                pick_name(a, u, vocab, ctx)
            } else {
                pick_name(b, u, vocab, ctx)
            }
        }
    }
}

/// Generate a short valid XML NCName.
pub(crate) fn gen_ncname(u: &mut Unstructured) -> Result<String, arbitrary::Error> {
    const STARTS: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    const CONTINUES: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-";

    let start_byte = u.choose(STARTS).copied().unwrap_or(b'a');
    let len = u.int_in_range(0usize..=4).unwrap_or(0);
    let mut name = String::new();
    name.push(start_byte as char);
    for _ in 0..len {
        let c = u.choose(CONTINUES).copied().unwrap_or(b'a');
        name.push(c as char);
    }
    Ok(name)
}

/// Check if a name matches a NameClass (used for `except` handling).
fn name_class_matches(nc: &NameClass, local: &str, ns: &str) -> bool {
    match nc {
        NameClass::Named {
            namespace_uri,
            name,
        } => name == local && namespace_uri == ns,
        NameClass::NsName {
            namespace_uri,
            except,
        } => {
            ns == namespace_uri
                && except
                    .as_ref()
                    .map(|e| !name_class_matches(e, local, ns))
                    .unwrap_or(true)
        }
        NameClass::AnyName { except } => except
            .as_ref()
            .map(|e| !name_class_matches(e, local, ns))
            .unwrap_or(true),
        NameClass::Alt { a, b } => {
            name_class_matches(a, local, ns) || name_class_matches(b, local, ns)
        }
    }
}
