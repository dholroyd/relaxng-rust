use relaxng_exemplifier::Generator;
use relaxng_model::model::DefineRule;
use relaxng_model::{Compiler, FsFiles, Syntax};
use relaxng_validator::Validator;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

fn compile(rnc: &str) -> Rc<RefCell<Option<DefineRule>>> {
    let dir = tempfile::tempdir().expect("tmpdir");
    let path = dir.path().join("schema.rnc");
    std::fs::write(&path, rnc).expect("write schema");
    let mut compiler = Compiler::new(FsFiles, Syntax::Compact);
    compiler.compile(&path).expect("compile schema").start
}

fn assert_valid(model: Rc<RefCell<Option<DefineRule>>>, doc: &str) {
    let reader = xmlparser::Tokenizer::from(doc);
    let mut v = Validator::new(model, reader).expect("compile validator");
    loop {
        match v.validate_next() {
            Some(Ok(())) => {}
            Some(Err(e)) => panic!("validation error: {e:?}\ndoc:\n{doc}"),
            None => break,
        }
    }
}

// Schemas used across multiple tests.
const SIMPLE: &str = "start = element data { element dog { text } | element sheep { text } }";

const WITH_ATTRS: &str = "start = element catalog {
    attribute version { text },
    element item {
        attribute id { text },
        attribute type { text },
        element name { text },
        element description { text }?
    }+
}";

const WITH_REPETITION: &str = "start = element list {
    element header { text },
    element item { attribute n { text }, text }*
}";

const INTERLEAVED: &str = "start = element root {
    element a { text } & element b { text } & element c { text }?
}";

const NESTED_CHOICE: &str = "start = element top {
    (element x { empty } | element y { element z { text } })
}";

#[test]
fn seeded_simple() {
    let model = compile(SIMPLE);
    let generator = Generator::new(model.clone(), 20);
    for seed in 0..32 {
        let doc = generator.generate_with_seed(seed);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn seeded_with_attrs() {
    let model = compile(WITH_ATTRS);
    let generator = Generator::new(model.clone(), 20);
    for seed in 0..32 {
        let doc = generator.generate_with_seed(seed);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn seeded_with_repetition() {
    let model = compile(WITH_REPETITION);
    let generator = Generator::new(model.clone(), 30);
    for seed in 0..32 {
        let doc = generator.generate_with_seed(seed);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn seeded_interleaved() {
    let model = compile(INTERLEAVED);
    let generator = Generator::new(model.clone(), 20);
    for seed in 0..32 {
        let doc = generator.generate_with_seed(seed);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn seeded_nested_choice() {
    let model = compile(NESTED_CHOICE);
    let generator = Generator::new(model.clone(), 20);
    for seed in 0..32 {
        let doc = generator.generate_with_seed(seed);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn coverage_simple() {
    let model = compile(SIMPLE);
    let generator = Generator::new(model.clone(), 20);
    let docs = generator.coverage_tour();
    assert!(!docs.is_empty());
    for doc in &docs {
        assert_valid(model.clone(), doc);
    }
}

#[test]
fn coverage_with_attrs() {
    let model = compile(WITH_ATTRS);
    let generator = Generator::new(model.clone(), 20);
    let docs = generator.coverage_tour();
    assert!(!docs.is_empty());
    for doc in &docs {
        assert_valid(model.clone(), doc);
    }
}

#[test]
fn coverage_with_repetition() {
    let model = compile(WITH_REPETITION);
    let generator = Generator::new(model.clone(), 30);
    let docs = generator.coverage_tour();
    assert!(!docs.is_empty());
    for doc in &docs {
        assert_valid(model.clone(), doc);
    }
}

#[test]
fn coverage_interleaved() {
    let model = compile(INTERLEAVED);
    let generator = Generator::new(model.clone(), 20);
    let docs = generator.coverage_tour();
    assert!(!docs.is_empty());
    for doc in &docs {
        assert_valid(model.clone(), doc);
    }
}

#[test]
fn coverage_nested_choice() {
    let model = compile(NESTED_CHOICE);
    let generator = Generator::new(model.clone(), 20);
    let docs = generator.coverage_tour();
    assert!(!docs.is_empty());
    for doc in &docs {
        assert_valid(model.clone(), doc);
    }
}

/// Parse `doc` and return every `(local_name, prefix)` pair that appears more
/// than once on the same element.
fn duplicate_attrs(doc: &str) -> Vec<(String, String)> {
    let mut duplicates = Vec::new();
    let mut stack: Vec<HashSet<(String, String)>> = Vec::new();
    for token in xmlparser::Tokenizer::from(doc) {
        match token.unwrap() {
            xmlparser::Token::ElementStart { .. } => {
                stack.push(HashSet::new());
            }
            xmlparser::Token::Attribute { prefix, local, .. } => {
                let key = (local.as_str().to_string(), prefix.as_str().to_string());
                if let Some(seen) = stack.last_mut() {
                    if !seen.insert(key.clone()) {
                        duplicates.push(key);
                    }
                }
            }
            xmlparser::Token::ElementEnd { .. } => {
                stack.pop();
            }
            _ => {}
        }
    }
    duplicates
}

fn assert_no_duplicate_attrs(doc: &str) {
    let dups = duplicate_attrs(doc);
    assert!(
        dups.is_empty(),
        "duplicate attributes {:?} in:\n{}",
        dups,
        doc
    );
}

// A schema where ZeroOrMore wraps a named attribute - the most direct way to
// trigger duplicates before the fix.
const REPEATED_ATTR: &str =
    "start = element root { attribute tag { text }*, element body { empty } }";

#[test]
fn no_duplicate_attrs_repeated_attr() {
    let model = compile(REPEATED_ATTR);
    let generator = Generator::new(model.clone(), 20);
    for seed in 0..64 {
        let doc = generator.generate_with_seed(seed);
        assert_no_duplicate_attrs(&doc);
        assert_valid(model.clone(), &doc);
    }
}

#[test]
fn no_duplicate_attrs_coverage_repeated() {
    let model = compile(REPEATED_ATTR);
    let generator = Generator::new(model.clone(), 20);
    for doc in generator.coverage_tour() {
        assert_no_duplicate_attrs(&doc);
        assert_valid(model.clone(), &doc);
    }
}
