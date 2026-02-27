use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use relaxng_model::{Compiler, Files, Syntax};
use relaxng_validator::Validator;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use xmlparser::Tokenizer;

const SCHEMA: &str = r#"
start = element catalog {
    attribute version { text },
    element item {
        (attribute id { text } | attribute ref { text }) &
        (attribute id { text } | attribute type { text }) &
        (attribute id { text } | attribute category { text }),
        element name { text },
        element description { text }?,
        (element tag { text }
         | element property {
             attribute key { text },
             text
           })*
    }+
}
"#;

/// Generate a document with `items` catalog entries.
fn make_document(items: usize) -> String {
    let mut doc = String::from(r#"<catalog version="1.0">"#);
    for i in 0..items {
        let attrs = if i % 2 == 0 {
            format!(
                r#" id="item-{i}" type="kind-{}" category="cat-{}""#,
                i % 5,
                i % 3
            )
        } else {
            format!(
                r#" ref="ref-{i}" type="kind-{}" category="cat-{}""#,
                i % 5,
                i % 3
            )
        };
        doc.push_str(&format!(r#"<item{attrs}><name>Item {i}</name>"#));
        if i % 3 == 0 {
            doc.push_str(&format!(
                "<description>Description for item {i}</description>"
            ));
        }
        for t in 0..(i % 4) {
            doc.push_str(&format!("<tag>tag-{t}</tag>"));
        }
        for p in 0..(i % 3) {
            doc.push_str(&format!(r#"<property key="prop-{p}">value-{p}</property>"#));
        }
        doc.push_str("</item>");
    }
    doc.push_str("</catalog>");
    doc
}

struct InMemoryFiles(HashMap<String, String>);

impl Files for InMemoryFiles {
    fn load(&self, path: &Path) -> Result<String, relaxng_model::RelaxError> {
        self.0.get(path.to_str().unwrap()).cloned().ok_or_else(|| {
            relaxng_model::RelaxError::Io(
                path.to_path_buf(),
                std::io::Error::from(std::io::ErrorKind::NotFound),
            )
        })
    }
}

fn compile_schema() -> Rc<RefCell<Option<relaxng_model::model::DefineRule>>> {
    let mut resources = HashMap::new();
    resources.insert("schema.rnc".to_string(), SCHEMA.to_string());
    let mut compiler = Compiler::new(InMemoryFiles(resources), Syntax::Compact);
    compiler
        .compile(Path::new("schema.rnc"))
        .expect("compile schema")
}

fn validate(schema: Rc<RefCell<Option<relaxng_model::model::DefineRule>>>, doc: &str) {
    let tokenizer = Tokenizer::from(doc);
    let mut v = Validator::new(schema, tokenizer);
    loop {
        match v.validate_next() {
            None => break,
            Some(Ok(())) => {}
            Some(Err(e)) => panic!("Validation error: {e:?}"),
        }
    }
}

fn bench_compile(c: &mut Criterion) {
    c.bench_function("compile_schema", |b| {
        b.iter(compile_schema);
    });
}

fn bench_validate(c: &mut Criterion) {
    let schema = compile_schema();

    let mut group = c.benchmark_group("validate");
    for items in [10, 50, 100] {
        let doc = make_document(items);
        group.bench_with_input(BenchmarkId::from_parameter(items), &doc, |b, doc| {
            b.iter(|| validate(schema.clone(), doc));
        });
    }
    group.finish();
}

criterion_group!(benches, bench_compile, bench_validate);
criterion_main!(benches);
