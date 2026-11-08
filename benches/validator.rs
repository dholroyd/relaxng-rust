use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use relaxng::model::{Compiler, Files, Syntax};
use relaxng::validator::{Validator, validate_slice};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

mod document;

use document::{SCHEMA, make_document};

struct InMemoryFiles(HashMap<String, String>);

impl Files for InMemoryFiles {
    fn load(&self, path: &Path) -> Result<String, relaxng::model::RelaxError> {
        self.0.get(path.to_str().unwrap()).cloned().ok_or_else(|| {
            relaxng::model::RelaxError::Io(
                path.to_path_buf(),
                std::io::Error::from(std::io::ErrorKind::NotFound),
            )
        })
    }
}

fn compile_schema() -> Rc<RefCell<Option<relaxng::model::DefineRule>>> {
    let mut resources = HashMap::new();
    resources.insert("schema.rnc".to_string(), SCHEMA.to_string());
    let mut compiler = Compiler::new(InMemoryFiles(resources), Syntax::Compact);
    compiler
        .compile(Path::new("schema.rnc"))
        .expect("compile schema")
        .start
}

fn validate(schema: Rc<RefCell<Option<relaxng::model::DefineRule>>>, doc: &str) {
    let mut v = Validator::new(schema, |_: &_, err| panic!("Validation error: {err:?}")).unwrap();
    validate_slice(&mut v, doc.as_bytes()).unwrap();
}

fn bench_compile(c: &mut Criterion) {
    c.bench_function("compile_schema", |b| {
        b.iter(compile_schema);
    });
}

fn bench_validate(c: &mut Criterion) {
    let schema = compile_schema();

    let mut group = c.benchmark_group("validate");
    for sections in [5, 20, 50] {
        let doc = make_document(sections);
        group.bench_with_input(BenchmarkId::from_parameter(sections), &doc, |b, doc| {
            b.iter(|| validate(schema.clone(), doc));
        });
    }
    group.finish();
}

criterion_group!(benches, bench_compile, bench_validate);
criterion_main!(benches);
