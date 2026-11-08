//! Instruction-count benchmarks, run under Callgrind by gungraun, for tracking performance in CI.
//!
//! Wall-clock timings on shared CI runners are too noisy to spot small regressions, whereas
//! instruction counts are nearly deterministic.  `benches/validator.rs` measures the same work
//! with criterion, for wall-clock timings locally.
//!
//! Running this needs Valgrind and a `gungraun-runner` binary matching the `gungraun` version in
//! `Cargo.lock`; see `.github/workflows/benchmark.yml`.

use gungraun::{library_benchmark, library_benchmark_group, main};
use relaxng::model::{Compiler, DefineRule, Files, RelaxError, Syntax};
use relaxng::validator::{Validator, validate_slice};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hint::black_box;
use std::path::Path;
use std::rc::Rc;

mod document;

use document::{SCHEMA, make_document};

type Schema = Rc<RefCell<Option<DefineRule>>>;

struct InMemoryFiles(HashMap<String, String>);

impl Files for InMemoryFiles {
    fn load(&self, path: &Path) -> Result<String, RelaxError> {
        self.0.get(path.to_str().unwrap()).cloned().ok_or_else(|| {
            RelaxError::Io(
                path.to_path_buf(),
                std::io::Error::from(std::io::ErrorKind::NotFound),
            )
        })
    }
}

fn compile_schema() -> Schema {
    let mut resources = HashMap::new();
    resources.insert("schema.rnc".to_string(), SCHEMA.to_string());
    let mut compiler = Compiler::new(InMemoryFiles(resources), Syntax::Compact);
    compiler
        .compile(Path::new("schema.rnc"))
        .expect("compile schema")
        .start
}

/// Setup for the `validate` benchmarks, which runs outside the measured region.
fn setup_validate(sections: usize) -> (Schema, String) {
    (compile_schema(), make_document(sections))
}

#[library_benchmark]
fn compile() -> Schema {
    black_box(compile_schema())
}

#[library_benchmark]
#[bench::sections_5(setup_validate(5))]
#[bench::sections_50(setup_validate(50))]
fn validate((schema, doc): (Schema, String)) {
    let mut v = Validator::new(schema, |_: &_, err| panic!("Validation error: {err:?}")).unwrap();
    validate_slice(&mut v, black_box(doc.as_bytes())).unwrap();
}

library_benchmark_group!(name = ci, benchmarks = [compile, validate]);

main!(library_benchmark_groups = ci);
