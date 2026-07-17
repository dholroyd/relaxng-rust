//! Random XML document generator for RelaxNG schemas.
//!
//! # Overview
//!
//! [`Generator`] takes a compiled RelaxNG schema and produces valid XML instance documents.
//! Two modes are supported:
//!
//! * **Reproducible generation** - [`Generator::generate_with_seed`] fills a byte buffer from a
//!   seeded PRNG and then drives [`arbitrary::Unstructured`]. Identical seeds produce identical
//!   documents.
//!
//! * **Fuzz-testing** - [`Generator::generate_from_bytes`] accepts raw bytes (e.g., from
//!   libFuzzer) directly as `Unstructured`. When the byte supply is exhausted the generator falls
//!   back to minimum-cost completions.
//!
//! * **Coverage tour** - [`Generator::coverage_tour`] returns a small set of documents that
//!   collectively exercise every `Choice` branch and every `Optional` path in the schema.

pub mod analysis;
pub mod coverage;
pub mod datatypes;
pub mod document;
pub mod names;

use analysis::Analysis;
use arbitrary::Unstructured;
use names::Vocabulary;
use rand::RngCore;
use rand::SeedableRng;
use relaxng_model::model::DefineRule;
use std::cell::RefCell;
use std::rc::Rc;

/// Generator for random XML instance documents conforming to a RelaxNG schema.
pub struct Generator {
    start: Rc<RefCell<Option<DefineRule>>>,
    analysis: Analysis,
    vocab: Vocabulary,
    default_fuel: usize,
    pretty: bool,
}

impl Generator {
    /// Build a generator from a compiled schema start rule.
    ///
    /// `start` is the value returned by [`relaxng_model::Compiler::compile`].
    /// `fuel` controls the approximate document size; larger values produce more deeply nested or
    /// repeated content.
    ///
    /// Output is pretty-printed by default; call `.pretty(false)` to get compact single-line output.
    pub fn new(start: Rc<RefCell<Option<DefineRule>>>, fuel: usize) -> Self {
        let (analysis, vocab) = {
            let borrow = start.borrow();
            if let Some(rule) = borrow.as_ref() {
                (
                    Analysis::new(rule.pattern()),
                    Vocabulary::from_pattern(rule.pattern()),
                )
            } else {
                (Analysis::default(), Vocabulary::default())
            }
        };
        Generator {
            start,
            analysis,
            vocab,
            default_fuel: fuel,
            pretty: true,
        }
    }

    /// Control whether generated documents are pretty-printed (default: `true`).
    pub fn pretty(mut self, pretty: bool) -> Self {
        self.pretty = pretty;
        self
    }

    /// Generate a document from arbitrary bytes.
    ///
    /// Suitable for use in `cargo-fuzz` harnesses: pass the fuzzer-supplied byte slice directly.
    /// When `bytes` are exhausted the generator falls back to minimum-cost completions, so this
    /// function always returns a document (it never panics due to insufficient randomness).
    pub fn generate_from_bytes(&self, bytes: &[u8]) -> String {
        let mut u = Unstructured::new(bytes);
        let borrow = self.start.borrow();
        if let Some(rule) = borrow.as_ref() {
            let output = document::generate_pattern(
                rule.pattern(),
                &mut u,
                self.default_fuel,
                &self.analysis,
                &self.vocab,
            );
            document::serialize_document(&output, self.pretty)
        } else {
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><error/>".to_string()
        }
    }

    /// Generate a document with a `u64` seed for reproducible output.
    ///
    /// Uses a fast PRNG to fill a byte buffer, then calls [`generate_from_bytes`].
    pub fn generate_with_seed(&self, seed: u64) -> String {
        let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);
        // Fill a buffer; size proportional to fuel for adequate randomness
        let buf_size = (self.default_fuel * 32).max(256);
        let mut buf = vec![0u8; buf_size];
        rng.fill_bytes(&mut buf);
        self.generate_from_bytes(&buf)
    }

    /// Return a minimal set of documents that together cover every `Choice` branch and
    /// every `Optional` path in the schema.
    ///
    /// The returned documents are deterministic (no randomness involved) and each one
    /// validates against the schema.
    pub fn coverage_tour(&self) -> Vec<String> {
        coverage::coverage_tour(
            &self.start,
            &self.analysis,
            &self.vocab,
            self.default_fuel,
            self.pretty,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_schema(rnc: &str) -> Rc<RefCell<Option<DefineRule>>> {
        use relaxng_model::{Compiler, FsFiles, Syntax};
        let dir = tempfile::tempdir().expect("tmpdir");
        let path = dir.path().join("schema.rnc");
        std::fs::write(&path, rnc).expect("write schema");
        let mut compiler = Compiler::new(FsFiles, Syntax::Compact);
        compiler.compile(&path).expect("compile schema").start
    }

    #[test]
    fn test_simple_element() {
        let start = compile_schema("start = element foo { empty }");
        let generator = Generator::new(start, 10);
        let doc = generator.generate_with_seed(42);
        assert!(doc.contains("<foo"), "doc: {}", doc);
    }

    #[test]
    fn test_reproducible() {
        let start = compile_schema("start = element root { element child { text } }");
        let generator = Generator::new(start, 20);
        let doc1 = generator.generate_with_seed(99);
        let doc2 = generator.generate_with_seed(99);
        assert_eq!(doc1, doc2, "same seed must produce same output");
    }

    #[test]
    fn test_different_seeds() {
        let start =
            compile_schema("start = element root { element a { empty } | element b { empty } }");
        let generator = Generator::new(start, 10);
        let docs: std::collections::HashSet<String> = (0u64..32)
            .map(|s| generator.generate_with_seed(s))
            .collect();
        assert!(
            docs.len() > 1,
            "expected variety across seeds, got: {:?}",
            docs
        );
    }

    #[test]
    fn test_from_bytes_empty() {
        let start = compile_schema("start = element x { empty }");
        let generator = Generator::new(start, 10);
        let doc = generator.generate_from_bytes(&[]);
        assert!(doc.contains("<x"), "doc: {}", doc);
    }

    #[test]
    fn test_coverage_tour_nonempty() {
        let start = compile_schema(
            "start = element root { element a { empty } | element b { empty } | element c { empty } }",
        );
        let generator = Generator::new(start, 10);
        let tour = generator.coverage_tour();
        assert!(!tour.is_empty());
    }

    #[test]
    fn test_attribute_generation() {
        let start = compile_schema("start = element e { attribute id { text }, empty }");
        let generator = Generator::new(start, 10);
        let doc = generator.generate_with_seed(1);
        assert!(doc.contains("id="), "expected attribute, got: {}", doc);
    }

    #[test]
    fn test_optional_generation() {
        let start = compile_schema("start = element root { element opt { empty }? }");
        let generator = Generator::new(start, 10);
        for seed in 0..8 {
            let _ = generator.generate_with_seed(seed);
        }
    }

    #[test]
    fn test_zero_or_more() {
        let start = compile_schema("start = element list { element item { text }* }");
        let generator = Generator::new(start, 20);
        let doc = generator.generate_with_seed(7);
        assert!(doc.contains("<list"), "doc: {}", doc);
    }
}
