//! An implementation of the [RELAX NG](https://relaxng.org/) XML schema language.
//!
//! - [`syntax`] parses the compact and XML syntaxes of an individual schema file.
//! - [`model`] compiles schema files into a pattern graph, starting from [`model::Compiler`].
//! - `validator` (feature `validator`) validates XML documents against a compiled schema.
//! - `exemplifier` (feature `exemplifier`) generates random example documents from a schema.
//!
//! The `validator`, `exemplifier` and `tool` (the `rng` program) features are enabled by default.
//! Disable default features to build only [`syntax`] and [`model`].

#[cfg(feature = "exemplifier")]
pub mod exemplifier;
pub mod model;
pub mod syntax;
#[cfg(feature = "validator")]
pub mod validator;
