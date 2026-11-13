//! Compiles one or more schema files into a single graph of [`Pattern`]s, joined up through
//! named [`DefineRule`]s.
//!
//! [`Compiler`] is the entry point.

mod compiler;
mod pattern;
mod restrictions;

pub mod ambiguity;
pub mod datatype;
pub mod lint;

pub use compiler::{
    AnnotationNameError, CompileResult, Compiler, DefinitionInfo, Files, FsFiles, RelaxError,
    Syntax,
};
pub use pattern::{
    Annotation, AnnotationAttribute, AnnotationContent, AnnotationDocumentation, AnnotationElement,
    CombineRule, DefineRule, NameClass, Param, PatRef, Pattern,
};
pub use restrictions::{RestrictionKind, Violation};
