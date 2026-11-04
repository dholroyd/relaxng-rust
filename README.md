# relaxng-rust

[Rust](/home/dave/projects/relaxng-rust/README.md) implementation of the [RELAX NG](https://relaxng.org/) XML schema language.

## Example

```
start =
  element data { children }

children =
  element dog { text } |
  element sheep { text }
```

```xml
<data>
  <cats>henry</cats>
</data>
```

```
$ rng validate schema.rnc input.xml

Validating "input.xml"
error: element-start not expected here
 --> input.xml:2:4
  |
2 |   <cats>henry</cats>
  |    ^^^^ Not allowed
help: Expected Element sheep dog
```

## Status

The validator passes all 384 tests from the RELAX NG test suite.

All 44 XML Schema built-in datatypes are supported (except `NOTATION`, which the RelaxNG spec forbids).

Entities declared in a DTD are not supported: the DOCTYPE is skipped, and a reference to any entity other than the
predefined ones is reported as an error.  Attribute values are limited to 1 MiB.

## Crate structure

Everything is in the single `relaxng` crate.  The schema model is always built; the rest sits behind cargo
features, all enabled by default:

 - `relaxng::syntax` parses both the 'compact' (non-XML) syntax and the XML syntax into an 'abstract syntax tree'
   representation of an individual schema file.
 - `relaxng::model` (starting from `relaxng::model::Compiler`) provides an intermediate representation of a RELAX NG
   schema, which might be composed of multiple related schema component files.
 - `relaxng::validator` (feature `validator`) validates instance documents.
 - `relaxng::exemplifier` (feature `exemplifier`) generates random example XML documents from a schema, for testing.
 - the `rng` program (feature `tool`, see [TOOL.md](TOOL.md)) is a CLI to validate XML documents against a
   RELAX NG schema, lint schemas and report coverage.

A dependant that only needs the schema model can avoid compiling the rest:

```toml
[dependencies]
relaxng = { version = "0.1", default-features = false }
```
