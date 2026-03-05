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

The `relaxng-validator` crate passes all 384 tests from the RELAX NG test suite.

All 44 XML Schema built-in datatypes are supported (except `NOTATION`, which the RelaxNG spec forbids).


## Subprojects

 - [`relaxng-syntax`](relaxng-syntax) supports parsing both the 'compact' (non-XML) syntax and the XML syntax into
   an 'abstract sytax tree' representation of an individual schema file.
 - [`relaxng-model`](relaxng-model) provides an intermediate representation of an RELAX NG schema, which might be
   composed of multiple related schema component files.
 - [`relaxng-validator`](relaxng-validator) able to perform validation of instance documents.  Uses
   [xmlparser](https://crates.io/crates/xmlparser).
 - [`relaxng-tool`](relaxng-tool) - a demo CLI program to validate XML documents against a RELAX NG schema.
