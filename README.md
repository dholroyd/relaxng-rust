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

The as of Feburary 2021, `relaxng-validator` crate passes 259 and fails 125 tests from the RELAX NG test suite.

Current problems,

 - ❌ Many restrictions required by the spec to reject invalid schema definitions are not yet implemented.  (Not so
   much of a problem if you already have a schema that works with other tools, but do not develop a schema against this
   tool.)
 - ❌ RELAX NG reuses the XML Schema datatype definitions but here, few datatypes and their facets are currently
   implemented.  Hitting an unsupported case will currently panic, rather than accepting an invalid input, (These
   panics will go away once the full datatype library is implemented.)
 - ❌ The implementation is currently very slow (compared to the [Jing](https://github.com/relaxng/jing-trang)
   implementation of RELAX NG validation, for example)

## Subprojects

 - [`relaxng-syntax`](relaxng-syntax) supports parsing both the 'compact' (non-XML) syntax and the XML syntax into
   an 'abstract sytax tree' representation of an individual schema file.
 - [`relaxng-model`](relaxng-model) provides an intermediate representation of an RELAX NG schema, which might be
   composed of multiple related schema component files.
 - [`relaxng-validator`](relaxng-validator) able to perform validation of instance documents.  Uses
   [xmlparser](https://crates.io/crates/xmlparser).
 - [`relaxng-tool`](relaxng-tool) - a demo CLI program to validate XML documents against a RELAX NG schema.