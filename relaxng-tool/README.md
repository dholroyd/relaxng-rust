# relaxng-tool

A command-line tool for working with [RELAX NG](https://relaxng.org/) schemas.
Supports both compact (`.rnc`) and XML (`.rng`) syntax.

## Installation

```sh
cargo install --path relaxng-tool
```

This installs the `rng` binary.

## Subcommands

### `rng validate` — Validate XML against a schema

```
USAGE:
    rng validate <schema> [xml]...
```

Validates one or more XML documents against a RELAX NG schema. Exits with
code 0 on success, 1 if the schema fails to compile, or 2 if validation fails.

```sh
rng validate schema.rnc document.xml
```

When validation succeeds, only a progress line is printed:

```
Validating "document.xml"
```

When validation fails, diagnostic messages pinpoint the problem:

<pre style="background:#1e1e1e;color:#d4d4d4;padding:1em;overflow-x:auto;border-radius:6px;font-size:0.9em">
Validating "examples/bad.xml"
<span style="font-weight: bold; color: #ff0000">error</span><span style="font-weight: bold">: attribute not expected here</span>
 <span style="font-weight: bold; color: #5c5cff">--&gt; </span>examples/bad.xml:3:18
  <span style="font-weight: bold; color: #5c5cff">|</span>
<span style="font-weight: bold; color: #5c5cff">3</span> <span style="font-weight: bold; color: #5c5cff">| </span>  &lt;recipe id="1" difficulty="extreme"&gt;
  <span style="font-weight: bold; color: #5c5cff">| </span>                 <span style="font-weight: bold; color: #ff0000">^^^^^^^^^^^^^^^^^^^^</span> <span style="font-weight: bold; color: #ff0000">Not allowed</span>
<span style="font-weight: bold; color: #00ffff">help</span><span style="font-weight: bold">: Expected Element recipe</span>
</pre>

---

### `rng lint` — Check a schema for issues

```
USAGE:
    rng lint <schema>
```

Scans a compiled schema for potential problems:

- **Unreachable definitions** — named patterns that are never referenced
- **Dead choice branches** — `notAllowed` inside a `choice`
- **Dead composites** — `group` or `interleave` containing `notAllowed`
- **Redundant wrapping** — e.g. `(x?)?`, `(x+)*`

Exits with code 0 if no warnings are found, or 1 if there are warnings.

```sh
rng lint schema.rnc
```

<pre style="background:#1e1e1e;color:#d4d4d4;padding:1em;overflow-x:auto;border-radius:6px;font-size:0.9em">
<span style="font-weight: bold; color: #aa5500">warning</span><span style="font-weight: bold">: unreachable definition 'unused'</span>
  <span style="font-weight: bold; color: #5c5cff">--&gt; </span>schema.rnc:10:1
   <span style="font-weight: bold; color: #5c5cff">|</span>
<span style="font-weight: bold; color: #5c5cff">10</span> <span style="font-weight: bold; color: #5c5cff">| </span>unused = element phantom { text }
   <span style="font-weight: bold; color: #5c5cff">| </span><span style="font-weight: bold; color: #aa5500">^^^^^^</span> <span style="font-weight: bold; color: #aa5500">defined here</span>

1 warning(s) found.
</pre>

---

### `rng coverage` — Report schema coverage

```
USAGE:
    rng coverage [OPTIONS] <schema> [xml]...

OPTIONS:
    --format <format>    text or html [default: text]
```

Validates XML documents against a schema and reports which schema patterns
were exercised. When multiple XML files are given, their coverage is merged.

#### Text output (default)

Uncovered patterns are reported as warnings with source locations,
followed by a summary line:

```sh
rng coverage schema.rnc tests/*.xml
```

<pre style="background:#1e1e1e;color:#d4d4d4;padding:1em;overflow-x:auto;border-radius:6px;font-size:0.9em">
<span style="font-weight: bold; color: #aa5500">warning</span><span style="font-weight: bold">: Uncovered DatatypeValue: "cup"</span>
  <span style="font-weight: bold; color: #5c5cff">--&gt; </span>schema.rnc:20:69
   <span style="font-weight: bold; color: #5c5cff">|</span>
<span style="font-weight: bold; color: #5c5cff">20</span> <span style="font-weight: bold; color: #5c5cff">| </span>        attribute unit { "g" | "kg" | "ml" | "l" | "tsp" | "tbsp" | "cup" | "whole" },
   <span style="font-weight: bold; color: #5c5cff">| </span>                                                                    <span style="font-weight: bold; color: #aa5500">^^^^^</span>

<span style="font-weight: bold; color: #aa5500">warning</span><span style="font-weight: bold">: Uncovered Attribute: vegetarian</span>
  <span style="font-weight: bold; color: #5c5cff">--&gt; </span>schema.rnc:10:5
   <span style="font-weight: bold; color: #5c5cff">|</span>
<span style="font-weight: bold; color: #5c5cff">10</span> <span style="font-weight: bold; color: #5c5cff">| </span>    attribute vegetarian { xsd:boolean }?,
   <span style="font-weight: bold; color: #5c5cff">| </span>    <span style="font-weight: bold; color: #aa5500">^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^</span>

Coverage: 21/32 trackable patterns covered
</pre>

#### HTML output

Generate a standalone HTML report with a visual progress bar and pattern table:

```sh
rng coverage --format html schema.rnc tests/*.xml > coverage.html
```

