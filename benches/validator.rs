use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use relaxng::model::{Compiler, Files, Syntax};
use relaxng::validator::{Validator, validate_slice};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

/// A schema exercising most features, shaped like real document schemas such as DocBook and TEI:
/// - Namespaces (default + prefixed), including the predeclared `xml` prefix
/// - Grammar with named definitions and recursion (`section`, inline elements, `tree-node`,
///   `foreign`)
/// - All occurrence operators: optional(?), zeroOrMore(*), oneOrMore(+)
/// - All composition: group(,), choice(|), interleave(&)
/// - Interleave of elements (`metadata`) and of attributes (`common-attrs`, as in DocBook's
///   common attributes or TEI's `att.global`)
/// - Text content mostly written as `(text | a | b)*` as TEI and DocBook do, with a wide choice
///   of inline elements (see [`EXTRA_INLINE_ELEMENTS`]); `heading` alone uses `mixed { … }`, so
///   that its faster path is covered too
/// - A content model whose first part is optional (`p`'s leading `title`)
/// - Overlapping content models, which leave the validator tracking several alternatives at once:
///   two `figure` definitions that both match the same element, and two `p` definitions, where a
///   `p` after the content blocks may be either the last block or the section's closing
///   paragraph (as HTML5's schema defines `a` once per content category)
/// - XSD datatypes with facets (integer ranges, string lengths, patterns, decimals, dates,
///   booleans)
/// - Datatype values (enumerated string values), and a choice of a datatype and a value
///   (`width`)
/// - List pattern (whitespace-separated tokens)
/// - NameClass: named, nsName (`app:*`), anyName (`*`), except, and choices of name classes
///
/// The placeholder `| EXTRA_INLINES` is replaced by [`schema()`].
const SCHEMA_TEMPLATE: &str = r#"
namespace app = "http://example.com/app"
default namespace bench = "http://example.com/ns"

start = element document {
    attribute version { "1.0" | "2.0" },
    attribute app:generator { text }?,
    metadata,
    body
}

# Attributes most elements accept, interleaved as in DocBook and TEI
common-attrs =
    attribute xml:id { xsd:ID }?
  & attribute xml:lang { xsd:language }?
  & attribute role { xsd:token }?

# Children in any order
metadata = element metadata {
    element title { xsd:token { minLength = "1" maxLength = "200" } }
  & element created { xsd:date }
  & element modified { xsd:dateTime }?
  & element revision { xsd:positiveInteger }?
  & element score { xsd:decimal { fractionDigits = "2" } }?
  & element tags { list { xsd:token+ } }?
  & element author {
        attribute email { xsd:string { pattern = ".+@.+\..+" } },
        attribute active { xsd:boolean },
        text
    }+
}

body = element body {
    section+
}

# A `p` after the content blocks may be either the last block or the closing paragraph
section = element section {
    attribute id { xsd:ID },
    attribute priority { xsd:integer { minInclusive = "1" maxInclusive = "10" } }?,
    element heading { mixed { element em { text }* } },
    content-block*,
    closing-paragraph?,
    section*
}

content-block =
    paragraph | table | code-block | figure | data-list | extension-point | tree-node

paragraph = element p {
    common-attrs,
    element title { (text | inline-element)* }?,
    (text | inline-element | element br { empty })*
}

# A second definition of `p`, overlapping `paragraph`
closing-paragraph = element p {
    attribute kind { "summary" }?,
    (text | inline-element)*
}

inline-element =
    element strong { common-attrs, (text | inline-element)* }
  | element em { common-attrs, (text | inline-element)* }
  | element code { text }
  | element a {
        attribute href { xsd:anyURI },
        attribute title { text }?,
        (text | inline-element)*
    }
  | element span {
        attribute class { xsd:token },
        common-attrs,
        (text | inline-element)*
    }
  | EXTRA_INLINES

table = element table {
    common-attrs,
    attribute border { xsd:nonNegativeInteger }?,
    element caption { text }?,
    element thead {
        table-row+
    },
    element tbody {
        table-row+
    }
}

table-row = element tr {
    (element th {
        attribute colspan { xsd:positiveInteger }?,
        attribute rowspan { xsd:positiveInteger }?,
        (text | inline-element)*
    } | element td {
        attribute colspan { xsd:positiveInteger }?,
        attribute rowspan { xsd:positiveInteger }?,
        (text | inline-element)*
    })+
}

code-block = element pre {
    common-attrs,
    attribute language { "rust" | "python" | "javascript" | "xml" | "json" | "sql" },
    attribute line-numbers { xsd:boolean }?,
    text
}

# Two overlapping definitions: a figure with one image and a caption matches both, up to its
# end tag
figure =
    element figure {
        common-attrs,
        attribute float { "left" | "right" | "none" }?,
        image,
        figcaption?
    }
  | element figure {
        common-attrs,
        attribute float { "none" }?,
        image+,
        figcaption,
        element credit { text }?
    }

image = element img {
    attribute src { xsd:anyURI },
    attribute alt { text },
    attribute width { xsd:nonNegativeInteger | "auto" }?,
    attribute height { xsd:nonNegativeInteger | "auto" }?,
    empty
}

figcaption = element figcaption { (text | inline-element)* }

data-list = element dl {
    common-attrs,
    (element dt { (text | inline-element)* },
     element dd { (text | inline-element)* })+
}

# Recursive tree structure exercising Ref patterns
tree-node = element node {
    attribute name { xsd:NCName },
    attribute value { text }?,
    tree-node*
}

# Wildcard / nsName patterns
extension-point = element app:extensions {
    attribute app:* { text }*,
    (element app:* - app:extensions { text } | foreign)*
}

# Elements from any other namespace, with any attributes
foreign = element * - (bench:* | app:*) {
    attribute * { text }*,
    (text | foreign)*
}
"#;

/// How many `inline1` … `inlineN` elements to add to the `inline-element` choice, on top of the
/// five named ones.  Real document schemas offer a wide choice of inline elements in text
/// content (DocBook around a hundred, TEI a few hundred), and the cost of deriving a text node
/// against such content grows with the width of that choice.
const EXTRA_INLINE_ELEMENTS: usize = 40;

fn schema() -> String {
    let extra = (1..=EXTRA_INLINE_ELEMENTS)
        .map(|i| format!("element inline{i} {{ common-attrs, (text | inline-element)* }}"))
        .collect::<Vec<_>>()
        .join("\n  | ");
    SCHEMA_TEMPLATE.replace("EXTRA_INLINES", &extra)
}

/// Generate XML documents that exercise all schema features.
///
/// Block-level structure is indented, as in most real documents, so element-only content
/// contains whitespace text nodes.  Text content alternates text and inline elements, so that
/// text nodes regularly follow child elements, and includes entity and character references
/// and CDATA sections.
fn make_document(sections: usize) -> String {
    let mut doc = String::new();
    doc.push_str(r#"<document xmlns="http://example.com/ns" xmlns:app="http://example.com/app" xmlns:o="http://example.com/other" version="1.0" app:generator="bench">"#);

    // metadata block - exercises interleave (children out of declaration order), datatypes,
    // list, boolean, date, decimal, pattern facet
    line(&mut doc, 1, r#"<metadata>"#);
    line(
        &mut doc,
        2,
        r#"<author email="alice@example.com" active="true">Alice Smith</author>"#,
    );
    line(
        &mut doc,
        2,
        r#"<title>Benchmark Document With Many Features</title>"#,
    );
    line(
        &mut doc,
        2,
        r#"<tags>benchmark performance testing validation</tags>"#,
    );
    line(&mut doc, 2, r#"<created>2025-06-15</created>"#);
    line(
        &mut doc,
        2,
        r#"<author email="bob@test.org" active="false">Bob Jones</author>"#,
    );
    line(&mut doc, 2, r#"<score>98.75</score>"#);
    line(&mut doc, 2, r#"<modified>2025-06-15T10:30:00</modified>"#);
    line(&mut doc, 2, r#"<revision>42</revision>"#);
    line(&mut doc, 1, r#"</metadata>"#);

    // body with sections
    line(&mut doc, 1, r#"<body>"#);

    for s in 0..sections {
        write_section(&mut doc, s, 0, 2);
    }

    line(&mut doc, 1, r#"</body>"#);
    line(&mut doc, 0, r#"</document>"#);
    doc.push('\n');
    doc
}

/// Starts a new line at the given indentation level and appends `s`.
fn line(doc: &mut String, indent: usize, s: &str) {
    doc.push('\n');
    for _ in 0..indent {
        doc.push_str("  ");
    }
    doc.push_str(s);
}

/// The name of one of the generated `inlineN` elements, chosen by `n`.
fn extra_inline(n: usize) -> String {
    format!("inline{}", n % EXTRA_INLINE_ELEMENTS + 1)
}

fn write_section(doc: &mut String, idx: usize, depth: usize, indent: usize) {
    let priority = (idx % 10) + 1;
    let id = format!("s{depth}_{idx}");
    line(
        doc,
        indent,
        &format!(r#"<section id="{id}" priority="{priority}">"#),
    );
    let indent = indent + 1;

    // heading with mixed content (em elements) - the one `mixed { … }` pattern
    line(
        doc,
        indent,
        &format!(r#"<heading>Section {idx} with <em>emphasis</em> text</heading>"#),
    );

    // Vary content blocks by index to exercise all branches
    let block_type = idx % 7;

    // Always include a paragraph - exercises text content + inline elements, alternating text
    // with elements many times over, including a few of the generated inline elements, common
    // attributes, and entity and character references
    let (x1, x2, x3) = (
        extra_inline(idx),
        extra_inline(idx + 13),
        extra_inline(idx + 27),
    );
    line(
        doc,
        indent,
        &format!(r#"<p xml:id="{id}_p" role="intro">This is a paragraph with "#),
    );
    doc.push_str(r#"<strong role="key">bold <em xml:lang="fr">and italic</em> text</strong> "#);
    doc.push_str(r#"and <code>a &lt; b &amp;&amp; c &gt; d</code> "#);
    doc.push_str(
        r#"and <a href="http://example.com?a=1&amp;b=2" title="Example &amp; more">a link</a> "#,
    );
    doc.push_str(
        r#"and <span class="highlight">highlighted <em>text</em> here</span> &#x2014; &#169; "#,
    );
    doc.push_str(&format!(r#"then <{x1}>a term</{x1}> "#));
    doc.push_str(&format!(
        r#"next to <{x2} xml:lang="en">a <{x3}>nested</{x3}> phrase</{x2}> "#
    ));
    doc.push_str(&format!(r#"and <{x3}>another</{x3}>"#));
    doc.push_str(r#"<br/>"#);
    doc.push_str(r#"with a line break &amp; a &quot;quote&quot;.</p>"#);

    if block_type <= 1 || block_type == 6 {
        // table - exercises choice of th/td, colspan/rowspan, caption
        line(doc, indent, r#"<table border="1">"#);
        line(doc, indent + 1, r#"<caption>Data Table</caption>"#);
        line(doc, indent + 1, r#"<thead>"#);
        line(doc, indent + 2, r#"<tr>"#);
        line(doc, indent + 3, r#"<th colspan="2">Header A</th>"#);
        line(doc, indent + 3, r#"<th>Header B</th>"#);
        line(doc, indent + 2, r#"</tr>"#);
        line(doc, indent + 1, r#"</thead>"#);
        line(doc, indent + 1, r#"<tbody>"#);
        for r in 0..3 {
            line(doc, indent + 2, r#"<tr>"#);
            line(
                doc,
                indent + 3,
                &format!(r#"<td rowspan="1">Cell {r}.1 <em>styled</em> value</td>"#),
            );
            line(doc, indent + 3, &format!(r#"<td>Cell {r}.2</td>"#));
            let x = extra_inline(idx + r);
            line(
                doc,
                indent + 3,
                &format!(r#"<td>Cell <{x}>{r}.3</{x}> value</td>"#),
            );
            line(doc, indent + 2, r#"</tr>"#);
        }
        line(doc, indent + 1, r#"</tbody>"#);
        line(doc, indent, r#"</table>"#);
    }

    if block_type == 2 || block_type == 6 {
        // code block - exercises datatype values (enum) and CDATA sections
        let languages = ["rust", "python", "javascript", "xml", "json", "sql"];
        let lang = languages[idx % languages.len()];
        line(
            doc,
            indent,
            &format!(
                r#"<pre language="{lang}" line-numbers="true"><![CDATA[if a < b && c > d {{ println!("hello"); }}]]></pre>"#
            ),
        );
    }

    if block_type == 3 || block_type == 6 {
        // figures - exercise empty elements (one written with an end tag), anyURI, a choice of
        // datatype and value, and the two overlapping `figure` definitions: the first figure
        // matches both until its end tag, the second only the first and the third only the
        // second
        line(
            doc,
            indent,
            &format!(r#"<figure xml:id="{id}_f1" float="none">"#),
        );
        line(
            doc,
            indent + 1,
            r#"<img src="http://example.com/img.png" alt="Benchmark figure" width="800" height="auto"></img>"#,
        );
        line(
            doc,
            indent + 1,
            r#"<figcaption>Figure with <strong>rich</strong> caption</figcaption>"#,
        );
        line(doc, indent, r#"</figure>"#);
        line(doc, indent, r#"<figure float="left">"#);
        line(
            doc,
            indent + 1,
            r#"<img src="http://example.com/a.png" alt="Floating figure" width="auto"/>"#,
        );
        line(doc, indent, r#"</figure>"#);
        line(doc, indent, r#"<figure role="gallery">"#);
        line(
            doc,
            indent + 1,
            r#"<img src="http://example.com/b.png" alt="First" width="320" height="200"/>"#,
        );
        line(
            doc,
            indent + 1,
            r#"<img src="http://example.com/c.png" alt="Second" width="320" height="200"/>"#,
        );
        line(doc, indent + 1, r#"<figcaption>A gallery</figcaption>"#);
        line(doc, indent + 1, r#"<credit>Photographer</credit>"#);
        line(doc, indent, r#"</figure>"#);
    }

    if block_type == 4 || block_type == 6 {
        // definition list
        line(doc, indent, r#"<dl>"#);
        for d in 0..3 {
            line(
                doc,
                indent + 1,
                &format!(r#"<dt>Term {d} <em>important</em></dt>"#),
            );
            line(
                doc,
                indent + 1,
                &format!(r#"<dd>Definition {d} with <code>code</code> in it</dd>"#),
            );
        }
        line(doc, indent, r#"</dl>"#);
    }

    if block_type == 5 || block_type == 6 {
        // extension point - exercises nsName with except, attribute wildcards, and foreign
        // elements matched by anyName with an except of a choice of name classes
        line(
            doc,
            indent,
            r#"<app:extensions app:version="2" app:mode="strict">"#,
        );
        line(doc, indent + 1, r#"<app:setting>enabled</app:setting>"#);
        line(
            doc,
            indent + 1,
            r#"<o:note o:kind="aside" plain="yes">Foreign <o:b>marked up</o:b> text &amp; more</o:note>"#,
        );
        line(doc, indent + 1, r#"<app:limit>100</app:limit>"#);
        line(doc, indent, r#"</app:extensions>"#);

        // tree - exercises recursion through a Ref
        line(doc, indent, r#"<node name="root" value="r">"#);
        for n in 0..2 {
            line(doc, indent + 1, &format!(r#"<node name="branch{n}">"#));
            line(doc, indent + 2, r#"<node name="leaf" value="1"/>"#);
            line(doc, indent + 2, r#"<node name="leaf2"></node>"#);
            line(doc, indent + 1, r#"</node>"#);
        }
        line(doc, indent, r#"</node>"#);
    }

    // More paragraphs to increase volume.  The first starts with a title, which only
    // `paragraph` allows.  The second may be either a content block or the section's closing
    // paragraph, so the validator tracks both definitions until it ends.
    for p in 0..2 {
        let x = extra_inline(idx * 2 + p);
        let title = if p == 0 { "<title>Note</title>" } else { "" };
        line(
            doc,
            indent,
            &format!(
                r#"<p>{title}Additional paragraph {p} with <strong>formatting</strong>, <{x}>markup</{x}> and <a href="http://example.com/{p}">links</a>.</p>"#
            ),
        );
    }
    if idx % 3 == 0 {
        // only the closing paragraph allows `kind`
        line(
            doc,
            indent,
            r#"<p kind="summary">In summary, <em>this section</em> covered several features.</p>"#,
        );
    }

    // Nested sections (recursion) - up to depth 2
    if depth < 2 {
        for sub in 0..2 {
            write_section(doc, idx * 10 + sub, depth + 1, indent);
        }
    }

    line(doc, indent - 1, r#"</section>"#);
}

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
    resources.insert("schema.rnc".to_string(), schema());
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
