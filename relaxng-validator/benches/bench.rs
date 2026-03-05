use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use relaxng_model::{Compiler, Files, Syntax};
use relaxng_validator::Validator;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use xmlparser::Tokenizer;

/// schema exercising most features:
/// - Namespaces (default + prefixed)
/// - Grammar with named definitions and mutual recursion
/// - All occurrence operators: optional(?), zeroOrMore(*), oneOrMore(+)
/// - All composition: group(,), choice(|), interleave(&)
/// - Mixed content
/// - XSD datatypes with facets (integer ranges, string lengths, patterns, decimals, dates, booleans)
/// - Datatype values (enumerated string values)
/// - List pattern (whitespace-separated tokens)
/// - NameClass: named, nsName (ns:*), anyName (*), except
/// - Nested elements with recursion (tree structures)
/// - Interleave of attributes and elements
/// - Attribute patterns with datatypes
const SCHEMA: &str = r#"
namespace app = "http://example.com/app"
default namespace = "http://example.com/ns"

start = element document {
    attribute version { "1.0" | "2.0" },
    attribute app:generator { text }?,
    metadata,
    body
}

metadata = element metadata {
    element title { xsd:token { minLength = "1" maxLength = "200" } },
    element created { xsd:date },
    element modified { xsd:dateTime }?,
    element revision { xsd:positiveInteger }?,
    element score { xsd:decimal { fractionDigits = "2" } }?,
    element tags { list { xsd:token+ } }?,
    element author {
        attribute email { xsd:string { pattern = ".+@.+\..+" } },
        attribute active { xsd:boolean },
        text
    }+
}

body = element body {
    section+
}

section = element section {
    attribute id { xsd:ID },
    attribute priority { xsd:integer { minInclusive = "1" maxInclusive = "10" } }?,
    element heading { mixed { element em { text }* } },
    content-block*,
    section*
}

content-block = paragraph | table | code-block | figure | data-list

paragraph = element p {
    mixed {
        (inline-element | element br { empty })*
    }
}

inline-element =
    element strong { mixed { inline-element* } }
  | element em { mixed { inline-element* } }
  | element code { text }
  | element a {
        attribute href { xsd:anyURI },
        attribute title { text }?,
        mixed { inline-element* }
    }
  | element span {
        attribute class { xsd:token },
        mixed { inline-element* }
    }

table = element table {
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
        mixed { inline-element* }
    } | element td {
        attribute colspan { xsd:positiveInteger }?,
        attribute rowspan { xsd:positiveInteger }?,
        mixed { inline-element* }
    })+
}

code-block = element pre {
    attribute language { "rust" | "python" | "javascript" | "xml" | "json" | "sql" },
    attribute line-numbers { xsd:boolean }?,
    text
}

figure = element figure {
    element img {
        attribute src { xsd:anyURI },
        attribute alt { text },
        attribute width { xsd:nonNegativeInteger }?,
        attribute height { xsd:nonNegativeInteger }?,
        empty
    },
    element figcaption { mixed { inline-element* } }?
}

data-list = element dl {
    (element dt { mixed { inline-element* } },
     element dd { mixed { inline-element* } })+
}

# Recursive tree structure exercising Ref patterns
tree-node = element node {
    attribute name { xsd:NCName },
    attribute value { text }?,
    tree-node*
}

# Wildcard / nsName patterns
extension-point = element app:extensions {
    element app:* { text }*
}
"#;

/// Generate XML documents that exercise all schema features.
fn make_document(sections: usize) -> String {
    let mut doc = String::new();
    doc.push_str(r#"<document xmlns="http://example.com/ns" xmlns:app="http://example.com/app" version="1.0" app:generator="bench">"#);

    // metadata block - exercises datatypes, list, boolean, date, decimal, pattern facet
    doc.push_str(r#"<metadata>"#);
    doc.push_str(r#"<title>Benchmark Document With Many Features</title>"#);
    doc.push_str(r#"<created>2025-06-15</created>"#);
    doc.push_str(r#"<modified>2025-06-15T10:30:00</modified>"#);
    doc.push_str(r#"<revision>42</revision>"#);
    doc.push_str(r#"<score>98.75</score>"#);
    doc.push_str(r#"<tags>benchmark performance testing validation</tags>"#);
    doc.push_str(r#"<author email="alice@example.com" active="true">Alice Smith</author>"#);
    doc.push_str(r#"<author email="bob@test.org" active="false">Bob Jones</author>"#);
    doc.push_str(r#"</metadata>"#);

    // body with sections
    doc.push_str(r#"<body>"#);

    for s in 0..sections {
        write_section(&mut doc, s, 0);
    }

    doc.push_str(r#"</body>"#);
    doc.push_str(r#"</document>"#);
    doc
}

fn write_section(doc: &mut String, idx: usize, depth: usize) {
    let priority = (idx % 10) + 1;
    doc.push_str(&format!(
        r#"<section id="s{depth}_{idx}" priority="{priority}">"#
    ));

    // heading with mixed content (em elements)
    doc.push_str(&format!(
        r#"<heading>Section {idx} with <em>emphasis</em> text</heading>"#
    ));

    // Vary content blocks by index to exercise all branches
    let block_type = idx % 7;

    // Always include a paragraph - exercises mixed content + inline elements
    doc.push_str(r#"<p>This is a paragraph with "#);
    doc.push_str(r#"<strong>bold <em>and italic</em></strong> "#);
    doc.push_str(r#"and <code>inline_code</code> "#);
    doc.push_str(r#"and <a href="http://example.com" title="Example">a link</a> "#);
    doc.push_str(r#"and <span class="highlight">highlighted <em>text</em></span>"#);
    doc.push_str(r#"<br/>"#);
    doc.push_str(r#"with a line break.</p>"#);

    if block_type <= 1 || block_type == 6 {
        // table - exercises interleave of th/td, colspan/rowspan, caption
        doc.push_str(r#"<table border="1">"#);
        doc.push_str(r#"<caption>Data Table</caption>"#);
        doc.push_str(r#"<thead><tr>"#);
        doc.push_str(r#"<th colspan="2">Header A</th>"#);
        doc.push_str(r#"<th>Header B</th>"#);
        doc.push_str(r#"</tr></thead>"#);
        doc.push_str(r#"<tbody>"#);
        for r in 0..3 {
            doc.push_str(r#"<tr>"#);
            doc.push_str(&format!(
                r#"<td rowspan="1">Cell {r}.1 <em>styled</em></td>"#
            ));
            doc.push_str(&format!(r#"<td>Cell {r}.2</td>"#));
            doc.push_str(&format!(r#"<td>Cell {r}.3</td>"#));
            doc.push_str(r#"</tr>"#);
        }
        doc.push_str(r#"</tbody></table>"#);
    }

    if block_type == 2 || block_type == 6 {
        // code block - exercises datatype values (enum)
        let languages = ["rust", "python", "javascript", "xml", "json", "sql"];
        let lang = languages[idx % languages.len()];
        doc.push_str(&format!(
            r#"<pre language="{lang}" line-numbers="true">fn main() {{ println!("hello"); }}</pre>"#
        ));
    }

    if block_type == 3 || block_type == 6 {
        // figure - exercises empty element, optional elements, anyURI
        doc.push_str(r#"<figure>"#);
        doc.push_str(
            r#"<img src="http://example.com/img.png" alt="Benchmark figure" width="800" height="600"/>"#,
        );
        doc.push_str(r#"<figcaption>Figure with <strong>rich</strong> caption</figcaption>"#);
        doc.push_str(r#"</figure>"#);
    }

    if block_type == 4 || block_type == 6 {
        // definition list
        doc.push_str(r#"<dl>"#);
        for d in 0..3 {
            doc.push_str(&format!(
                r#"<dt>Term {d} <em>important</em></dt><dd>Definition {d} with <code>code</code></dd>"#
            ));
        }
        doc.push_str(r#"</dl>"#);
    }

    // Add more paragraphs to increase volume
    for p in 0..2 {
        doc.push_str(&format!(
            r#"<p>Additional paragraph {p} with <strong>formatting</strong> and <a href="http://example.com/{p}">links</a>.</p>"#
        ));
    }

    // Nested sections (recursion) - up to depth 2
    if depth < 2 {
        for sub in 0..2 {
            write_section(doc, idx * 10 + sub, depth + 1);
        }
    }

    doc.push_str(r#"</section>"#);
}

struct InMemoryFiles(HashMap<String, String>);

impl Files for InMemoryFiles {
    fn load(&self, path: &Path) -> Result<String, relaxng_model::RelaxError> {
        self.0.get(path.to_str().unwrap()).cloned().ok_or_else(|| {
            relaxng_model::RelaxError::Io(
                path.to_path_buf(),
                std::io::Error::from(std::io::ErrorKind::NotFound),
            )
        })
    }
}

fn compile_schema() -> Rc<RefCell<Option<relaxng_model::model::DefineRule>>> {
    let mut resources = HashMap::new();
    resources.insert("schema.rnc".to_string(), SCHEMA.to_string());
    let mut compiler = Compiler::new(InMemoryFiles(resources), Syntax::Compact);
    compiler
        .compile(Path::new("schema.rnc"))
        .expect("compile schema")
        .start
}

fn validate(schema: Rc<RefCell<Option<relaxng_model::model::DefineRule>>>, doc: &str) {
    let tokenizer = Tokenizer::from(doc);
    let mut v = Validator::new(schema, tokenizer).unwrap();
    loop {
        match v.validate_next() {
            None => break,
            Some(Ok(())) => {}
            Some(Err(e)) => panic!("Validation error: {e:?}"),
        }
    }
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
