//! The schema and documents that the validator benchmarks use.
//!
//! Shared by `benches/validator.rs` and by the `shootout` crate (which includes this file by
//! path, so it must use nothing outside `std`), so that both measure the same thing.

/// The schema, in compact syntax.  `schema.rng` beside it is the same in XML syntax.
pub const SCHEMA: &str = include_str!("schema.rnc");

/// How many `inline1` … `inlineN` elements the schema adds to the `inline-element` choice, on top
/// of the five named ones (see the comment there for why), for the documents to use.
const EXTRA_INLINE_ELEMENTS: usize = 40;

/// Generate XML documents that exercise all schema features.
///
/// Block-level structure is indented, as in most real documents, so element-only content
/// contains whitespace text nodes.  Text content alternates text and inline elements, so that
/// text nodes regularly follow child elements, and includes entity and character references
/// and CDATA sections.
pub fn make_document(sections: usize) -> String {
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
