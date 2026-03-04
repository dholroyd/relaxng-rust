use codemap::CodeMap;
use relaxng_model::Compiler;
use relaxng_validator::{CoverageReport, Validator};

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::exit;

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Cli {
    /// Validate XML documents against a RELAX NG schema
    Validate { schema: PathBuf, xml: Vec<PathBuf> },
    /// Validate XML documents and report schema coverage
    Coverage {
        schema: PathBuf,
        xml: Vec<PathBuf>,
        #[structopt(long, default_value = "text", possible_values = &["text", "html"])]
        format: String,
    },
}

fn main() {
    match Cli::from_args() {
        Cli::Validate { schema, xml } => validate(schema, xml),
        Cli::Coverage {
            schema,
            xml,
            format,
        } => coverage(schema, xml, &format),
    }
}

fn compile_schema(
    schema: &PathBuf,
) -> std::rc::Rc<std::cell::RefCell<Option<relaxng_model::model::DefineRule>>> {
    let mut compiler = Compiler::default();
    match compiler.compile(schema) {
        Ok(m) => m,
        Err(err) => {
            compiler.dump_diagnostic(&err);
            exit(1);
        }
    }
}

fn run_validation<'a>(v: &mut Validator<'a>, xml: &PathBuf, doc: String) {
    loop {
        match v.validate_next() {
            Some(Ok(())) => {}
            Some(Err(err)) => {
                let (map, d) = v.diagnostic(xml.to_string_lossy().to_string(), doc, &err);
                let mut emitter = codemap_diagnostic::Emitter::stderr(
                    codemap_diagnostic::ColorConfig::Auto,
                    Some(&map),
                );
                emitter.emit(&d[..]);
                exit(2);
            }
            None => break,
        }
    }
}

fn validate(schema: PathBuf, xmls: Vec<PathBuf>) {
    let model = compile_schema(&schema);
    for xml in &xmls {
        let mut f = File::open(xml).expect("open example xml");
        let mut doc = String::new();
        f.read_to_string(&mut doc).expect("read xml");
        let src = doc.clone();
        let reader = xmlparser::Tokenizer::from(&src[..]);
        let mut v = Validator::new(model.clone(), reader);
        eprintln!("Validating {xml:?}");
        run_validation(&mut v, xml, doc);
    }
}

fn coverage(schema: PathBuf, xmls: Vec<PathBuf>, format: &str) {
    let mut compiler = Compiler::default();
    let model = match compiler.compile(&schema) {
        Ok(m) => m,
        Err(err) => {
            compiler.dump_diagnostic(&err);
            exit(1);
        }
    };
    let mut merged_report: Option<CoverageReport> = None;
    for xml in &xmls {
        let mut f = File::open(xml).expect("open example xml");
        let mut doc = String::new();
        f.read_to_string(&mut doc).expect("read xml");
        let src = doc.clone();
        let reader = xmlparser::Tokenizer::from(&src[..]);
        let mut v = Validator::new_with_coverage(model.clone(), reader);
        eprintln!("Validating {xml:?}");
        run_validation(&mut v, xml, doc);
        if let Some(report) = v.coverage_report() {
            match merged_report {
                Some(ref mut merged) => merged.merge(&report),
                None => merged_report = Some(report),
            }
        }
    }
    if let Some(report) = merged_report {
        match format {
            "html" => print_coverage_html(&report, compiler.code_map()),
            _ => print_coverage_text(&report, compiler.code_map()),
        }
    }
}

fn print_coverage_text(report: &CoverageReport, code_map: &CodeMap) {
    let uncovered: Vec<_> = report.uncovered_patterns().collect();
    if uncovered.is_empty() {
        println!(
            "Coverage: {0}/{0} trackable patterns covered",
            report.total_trackable()
        );
        return;
    }

    // Emit uncovered patterns as diagnostics for those with spans
    let mut no_span_patterns = vec![];
    for p in &uncovered {
        if p.spans.is_empty() {
            no_span_patterns.push(p);
            continue;
        }
        let labels: Vec<_> = p
            .spans
            .iter()
            .map(|span| codemap_diagnostic::SpanLabel {
                span: *span,
                style: codemap_diagnostic::SpanStyle::Primary,
                label: None,
            })
            .collect();
        let d = codemap_diagnostic::Diagnostic {
            level: codemap_diagnostic::Level::Warning,
            message: format!("Uncovered {}: {}", p.kind, p.name),
            code: None,
            spans: labels,
        };
        let mut emitter = codemap_diagnostic::Emitter::stderr(
            codemap_diagnostic::ColorConfig::Auto,
            Some(code_map),
        );
        emitter.emit(&[d]);
    }

    for p in &no_span_patterns {
        eprintln!("warning: Uncovered {}: {}", p.kind, p.name);
    }

    eprintln!(
        "\nCoverage: {}/{} trackable patterns covered",
        report.covered_count(),
        report.total_trackable()
    );
}

fn resolve_location(span: &codemap::Span, code_map: &CodeMap) -> String {
    let loc = code_map.look_up_span(*span);
    format!("{}:{}", loc.file.name(), loc.begin.line + 1,)
}

fn print_coverage_html(report: &CoverageReport, code_map: &CodeMap) {
    let covered = report.covered_count();
    let total = report.total_trackable();
    let pct = if total > 0 {
        covered as f64 / total as f64 * 100.0
    } else {
        100.0
    };

    println!(
        r#"<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Schema Coverage Report</title>
<style>
  body {{ font-family: system-ui, -apple-system, sans-serif; margin: 2rem; color: #1a1a1a; }}
  h1 {{ font-size: 1.4rem; }}
  .summary {{ margin: 1rem 0; padding: 1rem; background: #f5f5f5; border-radius: 6px; }}
  .bar {{ height: 1.2rem; background: #e0e0e0; border-radius: 4px; overflow: hidden; margin-top: 0.5rem; }}
  .bar-fill {{ height: 100%; background: #2e7d32; border-radius: 4px; }}
  table {{ border-collapse: collapse; width: 100%; margin-top: 1rem; }}
  th, td {{ text-align: left; padding: 0.4rem 0.8rem; border-bottom: 1px solid #e0e0e0; }}
  th {{ background: #fafafa; font-weight: 600; }}
  .covered {{ color: #2e7d32; }}
  .missing {{ color: #c62828; }}
  tr.missing {{ background: #fce4e4; }}
</style>
</head>
<body>
<h1>Schema Coverage Report</h1>
<div class="summary">
  <strong>{covered}</strong> / <strong>{total}</strong> trackable patterns covered ({pct:.1}%)
  <div class="bar"><div class="bar-fill" style="width:{pct:.1}%"></div></div>
</div>
<table>
<thead><tr><th>Status</th><th>Name</th><th>Kind</th><th>Location</th></tr></thead>
<tbody>"#
    );

    for p in report.patterns() {
        let is_covered = report.is_covered(p.pat_id);
        let (class, label) = if is_covered {
            ("covered", "COVERED")
        } else {
            ("missing", "MISSING")
        };
        let name = html_escape(&p.name);
        let location = if let Some(span) = p.spans.first() {
            html_escape(&resolve_location(span, code_map))
        } else {
            String::new()
        };
        println!(
            r#"<tr class="{class}"><td class="{class}">{label}</td><td><code>{name}</code></td><td>{kind}</td><td>{location}</td></tr>"#,
            kind = p.kind
        );
    }

    println!(
        r#"</tbody>
</table>
</body>
</html>"#
    );
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}
