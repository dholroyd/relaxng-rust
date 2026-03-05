use clap::Parser;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Instant;

#[derive(Parser, Debug)]
#[command(about = "Compare RelaxNG validator performance across implementations")]
struct Args {
    /// Number of runs per (validator, size) pair
    #[arg(short = 'n', long = "iterations", default_value = "20")]
    iterations: usize,

    /// Comma-separated section counts for generated documents
    #[arg(long = "sizes", default_value = "2,20,200,2000", value_delimiter = ',')]
    sizes: Vec<usize>,

    /// Limit to subset of validators (rng,jing,xmllint,rnv)
    #[arg(long = "only", value_delimiter = ',')]
    only: Option<Vec<String>>,

    /// Update the results section in README.md
    #[arg(long = "update")]
    update: bool,
}

// ---------------------------------------------------------------------------
// Document generation (mirrors bench.rs, with XML declaration prepended)
// ---------------------------------------------------------------------------

fn make_document(sections: usize) -> String {
    let mut doc = String::from("<?xml version=\"1.0\"?>\n");
    doc.push_str(r#"<document xmlns="http://example.com/ns" xmlns:app="http://example.com/app" version="1.0" app:generator="shootout">"#);

    // metadata block
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
    doc.push_str(&format!(
        r#"<heading>Section {idx} with <em>emphasis</em> text</heading>"#
    ));

    let block_type = idx % 7;

    // paragraph with mixed content + inline elements
    doc.push_str(r#"<p>This is a paragraph with "#);
    doc.push_str(r#"<strong>bold <em>and italic</em></strong> "#);
    doc.push_str(r#"and <code>inline_code</code> "#);
    doc.push_str(r#"and <a href="http://example.com" title="Example">a link</a> "#);
    doc.push_str(r#"and <span class="highlight">highlighted <em>text</em></span>"#);
    doc.push_str(r#"<br/>"#);
    doc.push_str(r#"with a line break.</p>"#);

    if block_type <= 1 || block_type == 6 {
        // table
        doc.push_str(r#"<table border="1"><caption>Data Table</caption><thead><tr>"#);
        doc.push_str(r#"<th colspan="2">Header A</th><th>Header B</th>"#);
        doc.push_str(r#"</tr></thead><tbody>"#);
        for r in 0..3 {
            doc.push_str(&format!(
                r#"<tr><td rowspan="1">Cell {r}.1 <em>styled</em></td><td>Cell {r}.2</td><td>Cell {r}.3</td></tr>"#
            ));
        }
        doc.push_str(r#"</tbody></table>"#);
    }

    if block_type == 2 || block_type == 6 {
        // code block with enumerated language
        let languages = ["rust", "python", "javascript", "xml", "json", "sql"];
        let lang = languages[idx % languages.len()];
        doc.push_str(&format!(
            r#"<pre language="{lang}" line-numbers="true">fn main() {{ println!("hello"); }}</pre>"#
        ));
    }

    if block_type == 3 || block_type == 6 {
        // figure with empty img element
        doc.push_str(r#"<figure>"#);
        doc.push_str(r#"<img src="http://example.com/img.png" alt="Benchmark figure" width="800" height="600"/>"#);
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

    // extra paragraphs
    for p in 0..2 {
        doc.push_str(&format!(
            r#"<p>Additional paragraph {p} with <strong>formatting</strong> and <a href="http://example.com/{p}">links</a>.</p>"#
        ));
    }

    // nested sections (recursion up to depth 2)
    if depth < 2 {
        for sub in 0..2 {
            write_section(doc, idx * 10 + sub, depth + 1);
        }
    }

    doc.push_str(r#"</section>"#);
}

// ---------------------------------------------------------------------------
// Validator discovery
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum ValidatorKind {
    /// `rng validate <schema.rnc> <doc.xml>`
    Rng { bin: PathBuf },
    /// `jing -c <schema.rnc> <doc.xml>`  (wrapper script)
    JingScript { bin: PathBuf },
    /// `java -jar <jar> -c <schema.rnc> <doc.xml>`
    JingJar { java: PathBuf, jar: PathBuf },
    /// `xmllint --relaxng <schema.rng> --noout <doc.xml>`
    Xmllint { bin: PathBuf },
    /// `rnv <schema.rnc> <doc.xml>`
    Rnv { bin: PathBuf },
}

impl ValidatorKind {
    fn name(&self) -> &'static str {
        match self {
            ValidatorKind::Rng { .. } => "rng",
            ValidatorKind::JingScript { .. } | ValidatorKind::JingJar { .. } => "jing",
            ValidatorKind::Xmllint { .. } => "xmllint",
            ValidatorKind::Rnv { .. } => "rnv",
        }
    }

    fn command(&self, rnc: &Path, rng: &Path, xml: &Path) -> Command {
        match self {
            ValidatorKind::Rng { bin } => {
                let mut cmd = Command::new(bin);
                cmd.args(["validate"]).arg(rnc).arg(xml);
                cmd
            }
            ValidatorKind::JingScript { bin } => {
                let mut cmd = Command::new(bin);
                cmd.arg("-c").arg(rnc).arg(xml);
                cmd
            }
            ValidatorKind::JingJar { java, jar } => {
                let mut cmd = Command::new(java);
                cmd.arg("-jar").arg(jar).arg("-c").arg(rnc).arg(xml);
                cmd
            }
            ValidatorKind::Xmllint { bin } => {
                let mut cmd = Command::new(bin);
                cmd.arg("--relaxng").arg(rng).arg("--noout").arg(xml);
                cmd
            }
            ValidatorKind::Rnv { bin } => {
                let mut cmd = Command::new(bin);
                cmd.arg(rnc).arg(xml);
                cmd
            }
        }
    }

    fn version(&self) -> Option<String> {
        match self {
            ValidatorKind::Rng { bin } => {
                let out = Command::new(bin).arg("--version").output().ok()?;
                let s = String::from_utf8_lossy(&out.stdout);
                // "relaxng-tool 0.1.0" -> "0.1.0"
                s.trim().split_whitespace().last().map(|v| v.to_string())
            }
            ValidatorKind::JingScript { bin } => {
                // `jing` with no args prints "Jing version YYYYMMDD" to stdout
                let out = Command::new(bin)
                    .stdout(Stdio::piped())
                    .stderr(Stdio::null())
                    .output()
                    .ok()?;
                let s = String::from_utf8_lossy(&out.stdout);
                for line in s.lines() {
                    if let Some(rest) = line.strip_prefix("Jing version ") {
                        return Some(rest.trim().to_string());
                    }
                }
                None
            }
            ValidatorKind::JingJar { java, jar } => {
                let out = Command::new(java)
                    .arg("-jar")
                    .arg(jar)
                    .stdout(Stdio::piped())
                    .stderr(Stdio::null())
                    .output()
                    .ok()?;
                let s = String::from_utf8_lossy(&out.stdout);
                for line in s.lines() {
                    if let Some(rest) = line.strip_prefix("Jing version ") {
                        return Some(rest.trim().to_string());
                    }
                }
                None
            }
            ValidatorKind::Xmllint { bin } => {
                let out = Command::new(bin)
                    .arg("--version")
                    .stderr(Stdio::piped())
                    .stdout(Stdio::piped())
                    .output()
                    .ok()?;
                // xmllint outputs version to stderr (or stdout in some builds)
                // Format: "xmllint: using libxml version 21405"
                // where 21405 means 2.14.5 (major*10000 + minor*100 + patch)
                let s = String::from_utf8_lossy(&out.stderr).to_string()
                    + &String::from_utf8_lossy(&out.stdout);
                for line in s.lines() {
                    if let Some(rest) = line
                        .rsplit_once("xmllint: using libxml version ")
                        .map(|(_, r)| r)
                    {
                        if let Ok(v) = rest.trim().parse::<u32>() {
                            return Some(format!(
                                "libxml {}.{}.{}",
                                v / 10000,
                                (v / 100) % 100,
                                v % 100
                            ));
                        }
                        return Some(format!("libxml {}", rest.trim()));
                    }
                }
                None
            }
            ValidatorKind::Rnv { bin } => {
                let out = Command::new(bin)
                    .arg("-v")
                    .stderr(Stdio::piped())
                    .stdout(Stdio::piped())
                    .output()
                    .ok()?;
                // rnv prints to stderr: "rnv version 1.7.11"
                let combined =
                    String::from_utf8_lossy(&out.stderr).to_string()
                    + &String::from_utf8_lossy(&out.stdout);
                for line in combined.lines() {
                    if let Some(rest) = line.strip_prefix("rnv version ") {
                        return Some(rest.trim().to_string());
                    }
                }
                None
            }
        }
    }

    fn name_with_version(&self) -> String {
        let base = self.name();
        match self.version() {
            Some(v) => format!("{base} {v}"),
            None => base.to_string(),
        }
    }

    /// Build a `jing -t` command that reports its own internal timing.
    /// Returns None for non-jing validators.
    fn jing_timed_command(&self, rnc: &Path, xml: &Path) -> Option<Command> {
        match self {
            ValidatorKind::JingScript { bin } => {
                let mut cmd = Command::new(bin);
                cmd.arg("-t").arg("-c").arg(rnc).arg(xml);
                Some(cmd)
            }
            ValidatorKind::JingJar { java, jar } => {
                let mut cmd = Command::new(java);
                cmd.arg("-jar").arg(jar).arg("-t").arg("-c").arg(rnc).arg(xml);
                Some(cmd)
            }
            _ => None,
        }
    }
}

/// Parse jing's "Elapsed time <load>+<validate>=<total> milliseconds" from stderr.
/// Returns (load_ms, validate_ms, total_ms).
fn parse_jing_timing(stderr: &str) -> Option<(f64, f64, f64)> {
    // Format: "Elapsed time 49+47=96 milliseconds"
    for line in stderr.lines() {
        if let Some(rest) = line.strip_prefix("Elapsed time ") {
            if let Some(rest) = rest.strip_suffix(" milliseconds") {
                let parts: Vec<&str> = rest.split('=').collect();
                if parts.len() == 2 {
                    let total: f64 = parts[1].parse().ok()?;
                    let load_validate: Vec<&str> = parts[0].split('+').collect();
                    if load_validate.len() == 2 {
                        let load: f64 = load_validate[0].parse().ok()?;
                        let validate: f64 = load_validate[1].parse().ok()?;
                        return Some((load, validate, total));
                    }
                }
            }
        }
    }
    None
}

fn which(name: &str) -> Option<PathBuf> {
    let output = Command::new("which").arg(name).output().ok()?;
    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if path.is_empty() {
            None
        } else {
            Some(PathBuf::from(path))
        }
    } else {
        None
    }
}

fn discover_rng() -> Option<ValidatorKind> {
    // 1. Binary built from the parent workspace by build.rs (compile-time path)
    if let Some(path) = option_env!("RNG_BUILT_BIN") {
        let p = PathBuf::from(path);
        if p.exists() {
            return Some(ValidatorKind::Rng { bin: p });
        }
    }
    // 2. Runtime override via env var
    if let Ok(val) = std::env::var("RELAXNG_RNG_BIN") {
        let p = PathBuf::from(&val);
        if p.exists() {
            return Some(ValidatorKind::Rng { bin: p });
        }
    }
    // 3. PATH
    which("rng").map(|bin| ValidatorKind::Rng { bin })
}

fn discover_jing() -> Option<ValidatorKind> {
    // 1. jing wrapper script in PATH
    if let Some(bin) = which("jing") {
        return Some(ValidatorKind::JingScript { bin });
    }
    // 2. JING_JAR env var
    let java = which("java")?;
    if let Ok(jar_path) = std::env::var("JING_JAR") {
        let jar = PathBuf::from(&jar_path);
        if jar.exists() {
            return Some(ValidatorKind::JingJar { java, jar });
        }
    }
    // 3. Known jar locations
    for candidate in &[
        "/usr/share/java/jing.jar",
        "/usr/local/share/java/jing.jar",
        "/opt/jing/bin/jing.jar",
    ] {
        let jar = PathBuf::from(candidate);
        if jar.exists() {
            return Some(ValidatorKind::JingJar {
                java: java.clone(),
                jar,
            });
        }
    }
    None
}

fn discover_xmllint() -> Option<ValidatorKind> {
    which("xmllint").map(|bin| ValidatorKind::Xmllint { bin })
}

fn discover_rnv() -> Option<ValidatorKind> {
    which("rnv").map(|bin| ValidatorKind::Rnv { bin })
}

type DiscoverFn = fn() -> Option<ValidatorKind>;

fn discover_validators(only: Option<&[String]>) -> Vec<ValidatorKind> {
    let candidates: &[(&str, DiscoverFn)] = &[
        ("rng", discover_rng),
        ("jing", discover_jing),
        ("xmllint", discover_xmllint),
        ("rnv", discover_rnv),
    ];

    let mut found = Vec::new();
    for (name, discover) in candidates {
        if let Some(filter) = only {
            if !filter.iter().any(|s| s == name) {
                continue;
            }
        }
        match discover() {
            Some(v) => {
                println!("  {name}: found ({v:?})");
                found.push(v);
            }
            None => {
                println!("  {name}: not found, skipping");
            }
        }
    }
    found
}

// ---------------------------------------------------------------------------
// Timing
// ---------------------------------------------------------------------------

struct TimingResult {
    min_ms: f64,
    median_ms: f64,
    mean_ms: f64,
    had_errors: bool,
}

fn run_timing(
    validator: &ValidatorKind,
    rnc: &Path,
    rng: &Path,
    xml: &Path,
    n: usize,
) -> TimingResult {
    let mut samples_ms = Vec::with_capacity(n);
    let mut had_errors = false;

    for _ in 0..n {
        let mut cmd = validator.command(rnc, rng, xml);
        cmd.stdout(Stdio::null()).stderr(Stdio::null());

        let t0 = Instant::now();
        let status = cmd.status().expect("failed to spawn validator process");
        let elapsed = t0.elapsed();

        samples_ms.push(elapsed.as_secs_f64() * 1000.0);
        if !status.success() {
            had_errors = true;
        }
    }

    samples_ms.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let min_ms = samples_ms[0];
    let mean_ms = samples_ms.iter().sum::<f64>() / samples_ms.len() as f64;
    let median_ms = if samples_ms.len() % 2 == 0 {
        (samples_ms[samples_ms.len() / 2 - 1] + samples_ms[samples_ms.len() / 2]) / 2.0
    } else {
        samples_ms[samples_ms.len() / 2]
    };

    TimingResult {
        min_ms,
        median_ms,
        mean_ms,
        had_errors,
    }
}

/// Run jing with `-t` and collect its self-reported timing (excludes JVM startup).
fn run_jing_timing(
    validator: &ValidatorKind,
    rnc: &Path,
    xml: &Path,
    n: usize,
) -> Option<TimingResult> {
    let mut total_samples = Vec::with_capacity(n);
    let mut had_errors = false;

    for _ in 0..n {
        let mut cmd = validator.jing_timed_command(rnc, xml)?;
        cmd.stdout(Stdio::piped()).stderr(Stdio::null());

        let output = cmd.output().expect("failed to spawn jing process");
        if !output.status.success() {
            had_errors = true;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        if let Some((_load, _validate, total)) = parse_jing_timing(&stdout) {
            total_samples.push(total);
        }
    }

    if total_samples.is_empty() {
        return None;
    }

    total_samples.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let min_ms = total_samples[0];
    let mean_ms = total_samples.iter().sum::<f64>() / total_samples.len() as f64;
    let median_ms = if total_samples.len() % 2 == 0 {
        (total_samples[total_samples.len() / 2 - 1] + total_samples[total_samples.len() / 2]) / 2.0
    } else {
        total_samples[total_samples.len() / 2]
    };

    Some(TimingResult {
        min_ms,
        median_ms,
        mean_ms,
        had_errors,
    })
}

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

fn print_table(size: usize, results: &[(String, TimingResult)]) {
    let name_w = results
        .iter()
        .map(|(n, _)| n.len())
        .max()
        .unwrap_or(8)
        .max(9);
    let col_w = 10usize;

    // Header
    println!("\n  Doc size: {} sections", size);
    println!(
        "  {:<name_w$}  {:>col_w$}  {:>col_w$}  {:>col_w$}",
        "validator",
        "min (ms)",
        "median",
        "mean",
        name_w = name_w,
        col_w = col_w,
    );
    println!(
        "  {:-<name_w$}  {:-<col_w$}  {:-<col_w$}  {:-<col_w$}",
        "",
        "",
        "",
        "",
        name_w = name_w,
        col_w = col_w,
    );

    for (name, t) in results {
        let flag = if t.had_errors { " [!]" } else { "" };
        println!(
            "  {:<name_w$}  {:>col_w$.2}  {:>col_w$.2}  {:>col_w$.2}{}",
            name,
            t.min_ms,
            t.median_ms,
            t.mean_ms,
            flag,
            name_w = name_w,
            col_w = col_w,
        );
    }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let args = Args::parse();

    println!("RelaxNG Validator Shootout");
    println!("==========================");
    println!("Discovering validators...");

    let validators = discover_validators(args.only.as_deref());

    if validators.is_empty() {
        eprintln!("No validators found. Install rng, jing, xmllint, or rnv.");
        std::process::exit(1);
    }

    println!(
        "\nRunning {} iterations per (validator, size) pair.",
        args.iterations
    );

    // Set up temp dir and write all files before any timing
    let tmpdir = tempfile::tempdir().expect("create temp dir");
    let tmp = tmpdir.path();

    let rnc_path = tmp.join("document.rnc");
    let rng_path = tmp.join("document.rng");

    std::fs::write(&rnc_path, include_str!("../schemas/catalog.rnc")).expect("write schema.rnc");
    std::fs::write(&rng_path, include_str!("../schemas/catalog.rng")).expect("write schema.rng");

    // Write all document files up front
    let mut doc_paths: Vec<(usize, PathBuf)> = Vec::new();
    for &size in &args.sizes {
        let xml_path = tmp.join(format!("document-{size}.xml"));
        let doc = make_document(size);
        std::fs::write(&xml_path, &doc).expect("write document");
        doc_paths.push((size, xml_path));
    }

    // Resolve versioned names once up front
    let validator_labels: Vec<String> = validators.iter().map(|v| v.name_with_version()).collect();

    // Timing loop: outer = sizes, inner = validators
    let mut all_results: Vec<(usize, Vec<(String, TimingResult)>)> = Vec::new();

    for (size, xml_path) in &doc_paths {
        print!("\nBenchmarking size={}... ", size);
        std::io::stdout().flush().ok();

        let mut size_results: Vec<(String, TimingResult)> = Vec::new();

        for (validator, label) in validators.iter().zip(validator_labels.iter()) {
            print!("{}...", validator.name());
            std::io::stdout().flush().ok();

            let timing = run_timing(validator, &rnc_path, &rng_path, xml_path, args.iterations);
            size_results.push((label.clone(), timing));

            // Also collect jing's self-reported timing (excludes JVM startup)
            if matches!(
                validator,
                ValidatorKind::JingScript { .. } | ValidatorKind::JingJar { .. }
            ) {
                if let Some(jt) =
                    run_jing_timing(validator, &rnc_path, xml_path, args.iterations)
                {
                    let jing_t_label = label.replace("jing", "jing -t");
                    size_results.push((jing_t_label, jt));
                }
            }
        }
        println!(" done");

        all_results.push((*size, size_results));
    }

    // Print results
    println!("\n\nResults");
    println!("=======");
    for (size, results) in &all_results {
        print_table(*size, results);
    }

    // Footer
    println!();
    println!("Notes:");
    println!("  [!] = one or more runs returned non-zero exit status");
    println!("  jing    = wall-clock time including JVM cold-start");
    println!("  jing -t = jing's self-reported time (excludes JVM startup)");
    println!("  min is the headline stat -- least affected by OS scheduling noise.");

    if args.update {
        let md = format_markdown_results(&all_results, args.iterations);
        update_readme(&md);
    }
}

fn format_markdown_results(
    all_results: &[(usize, Vec<(String, TimingResult)>)],
    iterations: usize,
) -> String {
    // Collect all validator names (preserving order from first size)
    let names: Vec<&str> = all_results
        .first()
        .map(|(_, results)| results.iter().map(|(n, _)| n.as_str()).collect())
        .unwrap_or_default();

    let mut md = String::new();
    md.push_str(&format!(
        "*{iterations} iterations per cell; min time shown (ms).*\n\n"
    ));

    // Header row: | sections | validator1 | validator2 | ...
    md.push_str("| sections |");
    for name in &names {
        md.push_str(&format!(" {name} |"));
    }
    md.push('\n');

    // Separator: | --- | ---: | ---: | ...
    md.push_str("| --- |");
    for _ in &names {
        md.push_str(" ---: |");
    }
    md.push('\n');

    // Data rows
    for (size, results) in all_results {
        md.push_str(&format!("| {size} |"));
        for name in &names {
            if let Some((_, t)) = results.iter().find(|(n, _)| n == name) {
                let flag = if t.had_errors { " \\[!\\]" } else { "" };
                md.push_str(&format!(" {:.1}{flag} |", t.min_ms));
            } else {
                md.push_str(" — |");
            }
        }
        md.push('\n');
    }

    md.push_str(
        "\n`jing` = wall-clock (includes JVM startup); \
         `jing -t` = jing's self-reported time.\n",
    );

    md
}

fn update_readme(results_md: &str) {
    let readme_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("README.md");
    let content = match std::fs::read_to_string(&readme_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to read {}: {e}", readme_path.display());
            return;
        }
    };

    const BEGIN: &str = "<!-- BEGIN RESULTS -->";
    const END: &str = "<!-- END RESULTS -->";

    let Some(start) = content.find(BEGIN) else {
        eprintln!(
            "README.md missing '{BEGIN}' marker. Add it where results should appear."
        );
        return;
    };
    let Some(end) = content.find(END) else {
        eprintln!("README.md missing '{END}' marker.");
        return;
    };

    let mut new_content = String::new();
    new_content.push_str(&content[..start + BEGIN.len()]);
    new_content.push('\n');
    new_content.push_str(results_md);
    new_content.push_str(&content[end..]);

    if let Err(e) = std::fs::write(&readme_path, &new_content) {
        eprintln!("Failed to write {}: {e}", readme_path.display());
        return;
    }
    println!("\nUpdated {}", readme_path.display());
}
