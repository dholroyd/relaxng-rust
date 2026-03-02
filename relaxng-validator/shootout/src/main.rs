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

    /// Comma-separated item counts for generated documents
    #[arg(long = "sizes", default_value = "500,1000,5000", value_delimiter = ',')]
    sizes: Vec<usize>,

    /// Limit to subset of validators (rng,jing,xmllint,rnv)
    #[arg(long = "only", value_delimiter = ',')]
    only: Option<Vec<String>>,
}

// ---------------------------------------------------------------------------
// Document generation (copied from bench.rs, with XML declaration prepended)
// ---------------------------------------------------------------------------

fn make_document(items: usize) -> String {
    let mut doc = String::from("<?xml version=\"1.0\"?>\n");
    doc.push_str(r#"<catalog version="1.0">"#);
    for i in 0..items {
        let attrs = format!(
            r#" id="item-{i}" type="kind-{}" category="cat-{}""#,
            i % 5,
            i % 3
        );
        doc.push_str(&format!(r#"<item{attrs}><name>Item {i}</name>"#));
        if i % 3 == 0 {
            doc.push_str(&format!(
                "<description>Description for item {i}</description>"
            ));
        }
        for t in 0..(i % 4) {
            doc.push_str(&format!("<tag>tag-{t}</tag>"));
        }
        for p in 0..(i % 3) {
            doc.push_str(&format!(r#"<property key="prop-{p}">value-{p}</property>"#));
        }
        doc.push_str("</item>");
    }
    doc.push_str("</catalog>");
    doc
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

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

fn print_table(size: usize, results: &[(&str, TimingResult)]) {
    let name_w = results
        .iter()
        .map(|(n, _)| n.len())
        .max()
        .unwrap_or(8)
        .max(9);
    let col_w = 10usize;

    // Header
    println!("\n  Doc size: {} items", size);
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

    let rnc_path = tmp.join("catalog.rnc");
    let rng_path = tmp.join("catalog.rng");

    std::fs::write(&rnc_path, include_str!("../schemas/catalog.rnc")).expect("write catalog.rnc");
    std::fs::write(&rng_path, include_str!("../schemas/catalog.rng")).expect("write catalog.rng");

    // Write all document files up front
    let mut doc_paths: Vec<(usize, PathBuf)> = Vec::new();
    for &size in &args.sizes {
        let xml_path = tmp.join(format!("catalog-{size}.xml"));
        let doc = make_document(size);
        std::fs::write(&xml_path, &doc).expect("write document");
        doc_paths.push((size, xml_path));
    }

    // Timing loop: outer = sizes, inner = validators
    let mut all_results: Vec<(usize, Vec<(&str, TimingResult)>)> = Vec::new();

    for (size, xml_path) in &doc_paths {
        print!("\nBenchmarking size={}... ", size);
        std::io::stdout().flush().ok();

        let mut size_results: Vec<(&str, TimingResult)> = Vec::new();

        for validator in &validators {
            print!("{}...", validator.name());
            std::io::stdout().flush().ok();

            let timing = run_timing(validator, &rnc_path, &rng_path, xml_path, args.iterations);
            size_results.push((validator.name(), timing));
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
    println!("  Jing timings include JVM cold-start (~250-400ms);");
    println!("  use `jing -t` for Jing's internal (algorithmic) timing.");
    println!("  min is the headline stat -- least affected by OS scheduling noise.");
}
