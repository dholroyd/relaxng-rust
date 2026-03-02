use std::path::PathBuf;
use std::process::Command;

fn main() {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    // shootout/ lives at relaxng-validator/shootout/, so two levels up is the workspace root
    let workspace_root = manifest_dir
        .join("../..")
        .canonicalize()
        .expect("could not resolve workspace root");

    eprintln!(
        "shootout build.rs: building rng from {}",
        workspace_root.display()
    );

    let status = Command::new("cargo")
        .args(["build", "--release", "--bin", "rng"])
        .current_dir(&workspace_root)
        .status()
        .expect("failed to spawn cargo build for rng");

    if !status.success() {
        panic!("cargo build --release --bin rng failed");
    }

    let rng_bin = workspace_root.join("target/release/rng");
    println!("cargo:rustc-env=RNG_BUILT_BIN={}", rng_bin.display());
}
