# relaxng shootout

A standalone binary that times Jing (Java), xmllint (C), RNV (C), and the `rng`
Rust validator against identical schema/document pairs and prints a comparison table.

## Validators compared

| Name    | Language | Schema format | Notes                              |
|---------|----------|---------------|------------------------------------|
| rng     | Rust     | .rnc          | this project (`rng validate`)      |
| jing    | Java     | .rnc (`-c`)   | JVM cold-start included in timing  |
| xmllint | C        | .rng XML      | `xmllint --relaxng --noout`        |
| rnv     | C        | .rnc          | `rnv schema.rnc doc.xml`           |

Missing validators are skipped gracefully.

## Build

This crate is intentionally **not part of the workspace** so it doesn't pull in
extra dependencies for the main project.

```sh
cd relaxng-validator/shootout
cargo build --release
```

## Usage

```sh
# Quick smoke test — only our validator, 3 runs
cargo run --release -- --only rng -n 3

# Default: 20 iterations, sizes 10/50/100/500
cargo run --release

# Custom
cargo run --release -- -n 10 --sizes 10,50,100

# Full shootout (needs jing, xmllint, rnv in PATH)
cargo run --release -- -n 20
```

## Validator discovery

- **rng**: `RELAXNG_RNG_BIN` env var, then `rng` in PATH
- **jing**: `jing` wrapper in PATH (e.g. `apt install jing`);
  fallback to `JING_JAR` env var → `java -jar $JING_JAR`;
  fallback to known jar locations (`/usr/share/java/jing.jar`, etc.)
- **xmllint**: `xmllint` in PATH
- **rnv**: `rnv` in PATH

## Fairness notes

- Same files written to a temp dir before any timing — no in-memory advantage
- All validator stdout/stderr suppressed — no terminal I/O cost in timing
- JVM cold-start is deliberately included for Jing (honest CLI comparison);
  use `jing -t` for Jing's internal algorithmic timing
- **min** is the headline stat (least affected by OS noise); median and mean also shown
