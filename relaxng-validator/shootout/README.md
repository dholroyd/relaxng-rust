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

# Default: 20 iterations, sizes 2/20/200/2000
cargo run --release

# Custom
cargo run --release -- -n 10 --sizes 5,50,500

# Full shootout (needs jing, xmllint, rnv in PATH)
cargo run --release -- -n 20

# Update the results table in this README
cargo run --release -- --update
```

## Validator discovery

- **rng**: `RELAXNG_RNG_BIN` env var, then `rng` in PATH
- **jing**: `jing` wrapper in PATH (e.g. `apt install jing`);
  fallback to `JING_JAR` env var → `java -jar $JING_JAR`;
  fallback to known jar locations (`/usr/share/java/jing.jar`, etc.)
- **xmllint**: `xmllint` in PATH
- **rnv**: `rnv` in PATH

## Latest results

<!-- BEGIN RESULTS -->
*20 iterations per cell; min time shown (ms).*

| sections | rng 0.1.0 | jing 20241231 | jing -t 20241231 | xmllint libxml 2.14.5 | rnv 1.7.11 |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2 | 2.4 | 170.8 | 113.0 | 1.8 | 1.4 |
| 20 | 5.0 | 173.3 | 122.0 | 5.3 | 3.9 |
| 200 | 24.7 | 233.6 | 172.0 | 42.6 | 27.1 |
| 2000 | 221.4 | 385.6 | 302.0 | 421.1 | 257.9 |

`jing` = wall-clock (includes JVM startup); `jing -t` = jing's self-reported time.
<!-- END RESULTS -->

## Fairness notes

- Same files written to a temp dir before any timing — no in-memory advantage
- All validator stdout/stderr suppressed — no terminal I/O cost in timing
- JVM cold-start is deliberately included for Jing (honest CLI comparison);
  use `jing -t` for Jing's internal algorithmic timing
- **min** is the headline stat (least affected by OS noise); median and mean also shown
