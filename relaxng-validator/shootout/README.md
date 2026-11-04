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

| sections | rng 0.1.0 | jing 20241231 | jing -t 20241231 | xmllint libxml 2.15.2 | rnv 1.7.11 |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2 | 2.4 | 194.6 | 145.0 | 1.9 | 1.6 |
| 20 | 3.5 | 219.6 | 169.0 | 4.9 | 3.7 |
| 200 | 13.6 | 276.1 | 222.0 | 36.3 | 24.2 |
| 2000 | 115.2 | 473.6 | 416.0 | 368.2 | 230.0 |

`jing` = wall-clock (includes JVM startup); `jing -t` = jing's self-reported time.
<!-- END RESULTS -->

## Fairness notes

- Same files written to a temp dir before any timing — no in-memory advantage
- All validator stdout/stderr suppressed — no terminal I/O cost in timing
- JVM cold-start is deliberately included for Jing (honest CLI comparison);
  use `jing -t` for Jing's internal algorithmic timing
- min is shown in hopes this is least affected by noise on the host system
