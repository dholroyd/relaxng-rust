use criterion::*;
use relaxng_model::Compiler;
use std::path::Path;

fn relaxng_compile(c: &mut Criterion) {
    c.bench(
        "compile_many",
        Benchmark::new("compile_many", move |b| {
            b.iter(|| {
                let tests = [
                    "../relaxng-compact-syntax/resources/examples/fo/main.rnc",
                    "../../dash-mpd/DASH-MPD.rnc",
                ];
                for n in &tests {
                    let mut compiler = Compiler::default();
                    let input = Path::new(n);
                    compiler.compile(input).expect("compiler.load()");
                }
            });
        }),
    );
}

criterion_group!(benches, relaxng_compile);
criterion_main!(benches);
