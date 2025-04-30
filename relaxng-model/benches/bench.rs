use criterion::*;
use relaxng_model::Compiler;
use std::path::Path;

fn relaxng_compile(c: &mut Criterion) {
    let mut group = c.benchmark_group("compile_many");
    let tests = [
        "../relaxng-compact-syntax/resources/examples/fo/main.rnc",
        "../../dash-mpd/DASH-MPD.rnc",
    ];

    for test in &tests {
        if !Path::new(test).exists() {
            println!("Missing file: {}", test);
            return;
        }
    }
    group.bench_function("compile_many", |b| {
        b.iter(|| {
            for n in &tests {
                let mut compiler = Compiler::default();
                let input = Path::new(n);
                compiler.compile(input).expect("compiler.load()");
            }
        });
    });
    group.finish();
}

criterion_group!(benches, relaxng_compile);
criterion_main!(benches);
