use criterion::*;
#[path = "../src/mining.rs"]
mod mining;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("onetime_rank", |b| {
        b.iter(|| {
            mining::ontime_rank(
                "/home/vic/Uni/OPL-ICCS311/Assignmet/3/mining/resources/data/2008.csv",
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
