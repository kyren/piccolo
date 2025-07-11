# Benchmarks

This directory contains benchmarks for `piccolo`.

## Running benchmarks

```
cargo bench
```

Benchmarks currently use Criterion with settings tuned for faster runs at the cost of greater run-to-run variability.
To reduce the noise, you can compile the benchmarks and then run them pinned to a specific core:

```
cargo build --profile bench --benches && taskset -c 0 cargo bench
```

## Adding benchmarks

To benchmark the execution of a script, add it to the `scripts` directory as a separate file, then run `cargo bench`.
The script runner will automatically detect the script and run it as if it was a Rust function.
`criterion` will report differences in execution time between runs as usual.

In case a standalone `.lua` script is not enough, you can add a new benchmark by:
- Adding a new `[[benches]]` entry to `Cargo.toml`, by copying any existing entry and changing the `name` field.
- Creating the corresponding file under `benches`, and adding the following boilerplate:

```rust
#[path = "common/common.rs"]
mod common;

use common::criterion;

fn main() {
  let mut criterion = common::criterion();

  criterion.bench_function("my_benchmark", |bencher| {
    bencher.iter(|| {
      // Your benchmark code here
    });
  });

  criterion.final_summary();
}
```

If your benchmark needs to have some setup code, use `iter_batched` instead of `iter`:

```rust
bencher.iter_batch(
  || {
    // Setup code here
    let mut lua = Lua::core();

    (lua, ...) // anything returned here is available in the next closure
  }
  |(mut lua, ...)| {
    // Your benchmark code here
  },
  // Change this to `LargeInput` only if your benchmark runs out of memory:
  BatchSize::SmallInput,
);
```
