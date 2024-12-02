use criterion::Criterion;
use std::time::Duration;

/// Produces a `Criterion` instance with default settings tuned for faster benchmark runs.
///
/// This results in greater noise; If you wish to reduce it, compile the benchmark,
/// and then separately run it pinned to a specific core:
/// ```bash
/// cargo build --profile bench --benches && taskset -c 0 cargo bench
/// ```
pub fn criterion() -> Criterion {
    Criterion::default()
        .configure_from_args()
        .warm_up_time(Duration::from_millis(100))
        .measurement_time(Duration::from_secs(1))
        .sample_size(20)
}
