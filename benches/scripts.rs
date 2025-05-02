#[path = "common/common.rs"]
mod common;

use criterion::BatchSize;
use piccolo::{Closure, Executor, Lua};
use std::ffi::OsStr;
use std::fs;
use std::path::Path;

fn main() {
    let mut criterion = common::criterion();

    let mut entries = Vec::new();
    for entry in
        fs::read_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("benches/scripts")).unwrap()
    {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension() == Some(OsStr::new("lua")) {
            entries.push((
                path.file_stem().unwrap().to_str().unwrap().to_owned(),
                fs::read_to_string(path).unwrap(),
            ));
        }
    }

    for (name, script) in entries {
        criterion.bench_function(&name, |bencher| {
            bencher.iter_batched(
                || {
                    let mut lua = Lua::full();
                    let executor = lua.enter(|ctx| ctx.stash(Executor::new(ctx)));
                    let script = lua.enter(|ctx| {
                        ctx.stash(Closure::load(ctx, Some("benchmark"), script.as_bytes()).unwrap())
                    });
                    (lua, executor, script)
                },
                |(mut lua, executor, script)| {
                    lua.enter(|ctx| {
                        let script = ctx.fetch(&script);
                        let executor = ctx.fetch(&executor);
                        executor.restart(ctx, script.into(), ());
                    });
                    lua.execute::<()>(&executor).unwrap();
                },
                BatchSize::SmallInput,
            )
        });
    }

    criterion.final_summary();
}
