mod table;

use std::{
    fs::{read_dir, File},
    io::{stdout, Write},
};

use piccolo::{io, Closure, Executor, Lua};

fn run_tests(dir: &str) -> bool {
    let _ = writeln!(stdout(), "running all test scripts in {dir:?}");

    let mut file_failed = false;
    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let file = io::buffered_read(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                let _ = writeln!(stdout(), "running {:?}", path);
                let mut lua = Lua::full();

                if let Err(err) = lua
                    .try_enter(|ctx| {
                        let closure =
                            Closure::load(ctx, Some(path.to_string_lossy().as_ref()), file)?;
                        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
                    })
                    .and_then(|executor| lua.execute::<()>(&executor))
                {
                    let _ = writeln!(stdout(), "error encountered running: {:?}", err);
                    file_failed = true;
                }
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
    file_failed
}

#[test]
fn test_scripts() {
    let mut file_failed = false;

    file_failed |= run_tests("./tests/scripts");

    let _ = writeln!(stdout(), "Running non-required tests");

    let non_required_failed = run_tests("./tests/scripts-wishlist");

    if non_required_failed {
        let _ = writeln!(stdout(), "one or more non-required tests failed");
    }

    if file_failed {
        panic!("one or more errors occurred");
    }
}
