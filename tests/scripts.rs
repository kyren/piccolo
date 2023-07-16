use std::{
    fs::{read_dir, File},
    io::{stdout, Write},
};

use piccolo::{io, Closure, Lua, Thread};

#[test]
fn test_scripts() {
    const DIR: &str = "./tests/scripts";

    let mut file_failed = false;

    let _ = writeln!(stdout(), "running all test scripts in {DIR:?}");

    for dir in read_dir(DIR).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let file = io::buffered_read(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                let _ = writeln!(stdout(), "running {:?}", path);
                let mut lua = Lua::full();

                if let Err(err) = lua
                    .try_run(|ctx| {
                        let closure = Closure::load(ctx, file)?;
                        let thread = Thread::new(&ctx);
                        thread.start(ctx, closure.into(), ())?;
                        Ok(ctx.state.registry.stash(&ctx, thread))
                    })
                    .and_then(|thread| lua.run_thread::<()>(&thread))
                {
                    let _ = writeln!(stdout(), "error encountered running: {:?}", err);
                    file_failed = true;
                }
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }

    if file_failed {
        panic!("one or more errors occurred");
    }
}
