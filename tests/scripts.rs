mod table;

use std::{
    fs::{read_dir, File},
    io::{stdout, Write},
};

use piccolo::{io, Closure, Executor, Lua};

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

    if file_failed {
        panic!("one or more errors occurred");
    }
}
