use std::{
    fs::{read_dir, File},
    io::{stdout, Write},
};

use deimos::{compile, io, Closure, Function, Lua, StaticValue};

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
                let mut lua = Lua::new();

                match lua
                    .try_run(|mc, root| {
                        Ok(root.registry.stash(
                            mc,
                            Function::Closure(Closure::new(
                                mc,
                                compile(mc, file)?,
                                Some(root.globals),
                            )?),
                        ))
                    })
                    .and_then(|function| lua.run_function(&function, &[]))
                {
                    Ok(ret) => match &ret[..] {
                        &[StaticValue::Boolean(true)] => {}
                        v => {
                            let _ = writeln!(stdout(), "error: unexpected return values: {:?}", v);
                            file_failed = true;
                        }
                    },
                    Err(err) => {
                        let _ = writeln!(stdout(), "error encountered running: {:?}", err);
                        file_failed = true;
                    }
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
