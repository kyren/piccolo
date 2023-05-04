use std::{
    fs::{read_dir, File},
    io::{stdout, Write},
};

use luster::{
    compile, io, sequence, Closure, Error, Function, Lua, SequenceExt, ThreadSequence,
    TrySequenceExt, Value,
};

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
                let r = lua.sequence(|root| {
                    sequence::from_fn_with(root, move |root, mc| {
                        Ok(Closure::new(mc, compile(mc, file)?, Some(root.globals))?)
                    })
                    .and_chain_with(root, move |root, mc, closure| {
                        Ok(ThreadSequence::call_function(
                            mc,
                            root.main_thread,
                            Function::Closure(closure),
                            &[],
                        )?)
                    })
                    .map_ok(|r| match &r[..] {
                        &[Value::Boolean(true)] => false,
                        v => {
                            let _ = writeln!(stdout(), "error: unexpected return values: {:?}", v);
                            true
                        }
                    })
                    .map_err(Error::to_static)
                    .boxed()
                });

                match r {
                    Err(err) => {
                        let _ = writeln!(stdout(), "error encountered running: {:?}", err);
                        file_failed = true;
                    }
                    Ok(true) => {
                        file_failed = true;
                    }
                    Ok(false) => {}
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
