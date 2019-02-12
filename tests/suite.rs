use std::fs::{read_dir, File};
use std::io::{stdout, Write};

use gc_sequence::{self as sequence, SequenceExt, SequenceResultExt};
use luster::{compile, io, parse_chunk, Closure, Error, Function, Lua, ThreadSequence, Value};

fn test_dir(dir: &str, run_code: bool) {
    let mut file_failed = false;

    let op = if run_code { "running" } else { "parsing" };
    let _ = writeln!(stdout(), "{} all files in '{}'", op, dir);

    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let file = io::buffered_read(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                let _ = writeln!(stdout(), "{} file {:?}", op, path);
                if run_code {
                    let mut lua = Lua::new();
                    let r = lua.sequence(|root| {
                        sequence::from_fn_with(root, move |mc, root| {
                            Ok(Closure::new(
                                mc,
                                compile(mc, root.interned_strings, file)?,
                                Some(root.globals),
                            )?)
                        })
                        .and_chain_with(root, move |mc, root, closure| {
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
                                let _ =
                                    writeln!(stdout(), "error: unexpected return values: {:?}", v);
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
                } else {
                    if let Err(err) = parse_chunk(file, |s| s.to_vec().into_boxed_slice()) {
                        let _ = writeln!(stdout(), "error encountered parsing: {:?}", err);
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

#[test]
fn test_suite_parsing() {
    test_dir("./tests/parsing", false);
}

#[test]
fn test_suite_running() {
    test_dir("./tests/running", true);
}
