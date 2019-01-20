use std::fs::{read_dir, File};
use std::io::{stdout, Write};

use luster::compiler::compile;
use luster::function::Closure;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::lua_sequence;
use luster::parser::parse_chunk;
use luster::sequence::{sequence_fn, SequenceExt};
use luster::value::Value;

fn test_dir(dir: &str, run_code: bool) {
    let mut file_failed = false;

    let op = if run_code { "running" } else { "parsing" };
    let _ = writeln!(stdout(), "{} all files in '{}'", op, dir);

    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let file = buffered_read(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                let _ = writeln!(stdout(), "{} file {:?}", op, path);
                if run_code {
                    let mut lua = Lua::new();
                    let r = lua_sequence!(
                        lua,
                        sequence_fn(move |mc, lc| Ok(Closure::new(
                            mc,
                            compile(mc, lc.interned_strings, file)?,
                            Some(lc.globals),
                        )?))
                        .and_then(move |mc, lc, closure| lc.main_thread.call_function(
                            mc,
                            closure,
                            &[],
                            64,
                        ))
                        .map(|r| match &r[..] {
                            &[Value::Boolean(true)] => false,
                            v => {
                                let _ =
                                    writeln!(stdout(), "error: unexpected return values: {:?}", v);
                                true
                            }
                        })
                    );

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
