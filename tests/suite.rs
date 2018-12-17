extern crate failure;
extern crate luster;

use std::fs::{read_dir, File};
use std::io::{stdout, Write};
use std::path::PathBuf;

use failure::Error;

use luster::compiler::compile_chunk;
use luster::function::Closure;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::parser::{parse_chunk, Chunk};
use luster::sequence::SequenceExt;
use luster::value::Value;

fn parse_file(path: &PathBuf) -> Result<Chunk, Error> {
    parse_chunk(buffered_read(File::open(path)?)?)
}

fn test_dir(dir: &str, run_code: bool) {
    let mut file_failed = false;

    let op = if run_code { "running" } else { "parsing" };
    let _ = writeln!(stdout(), "{} all files in '{}'", op, dir);

    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                let _ = writeln!(stdout(), "{} file {:?}", op, path);
                match parse_file(&path) {
                    Err(err) => {
                        let _ = writeln!(stdout(), "error encountered parsing: {:?}", err);
                        file_failed = true;
                    }
                    Ok(chunk) => {
                        if run_code {
                            let mut lua = Lua::new();
                            let r = lua.sequence(move |mc, lc| {
                                Ok(Box::new(
                                    lc.main_thread
                                        .call_function(
                                            mc,
                                            Closure::new(mc, compile_chunk(mc, &chunk)?)?,
                                            &[],
                                            64,
                                        )
                                        .map(|_, r| match &r[..] {
                                            &[Value::Boolean(true)] => Ok(false),
                                            v => {
                                                let _ = writeln!(
                                                    stdout(),
                                                    "unexpected return values: {:?}",
                                                    v
                                                );
                                                Ok(true)
                                            }
                                        }),
                                ))
                            });

                            match r {
                                Err(err) => {
                                    let _ =
                                        writeln!(stdout(), "error encountered running: {:?}", err);
                                    file_failed = true;
                                }
                                Ok(true) => {
                                    file_failed = true;
                                }
                                Ok(false) => {}
                            }
                        }
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
