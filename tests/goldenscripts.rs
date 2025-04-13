//! These tests test the Debug output of errors
//! and the standard output of scripts
//!
//! they look like
//! ```
//! --- <mode>
//! --- output
//! --- output
//! ---
//! <script>
//! ```
//! where `mode` dictates how to handle errors
//! and `script` is a valid Lua script.

use piccolo::{
    io, BoxSequence, Callback, CallbackReturn, Closure, Context, Execution, Executor, Lua, Stack,
};
use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader, Seek, SeekFrom},
    path::PathBuf,
    sync::mpsc::channel,
};

use crate::collected_print::PrintSeq;

mod collected_print {
    use gc_arena::Collect;
    use piccolo::{
        meta_ops::{self, MetaResult},
        Context, Execution, Sequence, SequencePoll, Stack, Value,
    };
    use std::{
        io::{Cursor, Write},
        sync::mpsc::Sender,
    };

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
    #[collect(require_static)]
    enum Mode {
        Init,
        First,
        Rest,
    }

    #[derive(Collect)]
    #[collect(require_static)]
    struct Output(Sender<Vec<u8>>);

    #[derive(Collect)]
    #[collect(no_drop)]
    pub struct PrintSeq<'gc> {
        mode: Mode,
        values: Vec<Value<'gc>>,
        output: Output,
    }

    impl<'gc> PrintSeq<'gc> {
        pub fn new(values: impl Iterator<Item = Value<'gc>>, sender: Sender<Vec<u8>>) -> Self {
            Self {
                mode: Mode::Init,
                values: values.into_iter().collect(),
                output: Output(sender),
            }
        }
    }

    impl<'gc> Sequence<'gc> for PrintSeq<'gc> {
        fn poll(
            &mut self,
            ctx: Context<'gc>,
            _exec: Execution<'gc, '_>,
            mut stack: Stack<'gc, '_>,
        ) -> Result<piccolo::SequencePoll<'gc>, piccolo::Error<'gc>> {
            let mut buf = Cursor::new(Vec::new());

            if self.mode == Mode::Init {
                self.mode = Mode::First;
            } else {
                self.values.push(stack.get(0));
            }
            stack.clear();

            while let Some(value) = self.values.pop() {
                match meta_ops::tostring(ctx, value)? {
                    MetaResult::Value(v) => {
                        if self.mode == Mode::First {
                            self.mode = Mode::Rest;
                        } else {
                            buf.write_all(b"\t")?;
                        }
                        v.display(&mut buf)?;
                    }
                    MetaResult::Call(call) => {
                        stack.extend(call.args);
                        return Ok(SequencePoll::Call {
                            function: call.function,
                            is_tail: false,
                        });
                    }
                }
            }

            buf.write_all(b"\n")?;
            buf.flush()?;
            self.output.0.send(buf.into_inner())?;
            Ok(SequencePoll::Return)
        }
    }
}

#[derive(Debug)]
enum GoldenScriptMode {
    /// We expect a compilation error
    CompileError,
    /// We expect the script to compile
    Pass,
}

#[test]
fn test_goldenscripts() {
    const DIR: &str = "./tests/goldenscripts";
    let mut failed_scripts: Vec<PathBuf> = Vec::new();
    eprintln!("running all goldenscripts in {DIR:?}");

    let (tx, rx) = channel();

    'main: for dir in read_dir(DIR).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        // Open up the file, and read up to (and including) a --- line indicating the
        // start of the Lua section
        let mut file = BufReader::new(File::open(&path).unwrap());
        let mut result_type = String::new();
        file.read_line(&mut result_type).unwrap();
        let mode = match result_type.trim_start_matches("---").trim() {
            "error" => GoldenScriptMode::CompileError,
            "pass" => GoldenScriptMode::Pass,
            mode => {
                eprintln!("{path:?}: goldenscript did not specify an existing mode: {mode:?}");
                failed_scripts.push(path);
                continue;
            }
        };
        let mut expected_output = String::new();
        let mut line = String::new();
        while line.trim() != "---" {
            if let Err(_) = file.read_line(&mut line) {
                eprintln!("{path:?}: reached EOF w/o encountering end of prelude");
                failed_scripts.push(path);
                continue 'main;
            }
            if line.trim() != "---" {
                expected_output.extend(line.trim_start_matches("---").trim_start().chars());
                line.clear();
            }
        }
        eprintln!("{path:?}: operating in {mode:?} mode");
        // Do some tricks to reset file position to the "start of script"
        let script_start = file.seek(SeekFrom::Start(0)).unwrap();
        let mut file = file.into_inner();
        file.seek(SeekFrom::Start(script_start)).unwrap();
        let file = io::buffered_read(file).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                eprintln!("running {:?}", path);
                let mut lua = Lua::full();

                let tx = tx.clone();
                lua.enter(|ctx| {
                    ctx.set_global(
                        "print",
                        Callback::from_fn(
                            &ctx,
                            move |ctx: Context<'_>,
                                  _: Execution<'_, '_>,
                                  mut stack: Stack<'_, '_>| {
                                Ok(CallbackReturn::Sequence(BoxSequence::new(
                                    &ctx,
                                    PrintSeq::new(stack.drain(..).rev(), tx.clone()),
                                )))
                            },
                        ),
                    )
                    .unwrap();
                });

                let result = lua
                    .try_enter(|ctx| {
                        let closure =
                            Closure::load(ctx, Some(path.to_string_lossy().as_ref()), file)?;
                        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
                    })
                    .and_then(|executor| lua.execute::<()>(&executor));
                match mode {
                    GoldenScriptMode::CompileError => {
                        match result {
                            Ok(()) => {
                                eprintln!("{path:?}: expected script to fail to compile, but it succeeded");
                                failed_scripts.push(path);
                                continue;
                            }
                            Err(e) => {
                                if format!("{e:?}\n") != expected_output {
                                    eprintln!("{path:?}: did not match expected output\n\nexpected:\n{expected_output}\noutput:\n{e:?}");
                                    failed_scripts.push(path);
                                    continue;
                                }
                            }
                        };
                        eprintln!("{path:?}: succeeded");
                    }
                    GoldenScriptMode::Pass => {
                        match result {
                            Err(_) => {
                                eprintln!("{path:?}: expected script to compile, but failed");
                                failed_scripts.push(path);
                                continue;
                            }
                            Ok(()) => {
                                // Stitch together our expected byte output, and compare
                                let output: Vec<_> = rx.try_iter().flatten().collect();
                                if output != expected_output.as_bytes() {
                                    // Technically `output` is ASCII, but UTF8 is compatible
                                    eprintln!("{path:?}: did not match expected output\n\nexpected:\n{expected_output}\noutput:\n{}\n---\n{output:?}", String::from_utf8_lossy(&output));
                                    failed_scripts.push(path);
                                    continue;
                                }
                            }
                        };
                        eprintln!("{path:?}: succeeded");
                    }
                }
            }
        }
    }

    if failed_scripts.len() > 0 {
        panic!("Failed goldenscripts: {failed_scripts:?}");
    }
}
