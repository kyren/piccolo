//! These tests test the output of errors and the standard output of scripts.
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

use piccolo::{Closure, Executor, Lua};
use std::{fs::read_dir, io::BufRead, path::PathBuf, sync::mpsc::channel};

use crate::collected_print::print_callback;

mod collected_print {
    use gc_arena::Collect;
    use piccolo::{
        meta_ops::{self, MetaResult},
        BoxSequence, Callback, CallbackReturn, Context, Execution, Sequence, SequencePoll, Stack,
        Value,
    };
    use std::{
        io::{Cursor, Write},
        pin::Pin,
        sync::mpsc::Sender,
    };

    pub fn print_callback<'gc>(ctx: piccolo::Context<'gc>, tx: Sender<Vec<u8>>) -> Callback<'gc> {
        Callback::from_fn(
            &ctx,
            move |ctx: Context<'_>, _: Execution<'_, '_>, mut stack: Stack<'_, '_>| {
                stack[..].reverse();

                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    PrintSeq {
                        first: true,
                        buf: Cursor::new(Vec::new()),
                        output: tx.clone(),
                    },
                )))
            },
        )
    }

    #[derive(Collect)]
    #[collect(require_static)]
    struct PrintSeq {
        first: bool,
        buf: Cursor<Vec<u8>>,
        output: Sender<Vec<u8>>,
    }

    impl<'gc> Sequence<'gc> for PrintSeq {
        fn poll(
            mut self: Pin<&mut Self>,
            ctx: Context<'gc>,
            _exec: Execution<'gc, '_>,
            mut stack: Stack<'gc, '_>,
        ) -> Result<SequencePoll<'gc>, piccolo::Error<'gc>> {
            while let Some(value) = stack.pop_back() {
                match meta_ops::tostring(ctx, value)? {
                    MetaResult::Value(v) => {
                        if self.first {
                            self.first = false;
                        } else {
                            self.buf.write_all(b"\t")?;
                        }
                        if let Value::String(s) = v {
                            self.buf.write_all(s.as_bytes())?;
                        } else {
                            write!(self.buf, "{}", v.display())?;
                        }
                    }
                    MetaResult::Call(call) => {
                        let bottom = stack.len();
                        stack.extend(call.args);
                        return Ok(SequencePoll::Call {
                            function: call.function,
                            bottom,
                        });
                    }
                }
            }

            self.buf.write_all(b"\n")?;
            self.buf.flush()?;
            let buf = std::mem::take(&mut self.buf).into_inner();
            self.output.send(buf)?;
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

    let files = read_dir(DIR)
        .and_then(|e| e.collect::<Result<Vec<_>, _>>())
        .expect("could not list dir contents");

    let mut files = files
        .into_iter()
        .map(|e| e.path())
        .filter(|p| p.extension().map(|s| s == "lua").unwrap_or(false))
        .collect::<Vec<_>>();

    files.sort();

    'main: for path in files {
        // Read the first line of the file to find the mode, specified like
        // "--- mode"
        let source = std::fs::read(&path).unwrap();

        let mut cursor = std::io::Cursor::new(&source);

        let mut result_type = String::new();
        cursor.read_line(&mut result_type).unwrap();
        let mode = match result_type.strip_prefix("---").map(|s| s.trim()) {
            Some("error") => GoldenScriptMode::CompileError,
            Some("pass") => GoldenScriptMode::Pass,
            mode => {
                eprintln!("{path:?}: goldenscript has unknown mode {mode:?}");
                failed_scripts.push(path);
                continue;
            }
        };

        let mut expected_output = String::new();
        let mut line = String::new();
        loop {
            if let Err(_) = cursor.read_line(&mut line) {
                eprintln!("{path:?}: reached EOF w/o encountering end of prelude");
                failed_scripts.push(path);
                continue 'main;
            }
            if let Some(str) = line
                .strip_prefix("--- ")
                .or_else(|| line.strip_prefix("---"))
            {
                expected_output.push_str(str);
                line.clear();
            } else {
                break;
            }
        }

        eprintln!("{path:?}: operating in {mode:?} mode");
        eprintln!("running {:?}", path);

        let mut lua = Lua::full();

        let tx = tx.clone();
        lua.enter(|ctx| {
            ctx.set_global("print", print_callback(ctx, tx.clone()));
        });

        let compile_result = lua.try_enter(|ctx| {
            let closure = Closure::load(ctx, Some(path.to_string_lossy().as_ref()), &source)?;
            Ok(ctx.stash(closure))
        });
        let (closure, compile_error) = match compile_result {
            Ok(o) => (Some(o), None),
            Err(e) => (None, Some(e)),
        };
        let run_result = closure.map(|closure| {
            lua.try_enter(|ctx| {
                let closure = ctx.fetch(&closure);
                Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
            })
            .and_then(|executor| lua.execute::<()>(&executor))
        });
        let run_error = run_result.map(|r| r.err()).flatten();

        match mode {
            GoldenScriptMode::CompileError => {
                if let Some(error) = compile_error {
                    let formatted_error = format!("{error}\n");
                    if formatted_error != expected_output {
                        eprintln!("{path:?}: did not match expected output\n\nexpected:\n{expected_output}\noutput:\n{formatted_error}");
                        failed_scripts.push(path);
                        continue;
                    }
                } else {
                    eprintln!("{path:?}: expected script to fail to compile, but it succeeded");
                    failed_scripts.push(path);
                    continue;
                }
                eprintln!("{path:?}: succeeded");
            }
            GoldenScriptMode::Pass => {
                if let Some(error) = compile_error {
                    eprintln!(
                        "{path:?}: expected script to compile, but it failed\nerror: {error}"
                    );
                    failed_scripts.push(path);
                    continue;
                }
                if let Some(error) = run_error {
                    eprintln!("{path:?}: expected script to pass, but it threw an error at runtime\nerror: {error:#}");
                    failed_scripts.push(path);
                    continue;
                }
                // Stitch together our expected byte output, and compare
                let output: Vec<_> = rx.try_iter().flatten().collect();
                if output != expected_output.as_bytes() {
                    // Technically `output` is ASCII, but UTF8 is compatible
                    eprintln!("{path:?}: did not match expected output\n\nexpected:\n{expected_output}\noutput:\n{}\n---\n{output:?}", String::from_utf8_lossy(&output));
                    failed_scripts.push(path);
                    continue;
                }
                eprintln!("{path:?}: succeeded");
            }
        }
    }

    if failed_scripts.len() > 0 {
        panic!("Failed goldenscripts: {failed_scripts:?}");
    }
}
