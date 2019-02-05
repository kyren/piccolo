use std::env;
use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use luster::{
    compile, io, sequence_fn, Closure, Error, Function, Lua, ParserError, SequenceExt, StaticError,
    ThreadSequence
};
use rustyline::Editor;
use getopts::Options;

fn run_repl(lua: &mut Lua) {
    let mut editor = Editor::<()>::new();

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            match editor.readline(prompt) {
                Ok(input) => line.push_str(&input),
                Err(_) => return,
            }

            let line_clone = line.clone();

            match lua.sequence(move |_| {
                Box::new(
                    sequence_fn(move |mc, lc| {
                        let result = compile(mc, lc.interned_strings, line_clone.as_bytes());
                        let result = match result {
                            Ok(res) => Ok(res),
                            err @ Err(Error::ParserError(ParserError::EndOfStream { expected: _ })) => err,
                            Err(_) => compile(
                                mc,
                                lc.interned_strings,
                                (String::new() + "return " + &line_clone).as_bytes(),
                            ),
                        };
                        Ok(Closure::new(mc, result?, Some(lc.globals))?)
                    })
                    .and_then(|mc, lc, closure| {
                        Ok(ThreadSequence::call_function(
                            mc,
                            lc.main_thread,
                            Function::Closure(closure),
                            &[],
                        )?)
                    })
                    .flatten()
                    .map_result(|values| match values {
                        Ok(values) => {
                            let output = values
                                .iter()
                                .map(|value| format!("{:?}", value))
                                .collect::<Vec<_>>()
                                .join("\t");
                            Ok(output)
                        }
                        Err(e) => Err(e.to_static()),
                    }),
                )
            }) {
                err @ Err(StaticError::ParserError(ParserError::EndOfStream { expected: _ })) => {
                    match line.chars().last() {
                        Some(c) => {
                            if c == '\n' {
                                editor.add_history_entry(line);
                                eprintln!("error: {}", err.err().unwrap());
                                break;
                            }
                            prompt = ">> ";
                            line.push_str("\n"); // separate input lines
                        }
                        _ => {}
                    }
                }
                Ok(out_string) => {
                    editor.add_history_entry(line);
                    println!("{}", out_string);
                    break;
                }
                Err(e) => {
                    editor.add_history_entry(line);
                    eprintln!("error: {}", e);
                    break;
                }
            }
        }
    }
}

fn show_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options] [FILE]", program);
    print!("{}", opts.usage(&brief));
}

fn show_version() {
    print!("luster v0.1.0");
}

fn main() -> Result<(), Box<StdError>> {
    let args : Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("f", "file", "File name to interpret, (can also use free argument)", "FILE");
    opts.optflag("r", "repl", "Load into REPL after loading file information, if any");
    opts.optflag("h", "help", "Show help text");
    opts.optflag("v", "version", "Show version information");
    
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(_) => {
            show_usage(&program, opts); 
            return Ok(())
        }
    };

    if matches.opt_present("h") {
        show_usage(&program, opts);
        return Ok(())
    }

    if matches.opt_present("v") {
        show_version();
        return Ok(())
    }

    let mut lua = Lua::new();

    let filename = match matches.opt_str("f") {
        Some(f) => f,
        None => if !matches.free.is_empty() {
                matches.free[0].clone()
            } else {
                run_repl(&mut lua);
                return Ok(())
            }
    };

    let file = io::buffered_read(File::open(filename)?)?;

    lua.sequence(|_| {
        Box::new(
            sequence_fn(|mc, lc| {
                Ok(Closure::new(
                    mc,
                    compile(mc, lc.interned_strings, file)?,
                    Some(lc.globals),
                )?)
            })
            .and_then(|mc, lc, closure| {
                Ok(ThreadSequence::call_function(
                    mc,
                    lc.main_thread,
                    Function::Closure(closure),
                    &[],
                )?)
            })
            .flatten()
            .map(|_| ())
            .map_err(|e| e.to_static()),
        )
    })?;

    if matches.opt_present("r") {
        run_repl(&mut lua)
    }

    Ok(())
}
