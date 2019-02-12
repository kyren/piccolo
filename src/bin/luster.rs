use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use clap::{App, Arg};
use rustyline::Editor;

use gc_sequence::{self as sequence, SequenceExt, SequenceResultExt};
use luster::{
    compile, io, Closure, Error, Function, Lua, ParserError, StaticError, ThreadSequence,
};

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

            match lua.sequence(move |root| {
                sequence::from_fn_with(root, move |mc, root| {
                    let result = compile(mc, root.interned_strings, line_clone.as_bytes());
                    let result = match result {
                        Ok(res) => Ok(res),
                        err @ Err(Error::ParserError(ParserError::EndOfStream { expected: _ })) => {
                            err
                        }
                        Err(_) => compile(
                            mc,
                            root.interned_strings,
                            (String::new() + "return " + &line_clone).as_bytes(),
                        ),
                    };
                    Ok(Closure::new(mc, result?, Some(root.globals))?)
                })
                .and_chain_with(root, |mc, root, closure| {
                    Ok(ThreadSequence::call_function(
                        mc,
                        root.main_thread,
                        Function::Closure(closure),
                        &[],
                    )?)
                })
                .map(|values| match values {
                    Ok(values) => {
                        let output = values
                            .iter()
                            .map(|value| format!("{:?}", value))
                            .collect::<Vec<_>>()
                            .join("\t");
                        Ok(output)
                    }
                    Err(e) => Err(e.to_static()),
                })
                .boxed()
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

fn main() -> Result<(), Box<StdError>> {
    let matches = App::new("luster")
        .version("0.1.0")
        .about("A Lua interpreter")
        .arg(
            Arg::with_name("repl")
                .short("r")
                .long("repl")
                .help("Load into REPL after loading file, if any"),
        )
        .arg(Arg::with_name("file").help("File to interpret").index(1))
        .get_matches();

    let mut lua = Lua::new();

    if !matches.is_present("file") {
        run_repl(&mut lua);
        return Ok(());
    }

    let file = io::buffered_read(File::open(matches.value_of("file").unwrap())?)?;

    lua.sequence(|root| {
        sequence::from_fn_with(root, |mc, root| {
            Ok(Closure::new(
                mc,
                compile(mc, root.interned_strings, file)?,
                Some(root.globals),
            )?)
        })
        .and_chain_with(root, |mc, root, closure| {
            Ok(ThreadSequence::call_function(
                mc,
                root.main_thread,
                Function::Closure(closure),
                &[],
            )?)
        })
        .map_ok(|_| ())
        .map_err(|e| e.to_static())
        .boxed()
    })?;

    if matches.is_present("repl") {
        run_repl(&mut lua);
    }

    Ok(())
}
