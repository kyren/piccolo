use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compile, io, sequence, Closure, Error, Function, Lua, ParserError, SequenceExt, StaticError,
    ThreadSequence, TrySequenceExt,
};

fn run_repl(lua: &mut Lua) -> Result<(), Box<dyn StdError>> {
    let mut editor = DefaultEditor::new()?;

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            line.push_str(&editor.readline(prompt)?);

            let line_clone = line.clone();

            match lua.sequence(move |root| {
                sequence::from_fn_with(root, move |root, mc| {
                    let result = compile(mc, line_clone.as_bytes());
                    let result = match result {
                        Ok(res) => Ok(res),
                        err @ Err(Error::ParserError(ParserError::EndOfStream { expected: _ })) => {
                            err
                        }
                        Err(_) => compile(mc, (String::new() + "return " + &line_clone).as_bytes()),
                    };
                    Ok(Closure::new(mc, result?, Some(root.globals))?)
                })
                .and_chain_with(root, |root, mc, closure| {
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
                                editor.add_history_entry(line)?;
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
                    editor.add_history_entry(line)?;
                    println!("{}", out_string);
                    break;
                }
                Err(e) => {
                    editor.add_history_entry(line)?;
                    eprintln!("error: {}", e);
                    break;
                }
            }
        }
    }
}

fn main() -> Result<(), Box<dyn StdError>> {
    let matches = Command::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .author(crate_authors!(", "))
        .arg(
            Arg::new("repl")
                .short('r')
                .long("repl")
                .help("Load into REPL after loading file, if any"),
        )
        .arg(Arg::new("file").help("File to interpret").index(1))
        .get_matches();

    let mut lua = Lua::new();

    if !matches.contains_id("file") {
        run_repl(&mut lua)?;
        return Ok(());
    }

    let file = io::buffered_read(File::open(matches.get_one::<String>("file").unwrap())?)?;

    lua.sequence(|root| {
        sequence::from_fn_with(root, |root, mc| {
            Ok(Closure::new(mc, compile(mc, file)?, Some(root.globals))?)
        })
        .and_chain_with(root, |root, mc, closure| {
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

    if matches.contains_id("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
