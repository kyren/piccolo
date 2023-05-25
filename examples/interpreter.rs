use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compile, compiler::ParserError, conversion::Variadic, io, Closure, Lua, StaticError, Value,
};

fn run_code(lua: &mut Lua, code: &str) -> Result<String, StaticError> {
    lua.try_run(|mc, state| {
        let result = compile(mc, state.strings, ("return ".to_string() + code).as_bytes());
        let result = match result {
            Ok(res) => Ok(res),
            Err(_) => compile(mc, state.strings, code.as_bytes()),
        };
        let closure = Closure::new(mc, result?, Some(state.globals))?;

        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.finish_main_thread();

    lua.try_run(|mc, state| {
        Ok(state
            .main_thread
            .take_return::<Variadic<Value>>(mc)??
            .iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>()
            .join("\t"))
    })
}

fn run_repl(lua: &mut Lua) -> Result<(), Box<dyn StdError>> {
    let mut editor = DefaultEditor::new()?;

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            line.push_str(&editor.readline(prompt)?);

            match run_code(lua, &line) {
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

    lua.try_run(|mc, state| {
        let closure = Closure::new(mc, compile(mc, state.strings, file)?, Some(state.globals))?;

        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.run_main_thread()?;

    if matches.contains_id("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
