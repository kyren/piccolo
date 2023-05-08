use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use deimos::{compile, io, Closure, Function, Lua, ParserError, StaticError};

fn run_code(lua: &mut Lua, code: &str) -> Result<String, StaticError> {
    let function = lua.try_run(|mc, root| {
        let result = compile(mc, ("return ".to_string() + code).as_bytes());
        let result = match result {
            Ok(res) => Ok(res),
            Err(_) => compile(mc, code.as_bytes()),
        };
        let closure = Closure::new(mc, result?, Some(root.globals))?;

        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    let res = lua.run_function(&function, &[])?;

    lua.try_run(|_, root| {
        Ok(res
            .iter()
            .map(|v| root.registry.fetch(v))
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

    let function = lua.try_run(|mc, root| {
        Ok(root.registry.stash(
            mc,
            Function::Closure(Closure::new(mc, compile(mc, file)?, Some(root.globals))?),
        ))
    })?;

    lua.run_function(&function, &[])?;

    if matches.contains_id("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
