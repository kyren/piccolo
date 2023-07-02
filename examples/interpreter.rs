use std::error::Error as StdError;
use std::fs::File;
use std::vec::Vec;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compiler::ParserError, io, Closure, FunctionProto, Lua, ProtoCompileError, StaticError, Thread,
    Value, Variadic,
};

fn run_code(lua: &mut Lua, code: &str) -> Result<String, StaticError> {
    let thread = lua.try_run(|ctx| {
        let result = Closure::load(ctx, ("return ".to_string() + code).as_bytes());
        let result = match result {
            Ok(res) => Ok(res),
            Err(_) => Closure::load(ctx, code.as_bytes()),
        };
        let closure = result?;
        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);

    lua.try_run(|ctx| {
        Ok(ctx
            .state
            .registry
            .fetch(&thread)
            .take_return::<Variadic<Value>>(ctx)??
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
                Err(StaticError::Runtime(err))
                    if matches!(
                        err.downcast::<ProtoCompileError>(),
                        Some(ProtoCompileError::Parser(ParserError::EndOfStream { .. }))
                    ) =>
                {
                    match line.chars().last() {
                        Some(c) => {
                            if c == '\n' {
                                editor.add_history_entry(line)?;
                                eprintln!("{}", StaticError::from(err));
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
                    eprintln!("{}", e);
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

    let mut lua = Lua::full();

    if !matches.contains_id("file") {
        run_repl(&mut lua)?;
        return Ok(());
    }

    let file = io::buffered_read(File::open(matches.get_one::<String>("file").unwrap())?)?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            FunctionProto::compile(ctx, file)?,
            Some(ctx.state.globals),
        )?;
        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.run_thread(&thread)?;

    if matches.contains_id("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
