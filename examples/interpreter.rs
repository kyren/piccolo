use std::error::Error as StdError;
use std::fs::File;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compiler::ParserError, io, meta_ops, AnyCallback, CallbackReturn, Closure, Function,
    FunctionProto, Lua, ProtoCompileError, StaticError, Thread,
};

fn run_code(lua: &mut Lua, code: &str) -> Result<(), StaticError> {
    let thread = lua.try_run(|ctx| {
        let closure = match Closure::load(ctx, ("return ".to_string() + code).as_bytes()) {
            Ok(closure) => closure,
            Err(err) => {
                if let Ok(closure) = Closure::load(ctx, code.as_bytes()) {
                    closure
                } else {
                    return Err(err.into());
                }
            }
        };
        let function = Function::compose(
            &ctx,
            [
                closure.into(),
                AnyCallback::from_fn(&ctx, |ctx, stack| {
                    Ok(if stack.is_empty() {
                        CallbackReturn::Return
                    } else {
                        CallbackReturn::TailCall(
                            meta_ops::call(ctx, ctx.state.globals.get(ctx, "print"))?,
                            None,
                        )
                    })
                })
                .into(),
            ],
        );
        let thread = Thread::new(&ctx);
        thread.start(ctx, function, ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);

    lua.try_run(|ctx| Ok(ctx.state.registry.fetch(&thread).take_return::<()>(ctx)??))
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
                Err(e) => {
                    editor.add_history_entry(line)?;
                    eprintln!("{}", e);
                    break;
                }
                Ok(()) => {
                    editor.add_history_entry(line)?;
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
