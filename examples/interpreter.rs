use std::error::Error as StdError;
use std::fs::File;

use clap::{crate_description, crate_name, crate_version, Arg, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compiler::{ParseError, ParseErrorKind},
    io, meta_ops, AnyCallback, CallbackReturn, Closure, Executor, Function, FunctionProto, Lua,
    ProtoCompileError, StashedExecutor, StaticError,
};

fn run_code(lua: &mut Lua, executor: &StashedExecutor, code: &str) -> Result<(), StaticError> {
    lua.try_run(|ctx| {
        let closure = match Closure::load(ctx, ("return ".to_string() + code).as_bytes()) {
            Ok(closure) => closure,
            Err(_) => Closure::load(ctx, code.as_bytes())?,
        };
        let function = Function::compose(
            &ctx,
            [
                closure.into(),
                AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                    Ok(if stack.is_empty() {
                        CallbackReturn::Return
                    } else {
                        CallbackReturn::Call {
                            function: meta_ops::call(ctx, ctx.state.globals.get(ctx, "print"))?,
                            then: None,
                        }
                    })
                })
                .into(),
            ],
        );
        let executor = ctx.state.registry.fetch(executor);
        executor.restart(ctx, function, ());
        Ok(())
    })?;

    lua.execute::<()>(executor)
}

fn run_repl(lua: &mut Lua) -> Result<(), Box<dyn StdError>> {
    let mut editor = DefaultEditor::new()?;
    let executor = lua.run(|ctx| {
        let executor = Executor::new(ctx);
        ctx.state.registry.stash(&ctx, executor)
    });

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            let read = editor.readline(prompt)?;
            let read_empty = read.trim().is_empty();
            if !read_empty {
                if !line.is_empty() {
                    // Separate input lines in the input to the parser
                    line.push('\n');
                }
                line.push_str(&read);
            }

            match run_code(lua, &executor, &line) {
                Err(StaticError::Runtime(err))
                    if !read_empty
                        && matches!(
                            err.downcast::<ProtoCompileError>(),
                            Some(ProtoCompileError::Parser(ParseError {
                                kind: ParseErrorKind::EndOfStream { .. },
                                ..
                            }))
                        ) =>
                {
                    prompt = ">> ";
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

    let executor = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            FunctionProto::compile(ctx, file)?,
            Some(ctx.state.globals),
        )?;
        Ok(ctx
            .state
            .registry
            .stash(&ctx, Executor::start(ctx, closure.into(), ())))
    })?;

    lua.execute(&executor)?;

    if matches.contains_id("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
