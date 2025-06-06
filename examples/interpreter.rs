use std::fs::File;
use std::{error::Error as StdError, io::Read};

use clap::{crate_description, crate_name, crate_version, Arg, ArgAction, Command};
use rustyline::DefaultEditor;

use piccolo::{
    compiler::{ParseError, ParseErrorKind},
    io, meta_ops, Callback, CallbackReturn, Closure, Executor, ExternError, Function, Lua,
    StashedExecutor,
};

fn run_code(lua: &mut Lua, executor: &StashedExecutor, code: &str) -> Result<(), ExternError> {
    lua.try_enter(|ctx| {
        let closure = match Closure::load(ctx, None, ("return ".to_owned() + code).as_bytes()) {
            Ok(closure) => closure,
            Err(_) => Closure::load(ctx, None, code.as_bytes())?,
        };
        let function = Function::compose(
            &ctx,
            [
                closure.into(),
                Callback::from_fn(&ctx, |ctx, _, stack| {
                    Ok(if stack.is_empty() {
                        CallbackReturn::Return
                    } else {
                        CallbackReturn::Call {
                            function: meta_ops::call(ctx, ctx.get_global_value("print"))?,
                            then: None,
                        }
                    })
                })
                .into(),
            ],
        );
        ctx.fetch(executor).restart(ctx, function, ());
        Ok(())
    })?;

    lua.execute::<()>(executor)
}

fn run_repl(lua: &mut Lua) -> Result<(), Box<dyn StdError>> {
    let mut editor = DefaultEditor::new()?;
    let executor = lua.enter(|ctx| ctx.stash(Executor::new(ctx)));

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
                Err(err)
                    if !read_empty
                        && matches!(
                            err.root_cause().downcast_ref::<ParseError>(),
                            Some(ParseError {
                                kind: ParseErrorKind::EndOfStream { .. },
                                ..
                            })
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
                .help("Load into REPL after loading file, if any")
                .action(ArgAction::SetTrue),
        )
        .arg(Arg::new("file").help("File to interpret").index(1))
        .get_matches();

    let mut lua = Lua::full();

    if !matches.contains_id("file") {
        run_repl(&mut lua)?;
        return Ok(());
    }

    let file_name = matches.get_one::<String>("file").unwrap();
    let mut file = io::buffered_read(File::open(file_name)?)?;
    let mut source = Vec::new();
    file.read_to_end(&mut source)?;

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(ctx, Some(file_name.as_str()), &source)?;
        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.execute::<()>(&executor)?;

    if matches.get_flag("repl") {
        run_repl(&mut lua)?;
    }

    Ok(())
}
