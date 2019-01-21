use std::env;
use std::error::Error;
use std::fs::File;

use luster::compiler::compile;
use luster::function::Closure;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::lua_sequence;
use luster::sequence::{sequence_fn, SequenceExt};

fn main() -> Result<(), Box<Error>> {
    let mut args = env::args();
    args.next();
    let file = buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua_sequence!(
        lua,
        sequence_fn(|mc, lc| Ok(Closure::new(
            mc,
            compile(mc, lc.interned_strings, file)?,
            Some(lc.globals),
        )?))
        .and_then(|mc, lc, closure| lc.main_thread.call_function(mc, closure, &[], 64))
        .map(|r| {
            println!("results: {:?}", r);
        })
    )?;

    Ok(())
}
