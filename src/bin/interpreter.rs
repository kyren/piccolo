use std::env;
use std::error::Error;
use std::fs::File;

use luster::{compile, io, sequence_fn, Closure, Lua, SequenceExt};

fn main() -> Result<(), Box<Error>> {
    let mut args = env::args();
    args.next();
    let file = io::buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua.sequence(|_| {
        Box::new(
            sequence_fn(|mc, lc| {
                Ok(Closure::new(
                    mc,
                    compile(mc, lc.interned_strings, file)?,
                    Some(lc.globals),
                )?)
            })
            .and_then(|mc, lc, closure| lc.main_thread.call_closure(mc, closure, &[], 64))
            .map(|_| ()),
        )
    })?;

    Ok(())
}
