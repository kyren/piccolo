extern crate gc_arena;
extern crate luster;

use std::env;
use std::error::Error as StdError;
use std::fs::File;

use luster::{compile, io, sequence_fn, Error, Lua, SequenceExt};

fn main() -> Result<(), Box<StdError>> {
    let mut args = env::args();
    args.next();
    let file = io::buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua.sequence(|_| {
        sequence_fn(move |mc, lc| -> Result<(), Error> {
            let function = compile(mc, lc.interned_strings, file)?;
            println!("output: {:#?}", function);
            Ok(())
        })
        .map_err(|e| e.to_static())
        .boxed()
    })?;

    Ok(())
}
