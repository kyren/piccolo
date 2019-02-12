extern crate gc_arena;
extern crate luster;

use std::env;
use std::error::Error as StdError;
use std::fs::File;

use gc_sequence::{self as sequence, SequenceExt, SequenceResultExt};
use luster::{compile, io, Error, Lua};

fn main() -> Result<(), Box<StdError>> {
    let mut args = env::args();
    args.next();
    let file = io::buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua.sequence(|root| {
        sequence::from_fn_with(
            root.interned_strings,
            |mc, interned_strings| -> Result<(), Error> {
                let function = compile(mc, interned_strings, file)?;
                println!("output: {:#?}", function);
                Ok(())
            },
        )
        .map_err(|e| e.to_static())
        .boxed()
    })?;

    Ok(())
}
