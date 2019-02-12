extern crate gc_arena;
extern crate luster;

use std::env;
use std::error::Error as StdError;
use std::fs::File;

use luster::{compile, io, Lua, StaticError};

fn main() -> Result<(), Box<StdError>> {
    let mut args = env::args();
    args.next();
    let file = io::buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua.mutate(|mc, root| -> Result<(), StaticError> {
        let function = compile(mc, root.interned_strings, file).map_err(|e| e.to_static())?;
        println!("output: {:#?}", function);
        Ok(())
    })?;

    Ok(())
}
