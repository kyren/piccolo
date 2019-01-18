extern crate gc_arena;
extern crate luster;

use std::env;
use std::error::Error as StdError;
use std::fs::File;

use luster::compiler::compile;
use luster::error::Error;
use luster::gen_sequence;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::sequence::sequence_fn;

fn main() -> Result<(), Box<StdError>> {
    let mut args = env::args();
    args.next();
    let file = buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let mut lua = Lua::new();
    lua.sequence(gen_sequence!(sequence_fn(
        move |mc, lc| -> Result<(), Error> {
            let function = compile(mc, lc.interned_strings, file)?;
            println!("output: {:#?}", function);
            Ok(())
        }
    )))?;

    Ok(())
}
