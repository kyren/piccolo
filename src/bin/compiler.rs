extern crate failure;
extern crate gc_arena;
extern crate luster;

use std::env;
use std::fs::File;

use failure::{err_msg, Error};

use gc_arena::rootless_arena;

use luster::code::compile_chunk;
use luster::parser::parse_chunk;

fn main() -> Result<(), Error> {
    let mut args = env::args();
    args.next();
    let file = File::open(args.next()
        .ok_or_else(|| err_msg("no file argument given"))?)?;

    rootless_arena(|mc| -> Result<(), Error> {
        let chunk = parse_chunk(file)?;
        let function = compile_chunk(mc, &chunk)?;
        println!("output: {:?}", function);
        Ok(())
    })?;

    Ok(())
}
