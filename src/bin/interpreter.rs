extern crate failure;
extern crate luster;

use std::env;
use std::fs::File;

use failure::{err_msg, Error};

use luster::compiler::compile_chunk;
use luster::function::Closure;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::parser::parse_chunk;
use luster::sequence::{sequence_fn, SequenceExt};

fn main() -> Result<(), Error> {
    let mut args = env::args();
    args.next();
    let file = File::open(
        args.next()
            .ok_or_else(|| err_msg("no file argument given"))?,
    )?;
    let chunk = parse_chunk(buffered_read(file)?)?;

    let mut lua = Lua::new();
    lua.sequence(move |_, lc| {
        Box::new(
            sequence_fn(move |mc| Closure::new(mc, compile_chunk(mc, &chunk)?))
                .and_then_with(lc.main_thread, move |mc, main_thread, closure| {
                    Ok(main_thread.call_function(mc, closure, &[], 64))
                })
                .map(|_, r| {
                    println!("results: {:?}", r);
                    Ok(())
                }),
        )
    })?;

    Ok(())
}
