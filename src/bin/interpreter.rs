extern crate failure;
extern crate luster;

use std::env;
use std::fs::File;

use failure::{err_msg, Error};

use luster::compiler::compile;
use luster::function::Closure;
use luster::gen_sequence;
use luster::io::buffered_read;
use luster::lua::Lua;
use luster::sequence::{sequence_fn, SequenceExt};

fn main() -> Result<(), Error> {
    let mut args = env::args();
    args.next();
    let file = buffered_read(File::open(
        args.next()
            .ok_or_else(|| err_msg("no file argument given"))?,
    )?)?;

    let mut lua = Lua::new();
    lua.sequence(gen_sequence!(sequence_fn(move |mc, lc| Ok(lc
        .main_thread
        .call_function(
            mc,
            Closure::new(
                mc,
                compile(mc, lc.interned_strings, file)?,
                Some(lc.globals),
            )?,
            &[],
            64,
        )
        .and_then(|_, _, r| {
            println!("results: {:?}", r);
            Ok(())
        })))
    .flatten()))?;

    Ok(())
}
