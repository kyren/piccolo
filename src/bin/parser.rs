extern crate failure;
extern crate gc_arena;
extern crate luster;

use std::env;
use std::fs::File;

use failure::{err_msg, Error};

use luster::io::buffered_read;
use luster::parser::parse_chunk;

fn main() -> Result<(), Error> {
    let mut args = env::args();
    args.next();
    let file = buffered_read(File::open(
        args.next()
            .ok_or_else(|| err_msg("no file argument given"))?,
    )?)?;

    let chunk = parse_chunk(file, |s| s.as_ref().to_vec().into_boxed_slice())?;
    println!("output: {:#?}", chunk);

    Ok(())
}
