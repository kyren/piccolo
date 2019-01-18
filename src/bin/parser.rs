extern crate gc_arena;
extern crate luster;

use std::env;
use std::error::Error;
use std::fs::File;

use luster::io::buffered_read;
use luster::parser::parse_chunk;

fn main() -> Result<(), Box<Error>> {
    let mut args = env::args();
    args.next();
    let file = buffered_read(File::open(
        args.next().ok_or_else(|| "no file argument given")?,
    )?)?;

    let chunk = parse_chunk(file, |s| s.as_ref().to_vec().into_boxed_slice())?;
    println!("output: {:#?}", chunk);

    Ok(())
}
