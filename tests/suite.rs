extern crate failure;
extern crate luster;

use std::fs::{read_dir, File};
use std::io::{stdout, Write};
use std::path::PathBuf;

use failure::Error;

use luster::io::buffered_read;
use luster::parser::parse_chunk;

#[test]
fn test_suite() {
    // Right now, we just check that all the files in the testsuite directory parse correctly.

    let mut file_failed = false;

    // We write to stdout directly as a hack to allow printing without capture from `cargo test`.
    let mut stdout = stdout();

    for dir in read_dir("./testsuite").expect("could not list 'testsuite' dir contents") {
        let path = dir.expect("could not read 'testsuite' dir entry").path();
        if let Some(ext) = path.extension() {
            if ext == "lua" {
                writeln!(stdout, "parsing file {:?}", path);
                if let Err(err) = parse_file(&path) {
                    writeln!(stdout, "error encountered: {:?}", err);
                    file_failed = true;
                }
            }
        } else {
            writeln!(stdout, "skipping file {:?}", path);
        }
    }

    if file_failed {
        panic!("one or more testsuite errors occurred");
    }
}

fn parse_file(path: &PathBuf) -> Result<(), Error> {
    parse_chunk(buffered_read(File::open(path)?)?)?;
    Ok(())
}
