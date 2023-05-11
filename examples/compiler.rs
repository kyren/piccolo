use std::{error::Error as StdError, fs::File};

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};

use piccolo::{
    compiler::{self, interning::BasicInterner},
    io, CompiledPrototype,
};

fn print_function<S: AsRef<[u8]>>(function: &CompiledPrototype<S>) {
    println!("=============");
    println!("FunctionProto({:p})", function);
    println!("=============");
    println!(
        "fixed_params: {}, has_varargs: {}, stack_size: {}",
        function.fixed_params, function.has_varargs, function.stack_size
    );
    if function.constants.len() > 0 {
        println!("constants:");
        for (i, c) in function.constants.iter().enumerate() {
            println!(
                "{}: {:?}",
                i,
                c.map_string(|s| String::from_utf8_lossy(s.as_ref()).into_owned())
            );
        }
    }
    if function.opcodes.len() > 0 {
        println!("opcodes:");
        for (i, c) in function.opcodes.iter().enumerate() {
            println!("{}: {:?}", i, c);
        }
    }
    if function.upvalues.len() > 0 {
        println!("upvalues:");
        for (i, u) in function.upvalues.iter().enumerate() {
            println!("{}: {:?}", i, u);
        }
    }
    if function.prototypes.len() > 0 {
        println!("prototypes:");
        for p in &function.prototypes {
            print_function(p);
        }
    }
}

fn main() -> Result<(), Box<dyn StdError>> {
    let matches = Command::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .author(crate_authors!(", "))
        .arg(
            Arg::new("parse")
                .short('p')
                .long("parse")
                .help("Parse file only and output AST"),
        )
        .arg(
            Arg::new("file")
                .required(true)
                .help("File to compile")
                .index(1),
        )
        .get_matches();

    let file = io::buffered_read(File::open(matches.get_one::<String>("file").unwrap())?)?;

    let interner = BasicInterner::default();

    if matches.contains_id("parse") {
        let chunk = compiler::parse_chunk(file, &interner)?;
        println!("{:#?}", chunk);
    } else {
        let chunk = compiler::parse_chunk(file, &interner)?;
        let prototype = compiler::compile_chunk(&chunk, &interner)?;
        print_function(&prototype);
    }

    Ok(())
}
