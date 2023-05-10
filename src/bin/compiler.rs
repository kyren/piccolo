extern crate deimos;
extern crate gc_arena;

use std::{error::Error as StdError, fs::File, path::PathBuf};

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};

use deimos::{
    compile,
    compiler::{self, StringInterner},
    io, FunctionProto, Lua,
};

fn print_function_proto<'gc>(function: &FunctionProto<'gc>) {
    println!("=============");
    println!("FunctionProto({:p})", function);
    println!("=============");
    println!(
        "fixed_params: {}, stack_size: {}",
        function.fixed_params, function.stack_size
    );
    if function.constants.len() > 0 {
        println!("constants:");
        for (i, c) in function.constants.iter().enumerate() {
            println!("{}: {:?}", i, c);
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
            print_function_proto(p);
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

    let file = io::buffered_read(File::open(matches.get_one::<PathBuf>("file").unwrap())?)?;

    if matches.contains_id("parse") {
        struct Interner;

        impl StringInterner for Interner {
            type String = Box<[u8]>;

            fn intern(&self, s: &[u8]) -> Self::String {
                Box::from(s)
            }
        }

        let chunk = compiler::parse_chunk(file, Interner)?;
        println!("{:#?}", chunk);
    } else {
        let mut lua = Lua::new();
        lua.try_run(|mc, _| {
            let function = compile(mc, file)?;
            print_function_proto(&function);
            Ok(())
        })?;
    }

    Ok(())
}
