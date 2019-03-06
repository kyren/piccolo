extern crate gc_arena;
extern crate luster;

use std::error::Error as StdError;
use std::fs::File;

use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

use luster::{compile, io, parser, FunctionProto, Lua, StaticError};

fn print_function_proto<'gc>(function: &FunctionProto<'gc>) {
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

fn main() -> Result<(), Box<StdError>> {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .author(crate_authors!(", "))
        .arg(
            Arg::with_name("parse")
                .short("p")
                .long("parse")
                .help("Parse file only and output AST"),
        )
        .arg(
            Arg::with_name("file")
                .required(true)
                .help("File to compile")
                .index(1),
        )
        .get_matches();

    let file = io::buffered_read(File::open(matches.value_of("file").unwrap())?)?;

    if matches.is_present("parse") {
        let chunk = parser::parse_chunk(file, |s| s.as_ref().to_vec().into_boxed_slice())?;
        println!("{:#?}", chunk);
    } else {
        let mut lua = Lua::new();
        lua.mutate(|mc, root| -> Result<(), StaticError> {
            let function = compile(mc, root.interned_strings, file).map_err(|e| e.to_static())?;
            print_function_proto(&function);
            Ok(())
        })?;
    }

    Ok(())
}
