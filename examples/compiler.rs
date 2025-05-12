use std::{error::Error as StdError, fs::File, io::Read};

use clap::{crate_description, crate_name, crate_version, Arg, Command};

use piccolo::{
    compiler::{self, interning::BasicInterner, string_utils::debug_utf8_lossy, CompiledPrototype},
    io,
};

fn print_function<S: AsRef<[u8]>>(function: &CompiledPrototype<S>, depth: usize) {
    let indent = "  ".repeat(depth);
    println!("{indent}===FunctionProto({:p})===", function);
    println!(
        "{indent}fixed_params: {}, has_varargs: {}, stack_size: {}",
        function.fixed_params, function.has_varargs, function.stack_size
    );
    if function.constants.len() > 0 {
        println!("{indent}---constants---");
        for (i, c) in function.constants.iter().enumerate() {
            println!(
                "{indent}{}: {:?}",
                i,
                c.as_string_ref()
                    .map_string(|s| debug_utf8_lossy(s.as_ref()))
            );
        }
    }
    if function.opcodes.len() > 0 {
        println!("{indent}---opcodes---");

        let mut line_number_ind = 0;
        println!("{indent}<line {}>", function.opcode_line_numbers[0].1);

        for (i, c) in function.opcodes.iter().enumerate() {
            if let Some(&(opcode_index, line_number)) =
                function.opcode_line_numbers.get(line_number_ind + 1)
            {
                if i >= opcode_index {
                    line_number_ind += 1;
                    println!("{indent}<line {}>", line_number);
                }
            }
            println!("{indent}{}: {:?}", i, c);
        }
    }
    if function.upvalues.len() > 0 {
        println!("{indent}---upvalues---");
        for (i, u) in function.upvalues.iter().enumerate() {
            println!("{indent}{}: {:?}", i, u);
        }
    }
    if function.prototypes.len() > 0 {
        println!("{indent}---prototypes---");
        for p in &function.prototypes {
            print_function(p, depth + 1);
        }
    }
}

fn main() -> Result<(), Box<dyn StdError>> {
    let matches = Command::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
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

    let mut file = io::buffered_read(File::open(matches.get_one::<String>("file").unwrap())?)?;
    let mut source = Vec::new();
    file.read_to_end(&mut source)?;

    let mut interner = BasicInterner::default();

    if matches.contains_id("parse") {
        let chunk = compiler::parse_chunk(&source, &mut interner)?;
        println!("{:#?}", chunk);
    } else {
        let chunk = compiler::parse_chunk(&source, &mut interner)?;
        let prototype = compiler::compile_chunk(&chunk, &mut interner)?;
        print_function(&prototype, 0);
    }

    Ok(())
}
