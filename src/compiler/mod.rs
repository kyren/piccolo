use std::io::Read;

use failure::Error;

use gc_arena::MutationContext;

use crate::function::FunctionProto;
use crate::parser::parse_chunk;
use crate::string::InternedStringSet;

mod compiler;
mod constant;
mod operators;
mod register_allocator;

pub use self::compiler::{compile_chunk, CompilerError};

pub fn compile<'gc, R: Read>(
    mc: MutationContext<'gc, '_>,
    interned_strings: InternedStringSet<'gc>,
    source: R,
) -> Result<FunctionProto<'gc>, Error> {
    Ok(compile_chunk(
        mc,
        interned_strings,
        &parse_chunk(source, |s| interned_strings.new_string(mc, s))?,
    )?)
}
