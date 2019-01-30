use std::io::Read;

use gc_arena::MutationContext;

use crate::{parse_chunk, Error, FunctionProto, InternedStringSet};

mod compiler;
mod operators;
mod register_allocator;

pub use self::compiler::{compile_chunk, CompilerError};

pub fn compile<'gc, R: Read>(
    mc: MutationContext<'gc, '_>,
    interned_strings: InternedStringSet<'gc>,
    source: R,
) -> Result<FunctionProto<'gc>, Error<'gc>> {
    Ok(compile_chunk(
        mc,
        &parse_chunk(source, |s| interned_strings.new_string(mc, s))?,
    )?)
}
