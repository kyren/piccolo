mod compiler;
mod operators;
mod register_allocator;

use std::io::Read;

use gc_arena::MutationContext;

use crate::{parse_chunk, string::InternedStringSet, Error, FunctionProto};

pub use self::compiler::{compile_chunk, CompilerError};

pub fn compile<'gc, R: Read>(
    mc: MutationContext<'gc, '_>,
    source: R,
) -> Result<FunctionProto<'gc>, Error<'gc>> {
    let interner = InternedStringSet::new(mc);
    Ok(compile_chunk(
        mc,
        &parse_chunk(source, |s| interner.new_string(mc, s))?,
    )?)
}
