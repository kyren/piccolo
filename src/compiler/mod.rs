mod compiler;
pub mod interning;
pub mod lexer;
mod operators;
pub mod parser;
mod register_allocator;

use std::io::Read;

use gc_arena::Mutation;

use crate::{string::InternedStringSet, Error, FunctionProto, String};

pub use self::{
    compiler::{compile_chunk, CompiledPrototype, CompilerError},
    interning::StringInterner,
    parser::parse_chunk,
    parser::ParserError,
};

pub fn compile<'gc, R: Read>(
    mc: &Mutation<'gc>,
    strings: InternedStringSet<'gc>,
    source: R,
) -> Result<FunctionProto<'gc>, Error<'gc>> {
    #[derive(Copy, Clone)]
    struct Interner<'gc, 'a> {
        strings: InternedStringSet<'gc>,
        mc: &'a Mutation<'gc>,
    }

    impl<'gc, 'a> StringInterner for Interner<'gc, 'a> {
        type String = String<'gc>;

        fn intern(&self, s: &[u8]) -> Self::String {
            self.strings.intern(self.mc, s)
        }
    }

    let interner = Interner { strings, mc };

    let chunk = parse_chunk(source, interner)?;
    let compiled_function = compile_chunk(&chunk, interner)?;

    Ok(FunctionProto::from_compiled(mc, compiled_function))
}
