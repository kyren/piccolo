mod compiler;
pub mod lexer;
mod operators;
pub mod parser;
mod register_allocator;

use std::io::Read;

use gc_arena::MutationContext;

use crate::{string::InternedStringSet, Error, FunctionProto, String};

pub use self::{
    compiler::{compile_chunk, CompiledFunction, CompilerError},
    parser::parse_chunk,
    parser::ParserError,
};

pub trait StringInterner {
    type String: AsRef<[u8]> + Clone;

    fn intern(&self, s: &[u8]) -> Self::String;
}

pub fn compile<'gc, R: Read>(
    mc: MutationContext<'gc, '_>,
    source: R,
) -> Result<FunctionProto<'gc>, Error<'gc>> {
    #[derive(Copy, Clone)]
    struct ISS<'gc, 'a> {
        string_set: InternedStringSet<'gc>,
        mc: MutationContext<'gc, 'a>,
    }

    impl<'gc, 'a> StringInterner for ISS<'gc, 'a> {
        type String = String<'gc>;

        fn intern(&self, s: &[u8]) -> Self::String {
            self.string_set.new_string(self.mc, s)
        }
    }

    let interner = ISS {
        string_set: InternedStringSet::new(mc),
        mc,
    };

    let chunk = parse_chunk(source, interner)?;
    let compiled_function = compile_chunk(&chunk, interner)?;

    Ok(FunctionProto::from_compiled(mc, compiled_function))
}
