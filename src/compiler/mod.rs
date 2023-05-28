mod compiler;
pub mod interning;
pub mod lexer;
mod operators;
pub mod parser;
mod register_allocator;

use std::io::Read;

use thiserror::Error;

use crate::{Context, FunctionProto, String};

pub use self::{
    compiler::{compile_chunk, CompilationError, CompiledPrototype},
    interning::StringInterner,
    parser::parse_chunk,
    parser::ParserError,
};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Parsing(#[from] ParserError),
    #[error(transparent)]
    Compilation(#[from] CompilationError),
}

pub fn compile<'gc, R: Read>(
    ctx: Context<'gc>,
    source: R,
) -> Result<FunctionProto<'gc>, CompilerError> {
    #[derive(Copy, Clone)]
    struct Interner<'gc>(Context<'gc>);

    impl<'gc> StringInterner for Interner<'gc> {
        type String = String<'gc>;

        fn intern(&self, s: &[u8]) -> Self::String {
            self.0.state.strings.intern(&self.0, s)
        }
    }

    let interner = Interner(ctx);

    let chunk = parse_chunk(source, interner)?;
    let compiled_function = compile_chunk(&chunk, interner)?;

    Ok(FunctionProto::from_compiled(&ctx, compiled_function))
}
