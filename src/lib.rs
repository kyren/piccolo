#[macro_use]
extern crate failure;
extern crate fnv;
#[macro_use]
extern crate lazy_static;
extern crate num_traits;

#[macro_use]
extern crate gc_arena;

pub mod compiler;
pub mod conversion;
pub mod function;
pub mod io;
pub mod lexer;
pub mod opcode;
pub mod operators;
pub mod parser;
pub mod state;
pub mod string;
pub mod table;
pub mod thread;
pub mod value;

#[cfg(test)]
mod tests;
