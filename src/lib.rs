#[macro_use]
extern crate failure;
extern crate fnv;
#[macro_use]
extern crate lazy_static;
extern crate num_traits;

#[macro_use]
extern crate gc_arena;

pub mod code;
pub mod function;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod state;
pub mod string;
pub mod table;
pub mod value;

#[cfg(test)]
mod tests;
