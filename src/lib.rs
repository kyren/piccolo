#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;

pub mod lexer;
pub mod parser;
pub mod gc;

#[cfg(test)]
mod tests;
