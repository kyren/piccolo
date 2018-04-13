#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;

pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;
