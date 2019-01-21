use std::error::Error as StdError;
use std::fmt;

use gc_arena::Collect;

use crate::{ClosureError, CompilerError, InvalidTableKey, ParserError, StringError};

#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum Error {
    ParserError(ParserError),
    CompilerError(CompilerError),
    ClosureError(ClosureError),
    InvalidTableKey(InvalidTableKey),
    StringError(StringError),
}

impl StdError for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParserError(error) => write!(fmt, "parser error: {}", error),
            Error::CompilerError(error) => write!(fmt, "compiler error: {}", error),
            Error::ClosureError(error) => write!(fmt, "closure error: {}", error),
            Error::InvalidTableKey(error) => write!(fmt, "invalid table key: {}", error),
            Error::StringError(error) => write!(fmt, "string error: {}", error),
        }
    }
}

impl From<ParserError> for Error {
    fn from(error: ParserError) -> Error {
        Error::ParserError(error)
    }
}

impl From<CompilerError> for Error {
    fn from(error: CompilerError) -> Error {
        Error::CompilerError(error)
    }
}

impl From<ClosureError> for Error {
    fn from(error: ClosureError) -> Error {
        Error::ClosureError(error)
    }
}

impl From<InvalidTableKey> for Error {
    fn from(error: InvalidTableKey) -> Error {
        Error::InvalidTableKey(error)
    }
}

impl From<StringError> for Error {
    fn from(error: StringError) -> Error {
        Error::StringError(error)
    }
}
