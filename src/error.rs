use std::error::Error as StdError;
use std::{fmt, io};

use gc_arena::Collect;

use crate::{ClosureError, CompilerError, InvalidTableKey, ParserError, StringError, ThreadError};

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct TypeError {
    pub expected: &'static str,
    pub found: &'static str,
}

impl StdError for TypeError {}

impl fmt::Display for TypeError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "type error, expected {}, found {}",
            self.expected, self.found
        )
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct RuntimeError(pub Option<String>);

impl StdError for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(msg) = &self.0 {
            write!(fmt, "{}", msg)
        } else {
            write!(fmt, "error")
        }
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum Error {
    ParserError(ParserError),
    CompilerError(CompilerError),
    ClosureError(ClosureError),
    InvalidTableKey(InvalidTableKey),
    StringError(StringError),
    ThreadError(ThreadError),
    TypeError(TypeError),
    RuntimeError(RuntimeError),
    IoError(io::Error),
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
            Error::ThreadError(error) => write!(fmt, "thread error: {}", error),
            Error::TypeError(error) => write!(fmt, "type error: {}", error),
            Error::RuntimeError(error) => write!(fmt, "runtime error: {}", error),
            Error::IoError(error) => write!(fmt, "i/o error: {}", error),
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

impl From<ThreadError> for Error {
    fn from(error: ThreadError) -> Error {
        Error::ThreadError(error)
    }
}

impl From<TypeError> for Error {
    fn from(error: TypeError) -> Error {
        Error::TypeError(error)
    }
}

impl From<RuntimeError> for Error {
    fn from(error: RuntimeError) -> Error {
        Error::RuntimeError(error)
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IoError(error)
    }
}
