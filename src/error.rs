use std::error::Error as StdError;
use std::string::String as StdString;
use std::{fmt, io};

use gc_arena::{Collect, Gc, StaticCollect};

use crate::{
    ClosureError, CompilerError, InvalidTableKey, ParserError, StringError, ThreadError, Value,
};

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
#[collect(empty_drop)]
pub struct RuntimeError<'gc>(pub Gc<'gc, Value<'gc>>);

impl<'gc> StdError for RuntimeError<'gc> {}

impl<'gc> fmt::Display for RuntimeError<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = Vec::new();
        self.0.display(&mut buf).unwrap();
        let s = StdString::from_utf8_lossy(&buf);
        write!(fmt, "{}", s)
    }
}

// Safe, no drop impl
#[derive(Debug, Collect)]
#[collect(unsafe_drop)]
pub enum Error<'gc> {
    IoError(StaticCollect<io::Error>),
    ParserError(ParserError),
    CompilerError(CompilerError),
    ClosureError(ClosureError),
    InvalidTableKey(InvalidTableKey),
    StringError(StringError),
    ThreadError(ThreadError),
    TypeError(TypeError),
    RuntimeError(RuntimeError<'gc>),
}

impl<'gc> StdError for Error<'gc> {}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IoError(error) => write!(fmt, "i/o error: {}", error.0),
            Error::ParserError(error) => write!(fmt, "parser error: {}", error),
            Error::CompilerError(error) => write!(fmt, "compiler error: {}", error),
            Error::ClosureError(error) => write!(fmt, "closure error: {}", error),
            Error::InvalidTableKey(error) => write!(fmt, "invalid table key: {}", error),
            Error::StringError(error) => write!(fmt, "string error: {}", error),
            Error::ThreadError(error) => write!(fmt, "thread error: {}", error),
            Error::TypeError(error) => write!(fmt, "type error: {}", error),
            Error::RuntimeError(error) => write!(fmt, "runtime error: {}", error),
        }
    }
}

impl<'gc> From<io::Error> for Error<'gc> {
    fn from(error: io::Error) -> Error<'gc> {
        Error::IoError(StaticCollect(error))
    }
}

impl<'gc> From<ParserError> for Error<'gc> {
    fn from(error: ParserError) -> Error<'gc> {
        Error::ParserError(error)
    }
}

impl<'gc> From<CompilerError> for Error<'gc> {
    fn from(error: CompilerError) -> Error<'gc> {
        Error::CompilerError(error)
    }
}

impl<'gc> From<ClosureError> for Error<'gc> {
    fn from(error: ClosureError) -> Error<'gc> {
        Error::ClosureError(error)
    }
}

impl<'gc> From<InvalidTableKey> for Error<'gc> {
    fn from(error: InvalidTableKey) -> Error<'gc> {
        Error::InvalidTableKey(error)
    }
}

impl<'gc> From<StringError> for Error<'gc> {
    fn from(error: StringError) -> Error<'gc> {
        Error::StringError(error)
    }
}

impl<'gc> From<ThreadError> for Error<'gc> {
    fn from(error: ThreadError) -> Error<'gc> {
        Error::ThreadError(error)
    }
}

impl<'gc> From<TypeError> for Error<'gc> {
    fn from(error: TypeError) -> Error<'gc> {
        Error::TypeError(error)
    }
}

impl<'gc> From<RuntimeError<'gc>> for Error<'gc> {
    fn from(error: RuntimeError<'gc>) -> Error<'gc> {
        Error::RuntimeError(error)
    }
}

impl<'gc> Error<'gc> {
    pub fn to_static(self) -> StaticError {
        match self {
            Error::IoError(error) => StaticError::IoError(error.0),
            Error::ParserError(error) => StaticError::ParserError(error),
            Error::CompilerError(error) => StaticError::CompilerError(error),
            Error::ClosureError(error) => StaticError::ClosureError(error),
            Error::InvalidTableKey(error) => StaticError::InvalidTableKey(error),
            Error::StringError(error) => StaticError::StringError(error),
            Error::ThreadError(error) => StaticError::ThreadError(error),
            Error::TypeError(error) => StaticError::TypeError(error),
            Error::RuntimeError(error) => {
                let mut buf = Vec::new();
                error.0.display(&mut buf).unwrap();
                StaticError::RuntimeError(StdString::from_utf8_lossy(&buf).to_owned().to_string())
            }
        }
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum StaticError {
    IoError(io::Error),
    ParserError(ParserError),
    CompilerError(CompilerError),
    ClosureError(ClosureError),
    InvalidTableKey(InvalidTableKey),
    StringError(StringError),
    ThreadError(ThreadError),
    TypeError(TypeError),
    RuntimeError(String),
}

impl StdError for StaticError {}

impl fmt::Display for StaticError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StaticError::IoError(error) => write!(fmt, "i/o error: {}", error),
            StaticError::ParserError(error) => write!(fmt, "parser error: {}", error),
            StaticError::CompilerError(error) => write!(fmt, "compiler error: {}", error),
            StaticError::ClosureError(error) => write!(fmt, "closure error: {}", error),
            StaticError::InvalidTableKey(error) => write!(fmt, "invalid table key: {}", error),
            StaticError::StringError(error) => write!(fmt, "string error: {}", error),
            StaticError::ThreadError(error) => write!(fmt, "thread error: {}", error),
            StaticError::TypeError(error) => write!(fmt, "type error: {}", error),
            StaticError::RuntimeError(error) => write!(fmt, "runtime error: {}", error),
        }
    }
}
