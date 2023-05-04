use std::{error::Error as StdError, fmt, io, string::String as StdString};

use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::{
    BadThreadMode, BinaryOperatorError, ClosureError, CompilerError, InvalidTableKey, ParserError,
    String, StringError, ThreadError, Value,
};

#[derive(Debug, Clone, Copy, Collect)]
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

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct RuntimeError<'gc>(pub Value<'gc>);

impl<'gc> StdError for RuntimeError<'gc> {}

impl<'gc> fmt::Display for RuntimeError<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = Vec::new();
        self.0.display(&mut buf).unwrap();
        let s = StdString::from_utf8_lossy(&buf);
        write!(fmt, "{}", s)
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub enum Error<'gc> {
    IoError(StaticCollect<io::Error>),
    ParserError(ParserError),
    CompilerError(CompilerError),
    ClosureError(ClosureError),
    InvalidTableKey(InvalidTableKey),
    StringError(StringError),
    ThreadError(ThreadError),
    BadThreadMode(BadThreadMode),
    TypeError(TypeError),
    BinaryOperatorError(BinaryOperatorError),
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
            Error::BadThreadMode(error) => write!(fmt, "bad thread mode: {}", error),
            Error::TypeError(error) => write!(fmt, "type error: {}", error),
            Error::BinaryOperatorError(error) => write!(fmt, "operator error: {}", error),
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

impl<'gc> From<BadThreadMode> for Error<'gc> {
    fn from(error: BadThreadMode) -> Error<'gc> {
        Error::BadThreadMode(error)
    }
}

impl<'gc> From<TypeError> for Error<'gc> {
    fn from(error: TypeError) -> Error<'gc> {
        Error::TypeError(error)
    }
}

impl<'gc> From<BinaryOperatorError> for Error<'gc> {
    fn from(error: BinaryOperatorError) -> Error<'gc> {
        Error::BinaryOperatorError(error)
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
            Error::BadThreadMode(error) => StaticError::BadThreadMode(error),
            Error::TypeError(error) => StaticError::TypeError(error),
            Error::BinaryOperatorError(error) => StaticError::BinaryOperatorError(error),
            Error::RuntimeError(error) => {
                let mut buf = Vec::new();
                error.0.display(&mut buf).unwrap();
                StaticError::RuntimeError(StdString::from_utf8_lossy(&buf).to_owned().to_string())
            }
        }
    }

    pub fn to_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        match self {
            Error::RuntimeError(error) => error.0,
            other => Value::String(String::from_std_string(mc, other.to_string())),
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
    BadThreadMode(BadThreadMode),
    TypeError(TypeError),
    BinaryOperatorError(BinaryOperatorError),
    RuntimeError(StdString),
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
            StaticError::BadThreadMode(error) => write!(fmt, "bad thread mode: {}", error),
            StaticError::TypeError(error) => write!(fmt, "type error: {}", error),
            StaticError::BinaryOperatorError(error) => write!(fmt, "operator error: {}", error),
            StaticError::RuntimeError(error) => write!(fmt, "runtime error: {}", error),
        }
    }
}
