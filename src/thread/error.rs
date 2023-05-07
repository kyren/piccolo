use std::error::Error as StdError;
use std::fmt;

use gc_arena::Collect;

use crate::{ThreadMode, TypeError};

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum BinaryOperatorError {
    Add,
    Subtract,
    Multiply,
    FloatDivide,
    FloorDivide,
    Modulo,
    Exponentiate,
    UnaryNegate,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    ShiftLeft,
    ShiftRight,
    LessThan,
    LessEqual,
}

impl StdError for BinaryOperatorError {}

impl fmt::Display for BinaryOperatorError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperatorError::Add => write!(fmt, "cannot add values"),
            BinaryOperatorError::Subtract => write!(fmt, "cannot subtract values"),
            BinaryOperatorError::Multiply => write!(fmt, "cannot multiply values"),
            BinaryOperatorError::FloatDivide => write!(fmt, "cannot float divide values"),
            BinaryOperatorError::FloorDivide => write!(fmt, "cannot floor divide values"),
            BinaryOperatorError::Modulo => write!(fmt, "cannot modulo values"),
            BinaryOperatorError::Exponentiate => write!(fmt, "cannot exponentiate values"),
            BinaryOperatorError::UnaryNegate => write!(fmt, "cannot negate value"),
            BinaryOperatorError::BitAnd => write!(fmt, "cannot bitwise AND values"),
            BinaryOperatorError::BitOr => write!(fmt, "cannot bitwise OR values"),
            BinaryOperatorError::BitXor => write!(fmt, "cannot bitwise XOR values"),
            BinaryOperatorError::BitNot => write!(fmt, "cannot bitwise NOT value"),
            BinaryOperatorError::ShiftLeft => write!(fmt, "cannot shift value left"),
            BinaryOperatorError::ShiftRight => write!(fmt, "cannot shift value right"),
            BinaryOperatorError::LessThan => write!(fmt, "cannot compare values with <"),
            BinaryOperatorError::LessEqual => write!(fmt, "cannot compare values with <="),
        }
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub struct BadThreadMode {
    pub expected: ThreadMode,
    pub found: ThreadMode,
}

impl StdError for BadThreadMode {}

impl fmt::Display for BadThreadMode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "bad thread mode: was {:?} expected {:?}",
            self.found, self.expected
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum ThreadError {
    ExpectedVariable(bool),
    BadCall(TypeError),
    BadYield,
}

impl StdError for ThreadError {}

impl fmt::Display for ThreadError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ThreadError::ExpectedVariable(true) => {
                write!(fmt, "operation expects variable stack")
            }
            ThreadError::ExpectedVariable(false) => {
                write!(fmt, "unexpected variable stack in operation")
            }
            ThreadError::BadCall(type_error) => fmt::Display::fmt(type_error, fmt),
            ThreadError::BadYield => write!(fmt, "cannot yield from main thread"),
        }
    }
}
