use gc_arena::Collect;
use thiserror::Error;

use crate::{ThreadMode, TypeError};

#[derive(Debug, Copy, Clone, Error, Collect)]
#[collect(require_static)]
pub enum BinaryOperatorError {
    #[error("cannot add values")]
    Add,
    #[error("cannot subtract values")]
    Subtract,
    #[error("cannot multiply values")]
    Multiply,
    #[error("cannot float divide values")]
    FloatDivide,
    #[error("cannot floor divide values")]
    FloorDivide,
    #[error("cannot modulo values")]
    Modulo,
    #[error("cannot exponentiate values")]
    Exponentiate,
    #[error("cannot negate value")]
    UnaryNegate,
    #[error("cannot bitwise AND values")]
    BitAnd,
    #[error("cannot bitwise OR values")]
    BitOr,
    #[error("cannot bitwise XOR values")]
    BitXor,
    #[error("cannot bitwise NOT value")]
    BitNot,
    #[error("cannot shift value left")]
    ShiftLeft,
    #[error("cannot shift value right")]
    ShiftRight,
    #[error("cannot compare values with <")]
    LessThan,
    #[error("cannot compare values with <=")]
    LessEqual,
}

#[derive(Debug, Copy, Clone, Error, Collect)]
#[collect(require_static)]
#[error("bad thread mode: was {found:?} expected {expected:?}")]
pub struct BadThreadMode {
    pub expected: ThreadMode,
    pub found: ThreadMode,
}

#[derive(Debug, Copy, Clone, Error, Collect)]
#[collect(require_static)]
pub enum ThreadError {
    #[error("{}", if *.0 {
        "operation expects variable stack"
    } else {
        "unexpected variable stack in operation"
    })]
    ExpectedVariable(bool),
    #[error(transparent)]
    BadCall(TypeError),
}
