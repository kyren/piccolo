use crate::value::Value;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
    Not,
    Minus,
    BitNot,
    Len,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Concat,
    NotEqual,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
}

pub fn apply_binop<'gc>(
    binop: BinaryOperator,
    left: Value<'gc>,
    right: Value<'gc>,
) -> Option<Value<'gc>> {
    match binop {
        BinaryOperator::Add => match (left, right) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a + b)),
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 + b)),
            (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a + b as f64)),
            _ => None,
        },
        _ => None,
    }
}
