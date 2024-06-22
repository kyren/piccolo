use crate::{
    opcode::{Operation, RCIndex},
    types::RegisterIndex,
    Constant,
};

use super::parser::{BinaryOperator, UnaryOperator};

// Binary operators which map directly to a single opcode
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum SimpleBinOp {
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
}

// Binary operators which map to Eq / LessThan / LessEqual operations combined with Jump and
// LoadBool
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ComparisonBinOp {
    NotEqual,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

// 'and' and 'or', which short circuit their right hand side
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ShortCircuitBinOp {
    And,
    Or,
}

// Categorized BinaryOperator
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum BinOpCategory {
    Simple(SimpleBinOp),
    Comparison(ComparisonBinOp),
    ShortCircuit(ShortCircuitBinOp),
    Concat,
}

pub fn categorize_binop(binop: BinaryOperator) -> BinOpCategory {
    match binop {
        BinaryOperator::Add => BinOpCategory::Simple(SimpleBinOp::Add),
        BinaryOperator::Sub => BinOpCategory::Simple(SimpleBinOp::Sub),
        BinaryOperator::Mul => BinOpCategory::Simple(SimpleBinOp::Mul),
        BinaryOperator::Mod => BinOpCategory::Simple(SimpleBinOp::Mod),
        BinaryOperator::Pow => BinOpCategory::Simple(SimpleBinOp::Pow),
        BinaryOperator::Div => BinOpCategory::Simple(SimpleBinOp::Div),
        BinaryOperator::IDiv => BinOpCategory::Simple(SimpleBinOp::IDiv),
        BinaryOperator::BitAnd => BinOpCategory::Simple(SimpleBinOp::BitAnd),
        BinaryOperator::BitOr => BinOpCategory::Simple(SimpleBinOp::BitOr),
        BinaryOperator::BitXor => BinOpCategory::Simple(SimpleBinOp::BitXor),
        BinaryOperator::ShiftLeft => BinOpCategory::Simple(SimpleBinOp::ShiftLeft),
        BinaryOperator::ShiftRight => BinOpCategory::Simple(SimpleBinOp::ShiftRight),
        BinaryOperator::Concat => BinOpCategory::Concat,
        BinaryOperator::NotEqual => BinOpCategory::Comparison(ComparisonBinOp::NotEqual),
        BinaryOperator::Equal => BinOpCategory::Comparison(ComparisonBinOp::Equal),
        BinaryOperator::LessThan => BinOpCategory::Comparison(ComparisonBinOp::LessThan),
        BinaryOperator::LessEqual => BinOpCategory::Comparison(ComparisonBinOp::LessEqual),
        BinaryOperator::GreaterThan => BinOpCategory::Comparison(ComparisonBinOp::GreaterThan),
        BinaryOperator::GreaterEqual => BinOpCategory::Comparison(ComparisonBinOp::GreaterEqual),
        BinaryOperator::And => BinOpCategory::ShortCircuit(ShortCircuitBinOp::And),
        BinaryOperator::Or => BinOpCategory::ShortCircuit(ShortCircuitBinOp::Or),
    }
}

pub fn simple_binop_operation(
    simple_binop: SimpleBinOp,
    dest: RegisterIndex,
    left: RCIndex,
    right: RCIndex,
) -> Operation {
    match simple_binop {
        SimpleBinOp::Add => Operation::Add { dest, left, right },
        SimpleBinOp::Sub => Operation::Sub { dest, left, right },
        SimpleBinOp::Mul => Operation::Mul { dest, left, right },
        SimpleBinOp::Div => Operation::Div { dest, left, right },
        SimpleBinOp::IDiv => Operation::IDiv { dest, left, right },
        SimpleBinOp::Mod => Operation::Mod { dest, left, right },
        SimpleBinOp::Pow => Operation::Pow { dest, left, right },
        SimpleBinOp::BitAnd => Operation::BitAnd { dest, left, right },
        SimpleBinOp::BitOr => Operation::BitOr { dest, left, right },
        SimpleBinOp::BitXor => Operation::BitXor { dest, left, right },
        SimpleBinOp::ShiftLeft => Operation::ShiftLeft { dest, left, right },
        SimpleBinOp::ShiftRight => Operation::ShiftRight { dest, left, right },
    }
}

pub fn simple_binop_const_fold<S: AsRef<[u8]>>(
    simple_binop: SimpleBinOp,
    left: &Constant<S>,
    right: &Constant<S>,
) -> Option<Constant<S>> {
    match simple_binop {
        SimpleBinOp::Add => left.add(right),
        SimpleBinOp::Sub => left.subtract(right),
        SimpleBinOp::Mul => left.multiply(right),
        SimpleBinOp::Mod => left.modulo(right),
        SimpleBinOp::Pow => left.exponentiate(right),
        SimpleBinOp::Div => left.float_divide(right),
        SimpleBinOp::IDiv => left.floor_divide(right),
        _ => None,
    }
}

pub fn comparison_binop_operation(
    comparison_binop: ComparisonBinOp,
    left: RCIndex,
    right: RCIndex,
    skip_if: bool,
) -> Operation {
    match comparison_binop {
        ComparisonBinOp::Equal => Operation::Eq {
            skip_if,
            left,
            right,
        },
        ComparisonBinOp::NotEqual => Operation::Eq {
            skip_if: !skip_if,
            left,
            right,
        },
        ComparisonBinOp::LessThan => Operation::Less {
            skip_if,
            left,
            right,
        },
        ComparisonBinOp::LessEqual => Operation::LessEq {
            skip_if,
            left,
            right,
        },
        ComparisonBinOp::GreaterThan => Operation::Less {
            skip_if,
            left: right,
            right: left,
        },
        ComparisonBinOp::GreaterEqual => Operation::LessEq {
            skip_if,
            left: right,
            right: left,
        },
    }
}

pub fn comparison_binop_const_fold<S: AsRef<[u8]>>(
    comparison_binop: ComparisonBinOp,
    left: &Constant<S>,
    right: &Constant<S>,
) -> Option<Constant<S>> {
    match comparison_binop {
        ComparisonBinOp::Equal => Some(Constant::Boolean(left.is_equal(right))),
        ComparisonBinOp::LessThan => match left.less_than(right) {
            Some(a) => Some(Constant::Boolean(a)),
            _ => None,
        },
        ComparisonBinOp::LessEqual => match left.less_equal(right) {
            Some(a) => Some(Constant::Boolean(a)),
            _ => None,
        },
        _ => None,
    }
}

pub fn unop_operation(
    unop: UnaryOperator,
    dest: RegisterIndex,
    source: RegisterIndex,
) -> Operation {
    match unop {
        UnaryOperator::Minus => Operation::Minus { dest, source },
        UnaryOperator::Not => Operation::Not { dest, source },
        UnaryOperator::BitNot => Operation::BitNot { dest, source },
        UnaryOperator::Len => Operation::Length { dest, source },
    }
}

pub fn unop_const_fold<S: AsRef<[u8]>>(
    unop: UnaryOperator,
    cons: &Constant<S>,
) -> Option<Constant<S>> {
    match unop {
        UnaryOperator::Minus => cons.negate(),
        UnaryOperator::Not => Some(cons.not()),
        UnaryOperator::BitNot => cons.bitwise_not(),
        UnaryOperator::Len => None,
    }
}
