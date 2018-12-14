use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::opcode::{ConstantIndex8, OpCode, RegisterIndex};
use crate::value::Value;

pub use crate::parser::BinaryOperator;
pub use crate::parser::UnaryOperator;

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

#[derive(Debug)]
pub enum BinOpArgs {
    RC(RegisterIndex, ConstantIndex8),
    CR(ConstantIndex8, RegisterIndex),
    RR(RegisterIndex, RegisterIndex),
}

pub struct SimpleBinOpEntry {
    pub make_opcode: fn(RegisterIndex, BinOpArgs) -> OpCode,
    pub constant_fold: for<'gc> fn(Value<'gc>, Value<'gc>) -> Option<Value<'gc>>,
}

lazy_static! {
    pub static ref SIMPLE_BINOPS: HashMap<SimpleBinOp, SimpleBinOpEntry> = {
        let mut m = HashMap::new();

        m.insert(
            SimpleBinOp::Add,
            SimpleBinOpEntry {
                make_opcode: |dest, args| match args {
                    BinOpArgs::RC(left, right) => OpCode::AddRC { dest, left, right },
                    BinOpArgs::CR(left, right) => OpCode::AddCR { dest, left, right },
                    BinOpArgs::RR(left, right) => OpCode::AddRR { dest, left, right },
                },
                constant_fold: |left, right| left.add(right),
            },
        );

        m
    };
}

pub struct ComparisonBinOpEntry {
    pub make_opcodes: fn(RegisterIndex, BinOpArgs) -> [OpCode; 4],
    pub constant_fold: for<'gc> fn(Value<'gc>, Value<'gc>) -> Option<Value<'gc>>,
}

lazy_static! {
    pub static ref COMPARISON_BINOPS: HashMap<ComparisonBinOp, ComparisonBinOpEntry> = {
        let mut m = HashMap::new();

        m.insert(
            ComparisonBinOp::Equal,
            ComparisonBinOpEntry {
                make_opcodes: |dest, args| {
                    [
                        match args {
                            BinOpArgs::RC(left, right) => OpCode::EqRC {
                                equal: true,
                                left,
                                right,
                            },
                            BinOpArgs::CR(left, right) => OpCode::EqCR {
                                equal: true,
                                left,
                                right,
                            },
                            BinOpArgs::RR(left, right) => OpCode::EqRR {
                                equal: true,
                                left,
                                right,
                            },
                        },
                        OpCode::Jump { offset: 1 },
                        OpCode::LoadBool {
                            dest,
                            value: false,
                            skip_next: true,
                        },
                        OpCode::LoadBool {
                            dest,
                            value: true,
                            skip_next: false,
                        },
                    ]
                },
                constant_fold: |left, right| Some(Value::Boolean(left == right)),
            },
        );

        m
    };
}

pub struct UnOpEntry {
    pub make_opcode: fn(RegisterIndex, RegisterIndex) -> OpCode,
    pub constant_fold: for<'gc> fn(Value<'gc>) -> Option<Value<'gc>>,
}

lazy_static! {
    pub static ref UNOPS: HashMap<UnaryOperator, UnOpEntry> = {
        let mut m = HashMap::new();

        m.insert(
            UnaryOperator::Not,
            UnOpEntry {
                make_opcode: |dest, source| OpCode::Not { dest, source },
                constant_fold: |v| Some(Value::Boolean(!v.as_bool())),
            },
        );

        m
    };
}
