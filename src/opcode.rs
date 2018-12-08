pub type Register = u8;
pub type Constant = u16;
pub type UpValueIndex = u8;
pub type FunctionProtoIndex = u8;

pub const MAX_VAR_COUNT: u8 = 254;

use gc_arena::Collect;

use crate::value::Value;

/// Count of arguments or return values which can either be a constant between 0-254 or a special
/// "variable" value.
#[derive(Debug, Copy, Clone, Collect)]
pub struct VarCount(u8);

impl VarCount {
    pub fn make_variable() -> VarCount {
        VarCount(0)
    }

    pub fn make_constant(constant: u8) -> Option<VarCount> {
        if constant == 255 {
            None
        } else {
            Some(VarCount(constant + 1))
        }
    }

    pub fn make_zero() -> VarCount {
        VarCount(1)
    }

    pub fn make_one() -> VarCount {
        VarCount(2)
    }

    pub fn is_variable(&self) -> bool {
        self.0 == 0
    }

    pub fn get_constant(&self) -> Option<u8> {
        if self.0 == 0 {
            None
        } else {
            Some(self.0 - 1)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Collect)]
pub enum UnOp {
    Minus,
    Not,
    BitNot,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Collect)]
pub enum BinOp {
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

#[derive(Debug, Copy, Clone, Collect)]
pub enum OpCode {
    Move {
        dest: Register,
        source: Register,
    },
    LoadConstant {
        dest: Register,
        constant: Constant,
    },
    LoadBool {
        dest: Register,
        value: bool,
        skip_next: bool,
    },
    LoadNil {
        dest: Register,
        count: u8,
    },
    Call {
        func: Register,
        args: VarCount,
        returns: VarCount,
    },
    Return {
        start: Register,
        count: VarCount,
    },
    Closure {
        proto: FunctionProtoIndex,
        dest: Register,
    },
    GetUpValue {
        source: UpValueIndex,
        dest: Register,
    },
    SetUpValue {
        source: Register,
        dest: UpValueIndex,
    },
    UnOp {
        unop: UnOp,
        dest: Register,
        source: Register,
    },
    BinOpRR {
        binop: BinOp,
        dest: Register,
        left: Register,
        right: Register,
    },
    BinOpRC {
        binop: BinOp,
        dest: Register,
        left: Register,
        right: Constant,
    },
    BinOpCR {
        binop: BinOp,
        dest: Register,
        left: Constant,
        right: Register,
    },
}

pub fn apply_unop<'gc>(unop: UnOp, value: Value<'gc>) -> Option<Value<'gc>> {
    match unop {
        UnOp::Not => Some(Value::Boolean(!value.as_bool())),
        _ => None,
    }
}

pub fn apply_binop<'gc>(binop: BinOp, left: Value<'gc>, right: Value<'gc>) -> Option<Value<'gc>> {
    match binop {
        BinOp::Add => match (left, right) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a + b)),
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 + b)),
            (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a + b as f64)),
            _ => None,
        },
        _ => None,
    }
}
