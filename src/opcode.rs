pub type Register = u8;
pub type Constant = u16;
pub type UpValueIndex = u8;
pub type FunctionProtoIndex = u8;

pub const MAX_VAR_COUNT: u8 = 254;

use gc_arena::Collect;

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
    AddRR {
        dest: Register,
        left: Register,
        right: Register,
    },
    AddRC {
        dest: Register,
        left: Register,
        right: Constant,
    },
    AddCR {
        dest: Register,
        left: Constant,
        right: Register,
    },
}
