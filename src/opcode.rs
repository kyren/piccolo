use gc_arena::Collect;

/// An index that points to a register in the stack relative to the current frame.
#[derive(Debug, Copy, Clone, Collect)]
pub struct RegisterIndex(pub u8);

/// An 8 bit index into the constant table
#[derive(Debug, Copy, Clone, Collect)]
pub struct ConstantIndex8(pub u8);

/// A 16 bit index into the constant table
#[derive(Debug, Copy, Clone, Collect)]
pub struct ConstantIndex16(pub u16);

/// An index into the upvalue table
#[derive(Debug, Copy, Clone, Collect)]
pub struct UpValueIndex(pub u8);

/// An index into the prototype table
#[derive(Debug, Copy, Clone, Collect)]
pub struct PrototypeIndex(pub u8);

pub const MAX_VAR_COUNT: u8 = 254;

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
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    LoadConstant {
        dest: RegisterIndex,
        constant: ConstantIndex16,
    },
    LoadBool {
        dest: RegisterIndex,
        value: bool,
        skip_next: bool,
    },
    LoadNil {
        dest: RegisterIndex,
        count: u8,
    },
    Call {
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    },
    Return {
        start: RegisterIndex,
        count: VarCount,
    },
    Jump {
        displacement: i16,
    },
    Closure {
        dest: RegisterIndex,
        proto: PrototypeIndex,
    },
    GetUpValue {
        dest: RegisterIndex,
        source: UpValueIndex,
    },
    SetUpValue {
        dest: UpValueIndex,
        source: RegisterIndex,
    },
    EqRR {
        equal: bool,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    EqRC {
        equal: bool,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    EqCR {
        equal: bool,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    Not {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    AddRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    AddRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    AddCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
}
