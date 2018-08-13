pub type Register = u8;
pub type Constant = u16;

pub const MAX_VAR_COUNT: u8 = 254;

/// Count of arguments or return values which can be 0-254 or a special "variable" value.
#[derive(Debug, Copy, Clone, Collect)]
pub struct VarCount(u8);

impl VarCount {
    pub fn make_variable() -> VarCount {
        VarCount(0)
    }

    pub fn make_count(count: u8) -> Option<VarCount> {
        if count == 255 {
            None
        } else {
            Some(VarCount(count + 1))
        }
    }

    pub fn is_variable(&self) -> bool {
        self.0 == 0
    }

    pub fn get_count(&self) -> Option<u8> {
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
        results: VarCount,
    },
    Return {
        start: Register,
        count: VarCount,
    },
}
