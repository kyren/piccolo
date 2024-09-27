use std::fmt::{self, Debug};

use gc_arena::Collect;

/// An index that points to a register in the stack relative to the current frame.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct RegisterIndex(pub u8);

/// An 8 bit index into the constant table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct ConstantIndex8(pub u8);

/// A 16 bit index into the constant table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct ConstantIndex16(pub u16);

/// An index into the upvalue table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct UpValueIndex(pub u8);

/// An index into the prototype table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct PrototypeIndex(pub u8);

/// A one byte Option value that can either be Some(0-254) or None
#[derive(Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct Opt254(u8);

#[derive(Debug, Collect, Clone, Copy, PartialEq, Eq)]
#[collect(require_static)]
pub enum UpValueDescriptor {
    Environment,
    ParentLocal(RegisterIndex),
    Outer(UpValueIndex),
}

/// Attributes that a locally declared variable can have.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum LocalAttribute {
    Const,
    Close,
}

impl Opt254 {
    pub fn try_new(v: Option<u8>) -> Option<Opt254> {
        if let Some(v) = v {
            if v == 255 {
                None
            } else {
                Some(Opt254(v))
            }
        } else {
            Some(Opt254(255))
        }
    }

    pub fn new(v: Option<u8>) -> Opt254 {
        Opt254::try_new(v).expect("Opt254 cannot hold Some(255)")
    }

    pub fn some(v: u8) -> Opt254 {
        Opt254::new(Some(v))
    }

    pub fn try_some(v: u8) -> Option<Opt254> {
        Opt254::try_new(Some(v))
    }

    pub fn none() -> Opt254 {
        Opt254::new(None)
    }

    pub fn is_some(self) -> bool {
        self.to_u8().is_some()
    }

    pub fn is_none(self) -> bool {
        self.to_u8().is_none()
    }

    pub fn to_u8(self) -> Option<u8> {
        if self.0 == 255 {
            None
        } else {
            Some(self.0)
        }
    }
}

impl Debug for Opt254 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.0 == 255 {
            write!(fmt, "Opt254(None)")
        } else {
            write!(fmt, "Opt254(Some({}))", self.0)
        }
    }
}

/// Count of arguments or return values which can either be a constant between 0-254 or a special
/// "variable" value.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct VarCount(Opt254);

impl VarCount {
    pub fn variable() -> VarCount {
        VarCount(Opt254::none())
    }

    pub fn constant(constant: u8) -> VarCount {
        VarCount(Opt254::some(constant))
    }

    pub fn try_constant(constant: u8) -> Option<VarCount> {
        Opt254::try_some(constant).map(VarCount)
    }

    pub fn is_variable(self) -> bool {
        self.0.is_none()
    }

    pub fn to_constant(self) -> Option<u8> {
        self.0.to_u8()
    }
}
