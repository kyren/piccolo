use std::hash::{Hash, Hasher};

use gc_arena::Collect;

use crate::compiler::lexer::{read_float, read_hex_float};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Constant<S> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(S),
}

impl<S> Constant<S> {
    pub fn to_bool(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn not(&self) -> Constant<S> {
        Constant::Boolean(!self.to_bool())
    }

    pub fn map_string<S2>(self, f: impl FnOnce(S) -> S2) -> Constant<S2> {
        match self {
            Constant::Nil => Constant::Nil,
            Constant::Boolean(b) => Constant::Boolean(b),
            Constant::Integer(i) => Constant::Integer(i),
            Constant::Number(n) => Constant::Number(n),
            Constant::String(s) => Constant::String(f(s)),
        }
    }
}

impl<S: AsRef<[u8]>> Constant<S> {
    /// Interprets Numbers, Integers, and Strings as a Number, if possible.
    pub fn to_number(&self) -> Option<f64> {
        match self {
            &Self::Integer(a) => Some(a as f64),
            &Self::Number(a) => Some(a),
            Self::String(a) => {
                if let Some(f) = read_hex_float(a.as_ref()) {
                    Some(f)
                } else {
                    read_float(a.as_ref())
                }
            }
            _ => None,
        }
    }

    /// Interprets Numbers, Integers, and Strings as an Integer, if possible.
    pub fn to_integer(&self) -> Option<i64> {
        match self {
            &Self::Integer(a) => Some(a),
            &Self::Number(a) => {
                if ((a as i64) as f64) == a {
                    Some(a as i64)
                } else {
                    None
                }
            }
            Self::String(a) => match if let Some(f) = read_hex_float(a.as_ref()) {
                Some(f)
            } else {
                read_float(a.as_ref())
            } {
                Some(f) => {
                    if ((f as i64) as f64) == f {
                        Some(f as i64)
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    // Mathematical operators

    pub fn add(&self, rhs: &Self) -> Option<Self> {
        Some(match (self, rhs) {
            (&Self::Integer(a), &Self::Integer(b)) => Self::Integer(a.wrapping_add(b)),
            (a, b) => Self::Number(a.to_number()? + b.to_number()?),
        })
    }

    pub fn subtract(&self, rhs: &Self) -> Option<Self> {
        Some(match (self, rhs) {
            (&Self::Integer(a), &Self::Integer(b)) => Self::Integer(a.wrapping_sub(b)),
            (a, b) => Self::Number(a.to_number()? - b.to_number()?),
        })
    }

    pub fn multiply(&self, rhs: &Self) -> Option<Self> {
        Some(match (self, rhs) {
            (&Self::Integer(a), &Self::Integer(b)) => Self::Integer(a.wrapping_mul(b)),
            (a, b) => Self::Number(a.to_number()? * b.to_number()?),
        })
    }

    /// This operation always returns a Number, even when called with Integer arguments.
    pub fn float_divide(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Number(self.to_number()? / rhs.to_number()?))
    }

    /// This operation returns an Integer only if both arguments are Integers. Rounding is towards
    /// negative infinity.
    pub fn floor_divide(&self, rhs: &Self) -> Option<Self> {
        match (self, rhs) {
            (&Self::Integer(a), &Self::Integer(b)) => {
                if b == 0 {
                    None
                } else {
                    Some(Self::Integer(a.wrapping_div(b)))
                }
            }
            (a, b) => Some(Self::Number((a.to_number()? / b.to_number()?).floor())),
        }
    }

    /// Computes the Lua modulus (`%`) operator. This is unlike Rust's `%` operator which computes
    /// the remainder.
    pub fn modulo(&self, rhs: &Self) -> Option<Self> {
        match (self, rhs) {
            (&Self::Integer(a), &Self::Integer(b)) => {
                if b == 0 {
                    None
                } else {
                    Some(Self::Integer(((a % b) + b) % b))
                }
            }
            (a, b) => {
                let (a, b) = (a.to_number()?, b.to_number()?);
                Some(Self::Number(((a % b) + b) % b))
            }
        }
    }

    /// This operation always returns a Number, even when called with Integer arguments.
    pub fn exponentiate(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Number(self.to_number()?.powf(rhs.to_number()?)))
    }

    pub fn negate(&self) -> Option<Self> {
        match self {
            &Self::Integer(a) => Some(Self::Integer(a.wrapping_neg())),
            &Self::Number(a) => Some(Self::Number(-a)),
            s => s.to_number().map(|x| Self::Number(-x)),
        }
    }

    // Bitwise operators

    pub fn bitwise_not(&self) -> Option<Self> {
        Some(Self::Integer(!self.to_integer()?))
    }

    pub fn bitwise_and(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Integer(self.to_integer()? & rhs.to_integer()?))
    }

    pub fn bitwise_or(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Integer(self.to_integer()? | rhs.to_integer()?))
    }

    pub fn bitwise_xor(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Integer(self.to_integer()? ^ rhs.to_integer()?))
    }

    pub fn shift_left(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Integer(self.to_integer()? << rhs.to_integer()?))
    }

    pub fn shift_right(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Integer(
            (self.to_integer()? as u64 >> rhs.to_integer()? as u64) as i64,
        ))
    }

    // Comparison operators

    pub fn is_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Nil, _) => false,

            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Boolean(_), _) => false,

            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Integer(a), Self::Number(b)) => *a as f64 == *b,
            (Self::Integer(_), _) => false,

            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Number(a), Self::Integer(b)) => *b as f64 == *a,
            (Self::Number(_), _) => false,

            (Self::String(a), Self::String(b)) => a.as_ref() == b.as_ref(),
            (Self::String(_), _) => false,
        }
    }

    pub fn less_than(&self, rhs: &Self) -> Option<bool> {
        Some(match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => a < b,
            (Self::String(a), Self::String(b)) => a.as_ref() < b.as_ref(),
            (a, b) => a.to_number()? < b.to_number()?,
        })
    }

    pub fn less_equal(&self, rhs: &Self) -> Option<bool> {
        Some(match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => a <= b,
            (Self::String(a), Self::String(b)) => a.as_ref() <= b.as_ref(),
            (a, b) => a.to_number()? <= b.to_number()?,
        })
    }
}

impl<S: AsRef<[u8]>> PartialEq for Constant<S> {
    fn eq(&self, other: &Self) -> bool {
        self.is_equal(other)
    }
}

/// Wrapper for a `Constant` that implements Hash and Eq, and only compares equal when the types are
/// bit for bit identical.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct IdenticalConstant<S>(pub Constant<S>);

impl<S> From<Constant<S>> for IdenticalConstant<S> {
    fn from(value: Constant<S>) -> Self {
        Self(value)
    }
}

impl<S: AsRef<[u8]>> PartialEq for IdenticalConstant<S> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Constant::Nil, Constant::Nil) => true,
            (Constant::Nil, _) => false,

            (Constant::Boolean(a), Constant::Boolean(b)) => a == b,
            (Constant::Boolean(_), _) => false,

            (Constant::Integer(a), Constant::Integer(b)) => a == b,
            (Constant::Integer(_), _) => false,

            (Constant::Number(a), Constant::Number(b)) => a.to_bits() == b.to_bits(),
            (Constant::Number(_), _) => false,

            (Constant::String(a), Constant::String(b)) => a.as_ref() == b.as_ref(),
            (Constant::String(_), _) => false,
        }
    }
}

impl<S: AsRef<[u8]>> Eq for IdenticalConstant<S> {}

impl<S: AsRef<[u8]>> Hash for IdenticalConstant<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Constant::Nil => {
                Hash::hash(&0, state);
            }
            Constant::Boolean(b) => {
                Hash::hash(&1, state);
                b.hash(state);
            }
            Constant::Integer(i) => {
                Hash::hash(&2, state);
                i.hash(state);
            }
            Constant::Number(n) => {
                Hash::hash(&3, state);
                n.to_bits().hash(state);
            }
            Constant::String(s) => {
                Hash::hash(&4, state);
                s.as_ref().hash(state);
            }
        }
    }
}
