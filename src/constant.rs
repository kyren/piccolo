use std::hash::{Hash, Hasher};

use gc_arena::Collect;

use crate::compiler::string_utils::{read_float, read_integer, trim_whitespace};

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

    pub fn as_string_ref(&self) -> Constant<&S> {
        match self {
            Constant::Nil => Constant::Nil,
            Constant::Boolean(b) => Constant::Boolean(*b),
            Constant::Integer(i) => Constant::Integer(*i),
            Constant::Number(n) => Constant::Number(*n),
            Constant::String(s) => Constant::String(s),
        }
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
    /// Converts the given constant to an integer or number, if possible.
    pub fn to_numeric(&self) -> Option<Constant<S>> {
        match self {
            &Self::Integer(a) => Some(Constant::Integer(a)),
            &Self::Number(a) => Some(Constant::Number(a)),
            Self::String(a) => {
                let a = trim_whitespace(a.as_ref());
                if let Some(i) = read_integer(a) {
                    Some(Constant::Integer(i))
                } else if let Some(n) = read_float(a) {
                    Some(Constant::Number(n))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Interprets Numbers, Integers, and Strings as a Number, if possible.
    pub fn to_number(&self) -> Option<f64> {
        match self.to_numeric() {
            Some(Self::Integer(a)) => Some(a as f64),
            Some(Self::Number(a)) => Some(a),
            _ => None,
        }
    }

    /// Interprets Numbers, Integers, and Strings as an Integer, if possible.
    pub fn to_integer(&self) -> Option<i64> {
        match self.to_numeric() {
            Some(Self::Integer(a)) => Some(a),
            Some(Self::Number(a)) => {
                if ((a as i64) as f64) == a {
                    Some(a as i64)
                } else {
                    None
                }
            }
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
                    // Wrapping version of std's div_floor
                    let d = a.wrapping_div(b);
                    let r = a.wrapping_rem(b);
                    let d = if (r > 0 && b < 0) || (r < 0 && b > 0) {
                        d - 1
                    } else {
                        d
                    };
                    Some(Self::Integer(d))
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
        let rhs = rhs.to_integer()?;
        if rhs < 0 {
            return None;
        }
        let rhs = rhs.try_into().ok().unwrap_or(u32::MAX);
        Some(Self::Integer(
            self.to_integer()?.checked_shl(rhs).unwrap_or(0),
        ))
    }

    pub fn shift_right(&self, rhs: &Self) -> Option<Self> {
        let rhs = rhs.to_integer()?;
        if rhs < 0 {
            return None;
        }
        let lhs = self.to_integer()? as u64;
        let rhs = rhs.try_into().ok().unwrap_or(u32::MAX);
        Some(Self::Integer(lhs.checked_shr(rhs).unwrap_or(0) as i64))
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
