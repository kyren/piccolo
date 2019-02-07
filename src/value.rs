use std::{f64, i64, io};

use gc_arena::{Collect, Gc, GcCell};
use num_traits::{identities::Zero, ToPrimitive};

use crate::{Callback, Closure, String, Table, Thread};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_copy)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub enum Value<'gc> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(String<'gc>),
    Table(Table<'gc>),
    Function(Function<'gc>),
    Thread(Thread<'gc>),
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Value<'gc>) -> bool {
        match (*self, *other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,

            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Boolean(_), _) => false,

            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Number(b)) => a as f64 == b,
            (Value::Integer(_), _) => false,

            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Number(a), Value::Integer(b)) => b as f64 == a,
            (Value::Number(_), _) => false,

            (Value::String(a), Value::String(b)) => a == b,
            (Value::String(_), _) => false,

            (Value::Table(a), Value::Table(b)) => a == b,
            (Value::Table(_), _) => false,

            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Function(_), _) => false,

            (Value::Thread(a), Value::Thread(b)) => a == b,
            (Value::Thread(_), _) => false,
        }
    }
}

// In the future will be able to use f64::copysign
// See https://github.com/rust-lang/rust/issues/58046
fn copysign(to: f64, from: f64) -> f64 {
    to * if from < 0.0 { -1.0 } else { 1.0 }
}

impl<'gc> Value<'gc> {
    pub fn type_name(self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Boolean(_) => "boolean",
            Value::Integer(_) => "integer",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Table(_) => "table",
            Value::Function(_) => "function",
            Value::Thread(_) => "thread",
        }
    }

    /// Lua `nil` and `false` are false, anything else is true.
    pub fn to_bool(self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn not(self) -> Value<'gc> {
        Value::Boolean(!self.to_bool())
    }

    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_add(b))),
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 + b)),
            (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a + b as f64)),
            _ => None,
        }
    }

    pub fn subtract(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_sub(b))),
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a - b)),
            (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 - b)),
            (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a - b as f64)),
            _ => None,
        }
    }

    pub fn multiply(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_mul(b))),
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a * b)),
            (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 * b)),
            (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a * b as f64)),
            _ => None,
        }
    }

    // A small helper function to handle division-like zero handling
    fn safe_div<T: PartialEq + ToPrimitive + Zero>(
        lhs: T,
        rhs: T,
        f: &Fn(T, T) -> Value<'gc>,
    ) -> Value<'gc> {
        match (lhs, rhs) {
            // Seems that all nans are negative in lua
            (ref a, ref b) if a.is_zero() && b.is_zero() => Value::Number(-f64::NAN),
            (ref a, ref b) if b.is_zero() => {
                Value::Number(copysign(f64::INFINITY, a.to_f64().unwrap()))
            }
            (a, b) => f(a, b),
        }
    }

    // This operation always returns a Number, even when called by int arguments
    pub fn float_divide(self, other: Value<'gc>) -> Option<Value<'gc>> {
        let (a, b) = match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => (a as f64, b as f64),
            (Value::Number(a), Value::Number(b)) => (a, b),
            (Value::Integer(a), Value::Number(b)) => (a as f64, b),
            (Value::Number(a), Value::Integer(b)) => (a, b as f64),
            _ => return None,
        };

        Some(Value::safe_div(a, b, &|a, b| Value::Number(a / b)))
    }

    pub fn floor_divide(self, other: Value<'gc>) -> Option<Value<'gc>> {
        let (a, b) = match (self, other) {
            // Seems that all nans are negative in lua
            (Value::Integer(a), Value::Integer(b)) => {
                return Some(Value::safe_div(a, b, &|a, b| {
                    Value::Integer(a.wrapping_div(b))
                }));
            }
            (Value::Number(a), Value::Number(b)) => (a, b),
            (Value::Integer(a), Value::Number(b)) => (a as f64, b),
            (Value::Number(a), Value::Integer(b)) => (a, b as f64),
            _ => return None,
        };

        Some(Value::safe_div(a, b, &|a, b| {
            Value::Number((a / b).floor())
        }))
    }

    // When given a % b, lua computes the remainder, not the modulo.
    // However, Rust computes the modulo correctly.  (e.g. -2 % 3 = 1 according to lua, and -2
    // according to Rust.)
    // This is why there is the second step.  Hopefully, the compiler will optimize the extra
    // mod out
    pub fn modulo(self, other: Value<'gc>) -> Option<Value<'gc>> {
        let (a, b) = match (self, other) {
            // n % 0 for integers throws an error
            (Value::Integer(_), Value::Integer(b)) if b == 0 => return None,
            (Value::Integer(a), Value::Integer(b)) => {
                return Some(Value::Integer(((a % b) + b) % b));
            }
            (Value::Number(a), Value::Number(b)) => (a, b),
            (Value::Integer(a), Value::Number(b)) => (a as f64, b),
            (Value::Number(a), Value::Integer(b)) => (a, b as f64),
            _ => return None,
        };

        Some(Value::safe_div(a, b, &|a, b| {
            Value::Number(((a % b) + b) % b)
        }))
    }

    // This operation always returns a Number, even when called by int arguments
    pub fn exponentiate(self, other: Value<'gc>) -> Option<Value<'gc>> {
        let (a, b) = match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => (a as f64, b as f64),
            (Value::Number(a), Value::Number(b)) => (a, b),
            (Value::Integer(a), Value::Number(b)) => (a as f64, b),
            (Value::Number(a), Value::Integer(b)) => (a, b as f64),
            _ => return None,
        };

        // No need for special casing, 0^0 = 1 in both Rust and Lua
        Some(Value::Number(a.powf(b)))
    }

    pub fn unary_negate(self) -> Option<Value<'gc>> {
        match self {
            Value::Integer(a) => Some(Value::Integer(-a)),
            Value::Number(a) => Some(Value::Number(-a)),
            _ => None,
        }
    }

    pub fn less_than(self, other: Value<'gc>) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a < b),
            (Value::Number(a), Value::Number(b)) => Some(a < b),
            (Value::Integer(a), Value::Number(b)) => Some((a as f64) < b),
            (Value::Number(a), Value::Integer(b)) => Some(a < (b as f64)),
            _ => None,
        }
    }

    pub fn display<W: io::Write>(self, mut w: W) -> Result<(), io::Error> {
        match self {
            Value::Nil => write!(w, "nil"),
            Value::Boolean(b) => write!(w, "{}", b),
            Value::Integer(i) => write!(w, "{}", i),
            Value::Number(f) => write!(w, "{}", f),
            Value::String(s) => w.write_all(s.as_bytes()),
            Value::Table(t) => write!(w, "<table {:?}>", t.0.as_ptr()),
            Value::Function(Function::Closure(c)) => write!(w, "<function {:?}>", Gc::as_ptr(c.0)),
            Value::Function(Function::Callback(c)) => write!(w, "<function {:?}>", Gc::as_ptr(c.0)),
            Value::Thread(t) => write!(w, "<thread {:?}>", GcCell::as_ptr(t.0)),
        }
    }
}

impl<'gc> From<bool> for Value<'gc> {
    fn from(v: bool) -> Value<'gc> {
        Value::Boolean(v)
    }
}

impl<'gc> From<i64> for Value<'gc> {
    fn from(v: i64) -> Value<'gc> {
        Value::Integer(v)
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(v: f64) -> Value<'gc> {
        Value::Number(v)
    }
}

impl<'gc> From<String<'gc>> for Value<'gc> {
    fn from(v: String<'gc>) -> Value<'gc> {
        Value::String(v)
    }
}

impl<'gc> From<Table<'gc>> for Value<'gc> {
    fn from(v: Table<'gc>) -> Value<'gc> {
        Value::Table(v)
    }
}

impl<'gc> From<Function<'gc>> for Value<'gc> {
    fn from(v: Function<'gc>) -> Value<'gc> {
        Value::Function(v)
    }
}

impl<'gc> From<Closure<'gc>> for Value<'gc> {
    fn from(v: Closure<'gc>) -> Value<'gc> {
        Value::Function(Function::Closure(v))
    }
}

impl<'gc> From<Callback<'gc>> for Value<'gc> {
    fn from(v: Callback<'gc>) -> Value<'gc> {
        Value::Function(Function::Callback(v))
    }
}
