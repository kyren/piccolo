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

    /// Uses native Rust function "parse," which has not been evaluated yet as being correct in the
    /// context of parsing lua numbers.
    pub fn to_number(self) -> Option<f64> {
        match self {
            Value::Integer(a) => Some(a as f64),
            Value::Number(a) => Some(a),
            Value::String(a) => std::string::String::from_utf8(a.as_bytes().to_vec())
                .ok()
                .and_then(|a| a.parse::<f64>().ok()),
            _ => None,
        }
    }

    /// Only Integers can become integers, unfortunately.
    pub fn to_integer(self) -> Option<i64> {
        match self {
            Value::Integer(a) => Some(a),
            _ => None,
        }
    }

    pub fn not(self) -> Value<'gc> {
        Value::Boolean(!self.to_bool())
    }

    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        bin_op(
            self,
            other,
            |a, b| Value::Integer(a.wrapping_add(b)),
            |a, b| Value::Number(a + b),
        )
    }

    pub fn subtract(self, other: Value<'gc>) -> Option<Value<'gc>> {
        bin_op(
            self,
            other,
            |a, b| Value::Integer(a.wrapping_sub(b)),
            |a, b| Value::Number(a - b),
        )
    }

    pub fn multiply(self, other: Value<'gc>) -> Option<Value<'gc>> {
        bin_op(
            self,
            other,
            |a, b| Value::Integer(a.wrapping_mul(b)),
            |a, b| Value::Number(a * b),
        )
    }

    /// This operation always returns a Number, even when called by int arguments
    pub fn float_divide(self, other: Value<'gc>) -> Option<Value<'gc>> {
        bin_op(
            self,
            other,
            |a, b| safe_div(a, b, |a, b| Value::Number(a as f64 / b as f64)),
            |a, b| safe_div(a, b, |a, b| Value::Number(a / b)),
        )
    }

    /// This operation returns an Integer only if both arguments are integers
    /// Rounding is towards negative infinity
    pub fn floor_divide(self, other: Value<'gc>) -> Option<Value<'gc>> {
        bin_op(
            self,
            other,
            |a, b| safe_div(a, b, |a, b| Value::Integer(a.wrapping_div(b))),
            |a, b| safe_div(a, b, |a, b| Value::Number((a / b).floor())),
        )
    }

    /// When given a % b, lua computes the modulo, not the remainder.
    /// However, Rust computes the remainder.  (e.g. -2 % 3 = 1 according to lua, and -2
    /// according to Rust.)
    /// This is why there is the second step.  Hopefully, the compiler will optimize the extra
    /// mod out
    pub fn modulo(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if self.to_integer().is_some() && other.to_integer().unwrap_or(1) == 0 {
            return None;
        }

        if self.to_number().is_some() && other.to_number().unwrap_or(1.0) == 0.0 {
            return Some(Value::Number(-f64::NAN));
        }

        bin_op(
            self,
            other,
            |a, b| safe_div(a, b, |a, b| Value::Integer(((a % b) + b) % b)),
            |a, b| safe_div(a, b, |a, b| Value::Number(((a % b) + b) % b)),
        )
    }

    /// This operation always returns a Number, even when called by int arguments
    pub fn exponentiate(self, other: Value<'gc>) -> Option<Value<'gc>> {
        // No need for special casing, 0^0 = 1 in both Rust and Lua
        bin_op(
            self,
            other,
            |a, b| Value::Number((a as f64).powf(b as f64)),
            |a, b| Value::Number(a.powf(b)),
        )
    }

    pub fn unary_negate(self) -> Option<Value<'gc>> {
        match self {
            Value::Integer(a) => Some(Value::Integer(-a)),
            Value::Number(a) => Some(Value::Number(-a)),
            _ => None,
        }
    }

    pub fn less_than(self, other: Value<'gc>) -> Option<bool> {
        bin_op(self, other, |a, b| a < b, |a, b| a < b)
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

// In the future will be able to use f64::copysign
// See https://github.com/rust-lang/rust/issues/58046
fn copysign(to: f64, from: f64) -> f64 {
    to * if from < 0.0 { -1.0 } else { 1.0 }
}

fn bin_op<'gc, U, F, G>(lhs: Value<'gc>, rhs: Value<'gc>, ifun: F, ffun: G) -> Option<U>
where
    F: Fn(i64, i64) -> U,
    G: Fn(f64, f64) -> U,
{
    match (lhs.to_integer(), rhs.to_integer()) {
        (Some(a), Some(b)) => Some(ifun(a, b)),
        _ => match (lhs.to_number(), rhs.to_number()) {
            (Some(a), Some(b)) => Some(ffun(a, b)),
            _ => None,
        },
    }
}

// A small helper function to handle division-like zero handling
fn safe_div<'gc, T, F>(lhs: T, rhs: T, f: F) -> Value<'gc>
where
    T: ToPrimitive + Zero,
    F: Fn(T, T) -> Value<'gc>,
{
    if lhs.is_zero() && rhs.is_zero() {
        Value::Number(-f64::NAN)
    } else if rhs.is_zero() {
        Value::Number(copysign(f64::INFINITY, lhs.to_f64().unwrap()))
    } else {
        f(lhs, rhs)
    }
}
