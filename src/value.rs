use std::{i64, io};

use gc_arena::{Collect, Gc};

use crate::{Callback, Closure, String, Table};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub enum Value<'gc> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(String<'gc>),
    Table(Table<'gc>),
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
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

            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::Closure(_), _) => false,

            (Value::Callback(a), Value::Callback(b)) => a == b,
            (Value::Callback(_), _) => false,
        }
    }
}

impl<'gc> Value<'gc> {
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
            Value::Closure(c) => write!(w, "<closure {:?}>", Gc::as_ptr(&c.0)),
            Value::Callback(c) => write!(w, "<callback {:?}>", Gc::as_ptr(&c.0)),
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

impl<'gc> From<Closure<'gc>> for Value<'gc> {
    fn from(v: Closure<'gc>) -> Value<'gc> {
        Value::Closure(v)
    }
}

impl<'gc> From<Callback<'gc>> for Value<'gc> {
    fn from(v: Callback<'gc>) -> Value<'gc> {
        Value::Callback(v)
    }
}
