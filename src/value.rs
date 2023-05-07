use std::{f64, i64, io};

use gc_arena::{Collect, Gc, MutationContext};

use crate::{
    lexer::{read_float, read_hex_float},
    AnyCallback, Closure, String, Table, Thread, UserData,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(AnyCallback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<AnyCallback<'gc>> for Function<'gc> {
    fn from(callback: AnyCallback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(String<'gc>),
    Table(Table<'gc>),
    Function(Function<'gc>),
    Thread(Thread<'gc>),
    UserData(UserData<'gc>),
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

            (Value::UserData(a), Value::UserData(b)) => a == b,
            (Value::UserData(_), _) => false,
        }
    }
}

impl<'gc> Value<'gc> {
    pub fn type_name(self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Boolean(_) => "boolean",
            Value::Integer(_) | Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Table(_) => "table",
            Value::Function(_) => "function",
            Value::Thread(_) => "thread",
            Value::UserData(_) => "userdata",
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
            Value::Thread(t) => write!(w, "<thread {:?}>", t.0.as_ptr()),
            Value::UserData(t) => write!(w, "<userdata {:?}>", t.0.as_ptr()),
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

    /// Interprets Numbers, Integers, and Strings as a Number, if possible.
    pub fn to_number(self) -> Option<f64> {
        match self {
            Value::Integer(a) => Some(a as f64),
            Value::Number(a) => Some(a),
            Value::String(a) => {
                if let Some(f) = read_hex_float(&a) {
                    Some(f)
                } else {
                    read_float(&a)
                }
            }
            _ => None,
        }
    }

    /// Interprets Numbers, Integers, and Strings as an Integer, if possible.
    pub fn to_integer(self) -> Option<i64> {
        match self {
            Value::Integer(a) => Some(a),
            Value::Number(a) => {
                if ((a as i64) as f64) == a {
                    Some(a as i64)
                } else {
                    None
                }
            }
            Value::String(a) => match if let Some(f) = read_hex_float(&a) {
                Some(f)
            } else {
                read_float(&a)
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

    /// Interprets Numbers, Integers, and Strings as a String, if possible.
    pub fn to_string(self, mc: MutationContext<'gc, '_>) -> Option<String<'gc>> {
        match self {
            Value::Integer(a) => Some(String::concat(mc, &[Value::Integer(a)]).unwrap()),
            Value::Number(a) => Some(String::concat(mc, &[Value::Number(a)]).unwrap()),
            Value::String(a) => Some(a),
            _ => None,
        }
    }

    pub fn not(self) -> Value<'gc> {
        Value::Boolean(!self.to_bool())
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

impl<'gc> From<&'static str> for Value<'gc> {
    fn from(v: &'static str) -> Value<'gc> {
        Value::String(String::from(v))
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

impl<'gc> From<AnyCallback<'gc>> for Value<'gc> {
    fn from(v: AnyCallback<'gc>) -> Value<'gc> {
        Value::Function(Function::Callback(v))
    }
}

impl<'gc> From<Thread<'gc>> for Value<'gc> {
    fn from(v: Thread<'gc>) -> Value<'gc> {
        Value::Thread(v)
    }
}

impl<'gc> From<UserData<'gc>> for Value<'gc> {
    fn from(v: UserData<'gc>) -> Value<'gc> {
        Value::UserData(v)
    }
}
