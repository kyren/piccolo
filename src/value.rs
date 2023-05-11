use std::{f64, fmt, i64, io, string::String as StdString};

use gc_arena::{Collect, MutationContext};

use crate::{AnyCallback, AnyUserData, Closure, Constant, Function, String, Table, Thread};

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
    UserData(AnyUserData<'gc>),
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::Nil
    }
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
            Value::Table(t) => write!(w, "<table {:p}>", t.0),
            Value::Function(Function::Closure(c)) => write!(w, "<function {:p}>", c.0),
            Value::Function(Function::Callback(c)) => write!(w, "<function {:p}>", c.as_ptr()),
            Value::Thread(t) => write!(w, "<thread {:p}>", t.0),
            Value::UserData(t) => write!(w, "<userdata {:p}>", t.0.as_ptr()),
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
        self.to_constant().and_then(|c| c.to_number())
    }

    /// Interprets Numbers, Integers, and Strings as an Integer, if possible.
    pub fn to_integer(self) -> Option<i64> {
        self.to_constant().and_then(|c| c.to_integer())
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

    pub fn to_constant(self) -> Option<Constant<String<'gc>>> {
        match self {
            Value::Nil => Some(Constant::Nil),
            Value::Boolean(b) => Some(Constant::Boolean(b)),
            Value::Integer(i) => Some(Constant::Integer(i)),
            Value::Number(n) => Some(Constant::Number(n)),
            Value::String(s) => Some(Constant::String(s)),
            _ => None,
        }
    }
}

impl<'gc> fmt::Display for Value<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = Vec::new();
        self.display(&mut buf).unwrap();
        let s = StdString::from_utf8_lossy(&buf);
        write!(fmt, "{}", s)
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

impl<'gc, S> From<Constant<S>> for Value<'gc>
where
    String<'gc>: From<S>,
{
    fn from(constant: Constant<S>) -> Self {
        match constant {
            Constant::Nil => Value::Nil,
            Constant::Boolean(b) => Value::Boolean(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Number(n) => Value::Number(n),
            Constant::String(s) => Value::String(s.into()),
        }
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

impl<'gc> From<AnyUserData<'gc>> for Value<'gc> {
    fn from(v: AnyUserData<'gc>) -> Value<'gc> {
        Value::UserData(v)
    }
}

pub trait IntoValue<'gc> {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc>;
}

impl<'gc, T> IntoValue<'gc> for T
where
    T: Into<Value<'gc>>,
{
    fn into_value(self, _mc: MutationContext<'gc, '_>) -> Value<'gc> {
        self.into()
    }
}

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        Value::String(String::from_static(mc, self.as_bytes()))
    }
}
