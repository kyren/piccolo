use std::{f64, fmt, i64, io, string::String as StdString};

use gc_arena::{Collect, Gc};

use crate::{Callback, Closure, Constant, Function, String, Table, Thread, UserData};

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

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::Nil
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

    pub fn write<W: io::Write>(self, mut w: W) -> Result<(), io::Error> {
        match self {
            Value::Nil => write!(w, "nil"),
            Value::Boolean(b) => write!(w, "{}", b),
            Value::Integer(i) => write!(w, "{}", i),
            Value::Number(f) => write!(w, "{}", f),
            Value::String(s) => w.write_all(s.as_bytes()),
            Value::Table(t) => write!(w, "<table {:p}>", Gc::as_ptr(t.into_inner())),
            Value::Function(Function::Closure(c)) => {
                write!(w, "<function {:p}>", Gc::as_ptr(c.into_inner()))
            }
            Value::Function(Function::Callback(c)) => {
                write!(w, "<function {:p}>", Gc::as_ptr(c.into_inner()))
            }
            Value::Thread(t) => write!(w, "<thread {:p}>", Gc::as_ptr(t.into_inner())),
            Value::UserData(u) => write!(w, "<userdata {:p}>", Gc::as_ptr(u.into_inner())),
        }
    }
    pub fn display(self) -> impl fmt::Display + 'gc {
        ValueDisplay(self)
    }

    pub fn is_nil(self) -> bool {
        matches!(self, Value::Nil)
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

    /// Converts value to either a Number or an Integer, if possible.
    pub fn to_numeric(self) -> Option<Self> {
        self.to_constant()
            .and_then(|c| c.to_numeric())
            .map(|c| c.into())
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
    pub fn into_string(self, ctx: crate::Context<'gc>) -> Option<String<'gc>> {
        match self {
            Value::Integer(i) => Some(ctx.intern(i.to_string().as_bytes())),
            Value::Number(n) => Some(ctx.intern(n.to_string().as_bytes())),
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Indicates whether the value can be implicitly converted to a String;
    /// if so, [`Value::into_string`] will return `Some` with the same result
    /// that [`Value::write`] will output.
    ///
    /// Note that [`Value::display`] may not result in the same output, when
    /// handling non-utf8 strings.
    pub fn is_implicit_string(self) -> bool {
        match self {
            Value::Integer(_) => true,
            Value::Number(_) => true,
            Value::String(_) => true,
            _ => false,
        }
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

struct ValueDisplay<'gc>(Value<'gc>);

impl<'gc> fmt::Display for ValueDisplay<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Value::Nil => write!(fmt, "nil"),
            Value::Boolean(b) => write!(fmt, "{}", b),
            Value::Integer(i) => write!(fmt, "{}", i),
            Value::Number(f) => write!(fmt, "{}", f),
            Value::String(s) => write!(fmt, "{}", StdString::from_utf8_lossy(&s)),
            Value::Table(t) => write!(fmt, "<table {:p}>", Gc::as_ptr(t.into_inner())),
            Value::Function(Function::Closure(c)) => {
                write!(fmt, "<function {:p}>", Gc::as_ptr(c.into_inner()))
            }
            Value::Function(Function::Callback(c)) => {
                write!(fmt, "<function {:p}>", Gc::as_ptr(c.into_inner()))
            }
            Value::Thread(t) => write!(fmt, "<thread {:p}>", Gc::as_ptr(t.into_inner())),
            Value::UserData(u) => write!(fmt, "<userdata {:p}>", Gc::as_ptr(u.into_inner())),
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

impl<'gc> From<Callback<'gc>> for Value<'gc> {
    fn from(v: Callback<'gc>) -> Value<'gc> {
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
