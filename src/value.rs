use std::{f64, fmt, i64};

use gc_arena::{Collect, Gc};

use crate::{Callback, Closure, Constant, Function, String, Table, Thread, UserData};

/// The single data type for all Lua variables.
///
/// Every value that Lua code can manipulate directly is ultimately a some kind of `Value`.
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

    /// Returns a proxy object which can display any `Value`.
    ///
    /// [`Value::Nil`] is printed as "nil", booleans, integers, and numbers are always printed as
    /// directly as they would be from Rust.
    ///
    /// [`Value::String`] is printed using the [`String::display_lossy`] method, which displays
    /// strings in a lossy fashion if they are not UTF-8 internally.
    ///
    /// [`Value::Table`]s, [`Value::Function`]s, [`Value::Thread`]s, and [`Value::UserData`]
    /// are all printed as `"<typename {:p}>"`, where 'typename' is the value returned by
    /// [`Value::type_name`].
    pub fn display(self) -> impl fmt::Display + 'gc {
        struct ValueDisplay<'gc>(Value<'gc>);

        impl<'gc> fmt::Display for ValueDisplay<'gc> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    Value::Nil => write!(fmt, "nil"),
                    Value::Boolean(b) => write!(fmt, "{}", b),
                    Value::Integer(i) => write!(fmt, "{}", i),
                    Value::Number(f) => write!(fmt, "{}", f),
                    Value::String(s) => write!(fmt, "{}", s.display_lossy()),
                    Value::Table(t) => write!(fmt, "<table {:p}>", Gc::as_ptr(t.into_inner())),
                    Value::Function(Function::Closure(c)) => {
                        write!(fmt, "<function {:p}>", Gc::as_ptr(c.into_inner()))
                    }
                    Value::Function(Function::Callback(c)) => {
                        write!(fmt, "<function {:p}>", Gc::as_ptr(c.into_inner()))
                    }
                    Value::Thread(t) => write!(fmt, "<thread {:p}>", Gc::as_ptr(t.into_inner())),
                    Value::UserData(u) => {
                        write!(fmt, "<userdata {:p}>", Gc::as_ptr(u.into_inner()))
                    }
                }
            }
        }

        ValueDisplay(self)
    }

    pub fn debug_shallow(self) -> impl fmt::Debug + 'gc {
        struct ShallowDebug<'gc>(Value<'gc>);

        impl<'gc> fmt::Debug for ShallowDebug<'gc> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    Value::Table(t) => {
                        write!(fmt, "Value::Table({:p})", Gc::as_ptr(t.into_inner()))
                    }
                    Value::Function(Function::Closure(c)) => {
                        write!(
                            fmt,
                            "Value::Function(Function::Closure({:p}))",
                            Gc::as_ptr(c.into_inner())
                        )
                    }
                    Value::Function(Function::Callback(c)) => {
                        write!(
                            fmt,
                            "Value::Function(Function::Callback({:p}))",
                            Gc::as_ptr(c.into_inner())
                        )
                    }
                    Value::Thread(t) => {
                        write!(fmt, "Value::Thread({:p})", Gc::as_ptr(t.into_inner()))
                    }
                    Value::UserData(u) => {
                        write!(fmt, "Value::UserData({:p})", Gc::as_ptr(u.into_inner()))
                    }
                    v => write!(fmt, "{v:?}"),
                }
            }
        }

        ShallowDebug(self)
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

    /// Interprets Numbers, Integers, and Strings as a String, otherwise returns None.
    ///
    /// If the value is a [`Value::String`], the string is returned directly. Otherwise, the
    /// returned string will always be the same as what [`Value::display`] would display.
    pub fn into_string(self, ctx: crate::Context<'gc>) -> Option<String<'gc>> {
        match self {
            Value::Integer(i) => Some(ctx.intern(i.to_string().as_bytes())),
            Value::Number(n) => Some(ctx.intern(n.to_string().as_bytes())),
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Indicates whether the value can be implicitly converted to a [`String`]; if so,
    /// [`Value::into_string`] will always return `Some`.
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
