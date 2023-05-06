use std::fmt;

use gc_arena::{Collect, DynamicRoot, DynamicRootSet, MutationContext, Rootable};

use crate::{Callback, Closure, Function, String, Table, Thread, UserData, Value};

#[derive(Clone)]
pub struct StaticTable(pub DynamicRoot<Rootable![Table<'gc>]>);

impl fmt::Debug for StaticTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticTable")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl PartialEq for StaticTable {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone)]
pub struct StaticClosure(pub DynamicRoot<Rootable![Closure<'gc>]>);

impl fmt::Debug for StaticClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl PartialEq for StaticClosure {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone)]
pub struct StaticCallback(pub DynamicRoot<Rootable![Callback<'gc>]>);

impl fmt::Debug for StaticCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticCallback")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl PartialEq for StaticCallback {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone)]
pub struct StaticThread(pub DynamicRoot<Rootable![Thread<'gc>]>);

impl fmt::Debug for StaticThread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticThread")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl PartialEq for StaticThread {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone)]
pub struct StaticUserData(pub DynamicRoot<Rootable![UserData<'gc>]>);

impl fmt::Debug for StaticUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl PartialEq for StaticUserData {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone)]
pub enum StaticString {
    Static(&'static [u8]),
    Rooted(DynamicRoot<Rootable![String<'gc>]>),
}

impl fmt::Debug for StaticString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StaticString::Static(s) => f.debug_tuple("StaticString::Static").field(s).finish(),
            StaticString::Rooted(s) => f
                .debug_tuple("StaticString::Rooted")
                .field(&s.as_ptr())
                .finish(),
        }
    }
}

impl PartialEq for StaticString {
    /// This does not semantically match equality for normal strings! This is a best effort test!
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StaticString::Static(a), StaticString::Static(b)) => a == b,
            (StaticString::Rooted(a), StaticString::Rooted(b)) => a.as_ptr() == b.as_ptr(),
            _ => false,
        }
    }
}

impl From<&'static str> for StaticString {
    fn from(s: &'static str) -> Self {
        Self::Static(s.as_bytes())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StaticFunction {
    Closure(StaticClosure),
    Callback(StaticCallback),
}

impl From<StaticClosure> for StaticFunction {
    fn from(closure: StaticClosure) -> Self {
        Self::Closure(closure)
    }
}

impl From<StaticCallback> for StaticFunction {
    fn from(callback: StaticCallback) -> Self {
        Self::Callback(callback)
    }
}

#[derive(Debug, Clone)]
pub enum StaticValue {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(StaticString),
    Table(StaticTable),
    Function(StaticFunction),
    Thread(StaticThread),
    UserData(StaticUserData),
}

impl StaticValue {
    pub fn to_bool(self) -> bool {
        match self {
            StaticValue::Nil => false,
            StaticValue::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn as_primitive<'gc>(&self) -> Option<Value<'gc>> {
        match self {
            StaticValue::Nil => Some(Value::Nil),
            StaticValue::Boolean(b) => Some(Value::Boolean(*b)),
            StaticValue::Integer(i) => Some(Value::Integer(*i)),
            StaticValue::Number(n) => Some(Value::Number(*n)),
            StaticValue::String(StaticString::Static(s)) => Some(Value::String(String::Static(s))),
            _ => None,
        }
    }
}

impl PartialEq for StaticValue {
    fn eq(&self, other: &StaticValue) -> bool {
        if let (Some(a), Some(b)) = (self.as_primitive(), other.as_primitive()) {
            a == b
        } else {
            match (self, other) {
                (StaticValue::String(a), StaticValue::String(b)) => a == b,
                (StaticValue::Table(a), StaticValue::Table(b)) => a == b,
                (StaticValue::Function(a), StaticValue::Function(b)) => a == b,
                (StaticValue::Thread(a), StaticValue::Thread(b)) => a == b,
                (StaticValue::UserData(a), StaticValue::UserData(b)) => a == b,
                _ => false,
            }
        }
    }
}

impl From<bool> for StaticValue {
    fn from(v: bool) -> StaticValue {
        StaticValue::Boolean(v)
    }
}

impl From<i64> for StaticValue {
    fn from(v: i64) -> StaticValue {
        StaticValue::Integer(v)
    }
}

impl From<f64> for StaticValue {
    fn from(v: f64) -> StaticValue {
        StaticValue::Number(v)
    }
}

impl From<&'static str> for StaticValue {
    fn from(v: &'static str) -> StaticValue {
        StaticValue::String(StaticString::Static(v.as_bytes()))
    }
}

impl From<StaticString> for StaticValue {
    fn from(v: StaticString) -> StaticValue {
        StaticValue::String(v)
    }
}

impl From<StaticTable> for StaticValue {
    fn from(v: StaticTable) -> StaticValue {
        StaticValue::Table(v)
    }
}

impl From<StaticFunction> for StaticValue {
    fn from(v: StaticFunction) -> StaticValue {
        StaticValue::Function(v)
    }
}

impl From<StaticClosure> for StaticValue {
    fn from(v: StaticClosure) -> StaticValue {
        StaticValue::Function(StaticFunction::Closure(v))
    }
}

impl From<StaticCallback> for StaticValue {
    fn from(v: StaticCallback) -> StaticValue {
        StaticValue::Function(StaticFunction::Callback(v))
    }
}

impl From<StaticUserData> for StaticValue {
    fn from(v: StaticUserData) -> StaticValue {
        StaticValue::UserData(v)
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Registry<'gc> {
    roots: DynamicRootSet<'gc>,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        Self {
            roots: DynamicRootSet::new(mc),
        }
    }

    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    pub fn stash<R: Stashable<'gc>>(&self, mc: MutationContext<'gc, '_>, r: R) -> R::Stashed {
        r.stash(&self.roots, mc)
    }

    pub fn fetch<F: Fetchable<'gc>>(&self, f: &F) -> F::Fetched {
        f.fetch(&self.roots)
    }
}

pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: MutationContext<'gc, '_>) -> Self::Stashed;
}

pub trait Fetchable<'gc> {
    type Fetched;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched;
}

macro_rules! reg_type {
    ($t:ident, $r:ident) => {
        impl<'gc> Stashable<'gc> for $t<'gc> {
            type Stashed = $r;

            fn stash(
                self,
                roots: &DynamicRootSet<'gc>,
                mc: MutationContext<'gc, '_>,
            ) -> Self::Stashed {
                $r(roots.stash::<Rootable!['a => $t<'a>]>(mc, self))
            }
        }
    };
}

reg_type!(Table, StaticTable);
reg_type!(Closure, StaticClosure);
reg_type!(Callback, StaticCallback);
reg_type!(Thread, StaticThread);
reg_type!(UserData, StaticUserData);

macro_rules! fetch_type {
    ($r:ident, $t:ident) => {
        impl<'gc> Fetchable<'gc> for $r {
            type Fetched = $t<'gc>;

            fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
                *roots.fetch::<Rootable!['b => $t<'b>]>(&self.0)
            }
        }
    };
}

fetch_type!(StaticTable, Table);
fetch_type!(StaticClosure, Closure);
fetch_type!(StaticCallback, Callback);
fetch_type!(StaticThread, Thread);
fetch_type!(StaticUserData, UserData);

impl<'gc> Stashable<'gc> for String<'gc> {
    type Stashed = StaticString;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: MutationContext<'gc, '_>) -> Self::Stashed {
        match self {
            String::Static(s) => StaticString::Static(s),
            s => StaticString::Rooted(roots.stash::<Rootable!['a => String<'a>]>(mc, s)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StaticString {
    type Fetched = String<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StaticString::Static(s) => String::Static(s),
            StaticString::Rooted(s) => *roots.fetch(s),
        }
    }
}

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StaticFunction;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: MutationContext<'gc, '_>) -> Self::Stashed {
        match self {
            Function::Closure(closure) => StaticFunction::Closure(closure.stash(roots, mc)),
            Function::Callback(callback) => StaticFunction::Callback(callback.stash(roots, mc)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StaticFunction {
    type Fetched = Function<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StaticFunction::Closure(closure) => Function::Closure(closure.fetch(roots)),
            StaticFunction::Callback(callback) => Function::Callback(callback.fetch(roots)),
        }
    }
}

impl<'gc> Stashable<'gc> for Value<'gc> {
    type Stashed = StaticValue;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: MutationContext<'gc, '_>) -> Self::Stashed {
        match self {
            Value::Nil => StaticValue::Nil,
            Value::Boolean(b) => StaticValue::Boolean(b),
            Value::Integer(i) => StaticValue::Integer(i),
            Value::Number(n) => StaticValue::Number(n),
            Value::String(s) => StaticValue::String(s.stash(roots, mc)),
            Value::Table(t) => StaticValue::Table(t.stash(roots, mc)),
            Value::Function(f) => StaticValue::Function(f.stash(roots, mc)),
            Value::Thread(t) => StaticValue::Thread(t.stash(roots, mc)),
            Value::UserData(u) => StaticValue::UserData(u.stash(roots, mc)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StaticValue {
    type Fetched = Value<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StaticValue::Nil => Value::Nil,
            StaticValue::Boolean(b) => Value::Boolean(*b),
            StaticValue::Integer(i) => Value::Integer(*i),
            StaticValue::Number(n) => Value::Number(*n),
            StaticValue::String(s) => Value::String(s.fetch(roots)),
            StaticValue::Table(t) => Value::Table(t.fetch(roots)),
            StaticValue::Function(f) => Value::Function(f.fetch(roots)),
            StaticValue::Thread(t) => Value::Thread(t.fetch(roots)),
            StaticValue::UserData(u) => Value::UserData(u.fetch(roots)),
        }
    }
}
