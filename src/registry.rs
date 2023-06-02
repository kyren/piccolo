use std::{any::TypeId, collections::hash_map, fmt};

use gc_arena::{lock::RefLock, Collect, DynamicRoot, DynamicRootSet, Gc, Mutation, Root, Rootable};
use rustc_hash::FxHashMap;

use crate::{
    any::AnyValue, AnyCallback, AnyUserData, Closure, Function, String, Table, Thread, Value,
};

#[derive(Clone)]
pub struct StaticTable(pub DynamicRoot<Rootable![Table<'gc>]>);

impl fmt::Debug for StaticTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticTable")
            .field(&self.0.as_ptr())
            .finish()
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

#[derive(Clone)]
pub struct StaticCallback(pub DynamicRoot<Rootable![AnyCallback<'gc>]>);

impl fmt::Debug for StaticCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticCallback")
            .field(&self.0.as_ptr())
            .finish()
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

#[derive(Clone)]
pub struct StaticUserData(pub DynamicRoot<Rootable![AnyUserData<'gc>]>);

impl fmt::Debug for StaticUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StaticString(pub DynamicRoot<Rootable![String<'gc>]>);

impl fmt::Debug for StaticString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticString")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Debug, Clone)]
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
            _ => None,
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
    singletons: Gc<'gc, RefLock<FxHashMap<TypeId, AnyValue<'gc, ()>>>>,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            roots: DynamicRootSet::new(mc),
            singletons: Gc::new(mc, RefLock::new(FxHashMap::default())),
        }
    }

    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    pub fn singleton<R: for<'a> Rootable<'a>>(
        &self,
        mc: &Mutation<'gc>,
        init: impl FnOnce() -> Root<'gc, R>,
    ) -> Root<'gc, R>
    where
        Root<'gc, R>: Copy,
    {
        let mut singletons = self.singletons.borrow_mut(mc);
        match singletons.entry(TypeId::of::<R>()) {
            hash_map::Entry::Occupied(occupied) => *occupied
                .get()
                .downcast::<R>()
                .expect("bad type in singletons table"),
            hash_map::Entry::Vacant(vacant) => {
                let v = init();
                vacant.insert(AnyValue::new::<R>(mc, (), v));
                v
            }
        }
    }

    pub fn stash<R: Stashable<'gc>>(&self, mc: &Mutation<'gc>, r: R) -> R::Stashed {
        r.stash(&self.roots, mc)
    }

    pub fn fetch<F: Fetchable<'gc>>(&self, f: &F) -> F::Fetched {
        f.fetch(&self.roots)
    }
}

pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed;
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
                mc: &Mutation<'gc>,
            ) -> Self::Stashed {
                $r(roots.stash::<Rootable!['a => $t<'a>]>(mc, self))
            }
        }
    };
}

reg_type!(String, StaticString);
reg_type!(Table, StaticTable);
reg_type!(Closure, StaticClosure);
reg_type!(AnyCallback, StaticCallback);
reg_type!(Thread, StaticThread);
reg_type!(AnyUserData, StaticUserData);

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

fetch_type!(StaticString, String);
fetch_type!(StaticTable, Table);
fetch_type!(StaticClosure, Closure);
fetch_type!(StaticCallback, AnyCallback);
fetch_type!(StaticThread, Thread);
fetch_type!(StaticUserData, AnyUserData);

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StaticFunction;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
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

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
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
