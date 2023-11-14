use std::{any::TypeId, fmt, hash::BuildHasherDefault};

use gc_arena::{
    allocator_api::MetricsAlloc, lock::RefLock, Collect, DynamicRoot, DynamicRootSet, Gc, Mutation,
    Root, Rootable,
};
use hashbrown::{hash_map, HashMap};
use rustc_hash::FxHasher;

use crate::{
    any::AnyValue, AnyCallback, AnyUserData, Closure, Context, Executor, Function, String, Table,
    Thread, Value,
};

#[derive(Clone)]
pub struct StashedTable(pub DynamicRoot<Rootable![Table<'_>]>);

impl fmt::Debug for StashedTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedTable")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedClosure(pub DynamicRoot<Rootable![Closure<'_>]>);

impl fmt::Debug for StashedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedCallback(pub DynamicRoot<Rootable![AnyCallback<'_>]>);

impl fmt::Debug for StashedCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedCallback")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedThread(pub DynamicRoot<Rootable![Thread<'_>]>);

impl fmt::Debug for StashedThread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedThread")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedUserData(pub DynamicRoot<Rootable![AnyUserData<'_>]>);

impl fmt::Debug for StashedUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedString(pub DynamicRoot<Rootable![String<'_>]>);

impl fmt::Debug for StashedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedString")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StashedExecutor(pub DynamicRoot<Rootable![Executor<'_>]>);

impl fmt::Debug for StashedExecutor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedExecutor")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum StashedFunction {
    Closure(StashedClosure),
    Callback(StashedCallback),
}

impl From<StashedClosure> for StashedFunction {
    fn from(closure: StashedClosure) -> Self {
        Self::Closure(closure)
    }
}

impl From<StashedCallback> for StashedFunction {
    fn from(callback: StashedCallback) -> Self {
        Self::Callback(callback)
    }
}

#[derive(Debug, Clone)]
pub enum StaticValue {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(StashedString),
    Table(StashedTable),
    Function(StashedFunction),
    Thread(StashedThread),
    UserData(StashedUserData),
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

impl From<StashedString> for StaticValue {
    fn from(v: StashedString) -> StaticValue {
        StaticValue::String(v)
    }
}

impl From<StashedTable> for StaticValue {
    fn from(v: StashedTable) -> StaticValue {
        StaticValue::Table(v)
    }
}

impl From<StashedFunction> for StaticValue {
    fn from(v: StashedFunction) -> StaticValue {
        StaticValue::Function(v)
    }
}

impl From<StashedClosure> for StaticValue {
    fn from(v: StashedClosure) -> StaticValue {
        StaticValue::Function(StashedFunction::Closure(v))
    }
}

impl From<StashedCallback> for StaticValue {
    fn from(v: StashedCallback) -> StaticValue {
        StaticValue::Function(StashedFunction::Callback(v))
    }
}

impl From<StashedUserData> for StaticValue {
    fn from(v: StashedUserData) -> StaticValue {
        StaticValue::UserData(v)
    }
}

pub trait Singleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self;
}

impl<'gc, T: Default> Singleton<'gc> for T {
    fn create(_: Context<'gc>) -> Self {
        Self::default()
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Registry<'gc> {
    roots: DynamicRootSet<'gc>,
    singletons: Gc<
        'gc,
        RefLock<
            HashMap<TypeId, AnyValue<'gc, ()>, BuildHasherDefault<FxHasher>, MetricsAlloc<'gc>>,
        >,
    >,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        let singletons =
            HashMap::with_hasher_in(BuildHasherDefault::default(), MetricsAlloc::new(mc));

        Self {
            roots: DynamicRootSet::new(mc),
            singletons: Gc::new(mc, RefLock::new(singletons)),
        }
    }

    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    pub fn singleton<S>(&self, ctx: Context<'gc>) -> &'gc Root<'gc, S>
    where
        S: for<'a> Rootable<'a>,
        Root<'gc, S>: Singleton<'gc>,
    {
        let mut singletons = self.singletons.borrow_mut(&ctx);
        match singletons.entry(TypeId::of::<S>()) {
            hash_map::Entry::Occupied(occupied) => occupied.get().downcast::<S>().unwrap(),
            hash_map::Entry::Vacant(vacant) => {
                let v = Root::<'gc, S>::create(ctx);
                vacant
                    .insert(AnyValue::new::<S>(&ctx, (), v))
                    .downcast::<S>()
                    .unwrap()
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

            fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
                $r(roots.stash::<Rootable![$t<'_>]>(mc, self))
            }
        }
    };
}

reg_type!(String, StashedString);
reg_type!(Table, StashedTable);
reg_type!(Closure, StashedClosure);
reg_type!(AnyCallback, StashedCallback);
reg_type!(Thread, StashedThread);
reg_type!(AnyUserData, StashedUserData);
reg_type!(Executor, StashedExecutor);

macro_rules! fetch_type {
    ($r:ident, $t:ident) => {
        impl<'gc> Fetchable<'gc> for $r {
            type Fetched = $t<'gc>;

            fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
                *roots.fetch::<Rootable![$t<'_>]>(&self.0)
            }
        }
    };
}

fetch_type!(StashedString, String);
fetch_type!(StashedTable, Table);
fetch_type!(StashedClosure, Closure);
fetch_type!(StashedCallback, AnyCallback);
fetch_type!(StashedThread, Thread);
fetch_type!(StashedUserData, AnyUserData);
fetch_type!(StashedExecutor, Executor);

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StashedFunction;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
        match self {
            Function::Closure(closure) => StashedFunction::Closure(closure.stash(roots, mc)),
            Function::Callback(callback) => StashedFunction::Callback(callback.stash(roots, mc)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StashedFunction {
    type Fetched = Function<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StashedFunction::Closure(closure) => Function::Closure(closure.fetch(roots)),
            StashedFunction::Callback(callback) => Function::Callback(callback.fetch(roots)),
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
