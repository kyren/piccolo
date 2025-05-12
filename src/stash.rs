use core::fmt;

use gc_arena::{DynamicRoot, DynamicRootSet, Mutation, Rootable};

use crate::{
    callback::CallbackInner,
    closure::ClosureInner,
    string::StringInner,
    table::TableInner,
    thread::{ExecutorInner, ThreadInner},
    userdata::UserDataInner,
    Callback, Closure, Error, Executor, Function, RuntimeError, String, Table, Thread, UserData,
    Value,
};

/// A trait for types that can be stashed into a [`DynamicRootSet`].
///
/// This trait is simpler to work with than having to manually specify `Rootable` projections and
/// can work with more types than just those that wrap a single `Gc` pointer.
///
/// It is implemented for all common `piccolo` types, allowing you to stash those types in the
/// registry or inside of async sequences.
pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed;
}

/// A trait for types that can be fetched from a [`DynamicRootSet`].
pub trait Fetchable {
    type Fetched<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc>;
}

#[derive(Clone)]
pub struct StashedString(DynamicRoot<Rootable![StringInner]>);

impl fmt::Debug for StashedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedString")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for String<'gc> {
    type Stashed = StashedString;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedString(roots.stash::<Rootable![StringInner]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedString {
    type Fetched<'gc> = String<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        String::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedTable(DynamicRoot<Rootable![TableInner<'_>]>);

impl fmt::Debug for StashedTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedTable")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Table<'gc> {
    type Stashed = StashedTable;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedTable(roots.stash::<Rootable![TableInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedTable {
    type Fetched<'gc> = Table<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Table::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedClosure(DynamicRoot<Rootable![ClosureInner<'_>]>);

impl fmt::Debug for StashedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Closure<'gc> {
    type Stashed = StashedClosure;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedClosure(roots.stash::<Rootable![ClosureInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedClosure {
    type Fetched<'gc> = Closure<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Closure::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedCallback(DynamicRoot<Rootable![CallbackInner<'_>]>);

impl fmt::Debug for StashedCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedCallback")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Callback<'gc> {
    type Stashed = StashedCallback;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedCallback(roots.stash::<Rootable![CallbackInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedCallback {
    type Fetched<'gc> = Callback<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Callback::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedThread(DynamicRoot<Rootable![ThreadInner<'_>]>);

impl fmt::Debug for StashedThread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedThread")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Thread<'gc> {
    type Stashed = StashedThread;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedThread(roots.stash::<Rootable![ThreadInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedThread {
    type Fetched<'gc> = Thread<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Thread::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedUserData(DynamicRoot<Rootable![UserDataInner<'_>]>);

impl fmt::Debug for StashedUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for UserData<'gc> {
    type Stashed = StashedUserData;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedUserData(roots.stash::<Rootable![UserDataInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedUserData {
    type Fetched<'gc> = UserData<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        UserData::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedExecutor(DynamicRoot<Rootable![ExecutorInner<'_>]>);

impl fmt::Debug for StashedExecutor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedExecutor")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Executor<'gc> {
    type Stashed = StashedExecutor;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedExecutor(roots.stash::<Rootable![ExecutorInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedExecutor {
    type Fetched<'gc> = Executor<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Executor::from_inner(roots.fetch(&self.0))
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

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StashedFunction;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        match self {
            Function::Closure(closure) => StashedFunction::Closure(closure.stash(mc, roots)),
            Function::Callback(callback) => StashedFunction::Callback(callback.stash(mc, roots)),
        }
    }
}

impl Fetchable for StashedFunction {
    type Fetched<'gc> = Function<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedFunction::Closure(closure) => Function::Closure(closure.fetch(roots)),
            StashedFunction::Callback(callback) => Function::Callback(callback.fetch(roots)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StashedValue {
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

impl StashedValue {
    pub fn to_bool(self) -> bool {
        match self {
            StashedValue::Nil => false,
            StashedValue::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn as_primitive<'gc>(&self) -> Option<Value<'gc>> {
        match self {
            StashedValue::Nil => Some(Value::Nil),
            StashedValue::Boolean(b) => Some(Value::Boolean(*b)),
            StashedValue::Integer(i) => Some(Value::Integer(*i)),
            StashedValue::Number(n) => Some(Value::Number(*n)),
            _ => None,
        }
    }
}

impl From<bool> for StashedValue {
    fn from(v: bool) -> StashedValue {
        StashedValue::Boolean(v)
    }
}

impl From<i64> for StashedValue {
    fn from(v: i64) -> StashedValue {
        StashedValue::Integer(v)
    }
}

impl From<f64> for StashedValue {
    fn from(v: f64) -> StashedValue {
        StashedValue::Number(v)
    }
}

impl From<StashedString> for StashedValue {
    fn from(v: StashedString) -> StashedValue {
        StashedValue::String(v)
    }
}

impl From<StashedTable> for StashedValue {
    fn from(v: StashedTable) -> StashedValue {
        StashedValue::Table(v)
    }
}

impl From<StashedFunction> for StashedValue {
    fn from(v: StashedFunction) -> StashedValue {
        StashedValue::Function(v)
    }
}

impl From<StashedClosure> for StashedValue {
    fn from(v: StashedClosure) -> StashedValue {
        StashedValue::Function(StashedFunction::Closure(v))
    }
}

impl From<StashedCallback> for StashedValue {
    fn from(v: StashedCallback) -> StashedValue {
        StashedValue::Function(StashedFunction::Callback(v))
    }
}

impl From<StashedUserData> for StashedValue {
    fn from(v: StashedUserData) -> StashedValue {
        StashedValue::UserData(v)
    }
}

impl<'gc> Stashable<'gc> for Value<'gc> {
    type Stashed = StashedValue;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        match self {
            Value::Nil => StashedValue::Nil,
            Value::Boolean(b) => StashedValue::Boolean(b),
            Value::Integer(i) => StashedValue::Integer(i),
            Value::Number(n) => StashedValue::Number(n),
            Value::String(s) => StashedValue::String(s.stash(mc, roots)),
            Value::Table(t) => StashedValue::Table(t.stash(mc, roots)),
            Value::Function(f) => StashedValue::Function(f.stash(mc, roots)),
            Value::Thread(t) => StashedValue::Thread(t.stash(mc, roots)),
            Value::UserData(u) => StashedValue::UserData(u.stash(mc, roots)),
        }
    }
}

impl Fetchable for StashedValue {
    type Fetched<'gc> = Value<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedValue::Nil => Value::Nil,
            StashedValue::Boolean(b) => Value::Boolean(*b),
            StashedValue::Integer(i) => Value::Integer(*i),
            StashedValue::Number(n) => Value::Number(*n),
            StashedValue::String(s) => Value::String(s.fetch(roots)),
            StashedValue::Table(t) => Value::Table(t.fetch(roots)),
            StashedValue::Function(f) => Value::Function(f.fetch(roots)),
            StashedValue::Thread(t) => Value::Thread(t.fetch(roots)),
            StashedValue::UserData(u) => Value::UserData(u.fetch(roots)),
        }
    }
}

pub enum StashedError {
    Lua(StashedValue),
    Runtime(RuntimeError),
}

impl From<StashedValue> for StashedError {
    fn from(error: StashedValue) -> Self {
        Self::Lua(error)
    }
}

impl From<RuntimeError> for StashedError {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<E: Into<anyhow::Error>> From<E> for StashedError {
    fn from(error: E) -> Self {
        RuntimeError::from(error).into()
    }
}

impl<'gc> Stashable<'gc> for Error<'gc> {
    type Stashed = StashedError;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        match self {
            Error::Lua(err) => StashedError::Lua(err.0.stash(mc, roots)),
            Error::Runtime(err) => StashedError::Runtime(err),
        }
    }
}

impl Fetchable for StashedError {
    type Fetched<'gc> = Error<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedError::Lua(err) => Error::from_value(err.fetch(roots)),
            StashedError::Runtime(err) => err.clone().into(),
        }
    }
}
