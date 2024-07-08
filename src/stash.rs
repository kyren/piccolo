use std::{cell::Cell, fmt, marker::PhantomData};

use gc_arena::{
    Collect, DynamicRoot, DynamicRootSet, Gc, MismatchedRootSet, Mutation, Root, Rootable,
};

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

type Invariant<'a> = PhantomData<Cell<&'a ()>>;

/// A handle to a `Gc` pointer held inside a [`StashedRootSet`] which is not branded by the `'gc`
/// lifetime and can be held outside of the normal GC context.
///
/// Unlike a normal [`DynamicRoot`], `StashedRoot` is branded with an invariant `'ctx` lifetime.
/// This is *separate* from the usual `'gc` branding, and is used in certain `piccolo` contexts
/// for correctness.
pub struct StashedRoot<'ctx, R: for<'gc> Rootable<'gc>> {
    inner: DynamicRoot<R>,
    _invariant: Invariant<'ctx>,
}

impl<'ctx, R: for<'gc> Rootable<'gc>> Clone for StashedRoot<'ctx, R> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _invariant: PhantomData,
        }
    }
}

impl<'ctx, R: for<'gc> Rootable<'gc>> StashedRoot<'ctx, R> {
    pub fn as_ptr<'gc>(&self) -> *const Root<'gc, R> {
        self.inner.as_ptr()
    }
}

/// A wrapper around a [`DynamicRootSet`] that brands its returned roots with an invariant `'ctx`
/// lifetime.
///
/// Returned [`StashedRoot`] handles are branded with an invariant `'ctx` lifetime. This is
/// *separate* from the usual `'gc` branding, and is used in certain `piccolo` contexts for
/// correctness.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct StashedRootSet<'ctx, 'gc> {
    roots: DynamicRootSet<'gc>,
    _invariant: Invariant<'ctx>,
}

impl<'ctx, 'gc> StashedRootSet<'ctx, 'gc> {
    /// Create a new `StashedRootSet` by wrapping a `DynamicRootSet`.
    ///
    /// This creates a `StashedRootSet` with an *unbound* `'ctx` lifetime, it is up to the user to
    /// limit this `'ctx` lifetime into something useful.
    pub fn new(roots: DynamicRootSet<'gc>) -> Self {
        Self {
            roots,
            _invariant: PhantomData,
        }
    }

    /// Store a root inside this root set.
    pub fn stash_root<R: for<'a> Rootable<'a>>(
        &self,
        mc: &Mutation<'gc>,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> StashedRoot<'ctx, R> {
        StashedRoot {
            inner: self.roots.stash(mc, root),
            _invariant: PhantomData,
        }
    }

    /// Fetch a root from this root set.
    pub fn fetch_root<R: for<'r> Rootable<'r>>(
        &self,
        root: &StashedRoot<'ctx, R>,
    ) -> Gc<'gc, Root<'gc, R>> {
        self.roots.fetch(&root.inner)
    }

    /// Fetch a root, returning an error if this root does not belong to this root set.
    pub fn try_fetch_root<R: for<'r> Rootable<'r>>(
        &self,
        root: &StashedRoot<'ctx, R>,
    ) -> Result<Gc<'gc, Root<'gc, R>>, MismatchedRootSet> {
        self.roots.try_fetch(&root.inner)
    }

    /// Returns true if the given root belings to this root set.
    pub fn contains_root<R: for<'r> Rootable<'r>>(&self, root: &StashedRoot<'ctx, R>) -> bool {
        self.roots.contains(&root.inner)
    }

    /// "Stash" a value with a `'gc` branding lifetime in this `StashRootSet`, creating a `'ctx`
    /// handle to it.
    ///
    /// This works for any type that implements the [`Stashable`] trait, which all common `piccolo`
    /// types do.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> S::Stashed<'ctx> {
        s.stash(mc, StashedRootSet::new(self.roots))
    }

    /// "Fetch" the real value for a handle that has been returned from [`StashedRootSet::stash`].
    ///
    /// It can be implemented for external types by implementing the [`Fetchable`] trait.
    pub fn fetch<F: Fetchable<'ctx>>(&self, f: &F) -> F::Fetched<'gc> {
        f.fetch(StashedRootSet::new(self.roots))
    }
}

/// A trait for types that can be stashed into a [`StashedRootSet`].
///
/// This trait is simpler to work with than having to manually specify `Rootable` projections and
/// can work with more types than just those that wrap a single `Gc` pointer.
///
/// It is implemented for all common `piccolo` types, allowing you to stash those types in the
/// registry or inside of async sequences.
pub trait Stashable<'gc> {
    type Stashed<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx>;
}

/// A trait for types that can be fetched from a [`StashedRootSet`].
pub trait Fetchable<'ctx> {
    type Fetched<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc>;
}

#[derive(Clone)]
pub struct StashedString<'ctx>(StashedRoot<'ctx, Rootable![StringInner]>);

impl<'ctx> fmt::Debug for StashedString<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedString")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for String<'gc> {
    type Stashed<'ctx> = StashedString<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedString(roots.stash_root::<Rootable![StringInner]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedString<'ctx> {
    type Fetched<'gc> = String<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        String::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedTable<'ctx>(StashedRoot<'ctx, Rootable![TableInner<'_>]>);

impl<'ctx> fmt::Debug for StashedTable<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedTable")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Table<'gc> {
    type Stashed<'ctx> = StashedTable<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedTable(roots.stash_root::<Rootable![TableInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedTable<'ctx> {
    type Fetched<'gc> = Table<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        Table::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedClosure<'ctx>(StashedRoot<'ctx, Rootable![ClosureInner<'_>]>);

impl<'ctx> fmt::Debug for StashedClosure<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Closure<'gc> {
    type Stashed<'ctx> = StashedClosure<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedClosure(roots.stash_root::<Rootable![ClosureInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedClosure<'ctx> {
    type Fetched<'gc> = Closure<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        Closure::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedCallback<'ctx>(StashedRoot<'ctx, Rootable![CallbackInner<'_>]>);

impl<'ctx> fmt::Debug for StashedCallback<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedCallback")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Callback<'gc> {
    type Stashed<'ctx> = StashedCallback<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedCallback(roots.stash_root::<Rootable![CallbackInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedCallback<'ctx> {
    type Fetched<'gc> = Callback<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        Callback::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedThread<'ctx>(StashedRoot<'ctx, Rootable![ThreadInner<'_>]>);

impl<'ctx> fmt::Debug for StashedThread<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedThread")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Thread<'gc> {
    type Stashed<'ctx> = StashedThread<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedThread(roots.stash_root::<Rootable![ThreadInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedThread<'ctx> {
    type Fetched<'gc> = Thread<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        Thread::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedUserData<'ctx>(StashedRoot<'ctx, Rootable![UserDataInner<'_>]>);

impl<'ctx> fmt::Debug for StashedUserData<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for UserData<'gc> {
    type Stashed<'ctx> = StashedUserData<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedUserData(roots.stash_root::<Rootable![UserDataInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedUserData<'ctx> {
    type Fetched<'gc> = UserData<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        UserData::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedExecutor<'ctx>(StashedRoot<'ctx, Rootable![ExecutorInner<'_>]>);

impl<'ctx> fmt::Debug for StashedExecutor<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedExecutor")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Executor<'gc> {
    type Stashed<'ctx> = StashedExecutor<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        StashedExecutor(roots.stash_root::<Rootable![ExecutorInner<'_>]>(mc, self.into_inner()))
    }
}

impl<'ctx> Fetchable<'ctx> for StashedExecutor<'ctx> {
    type Fetched<'gc> = Executor<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        Executor::from_inner(roots.fetch_root(&self.0))
    }
}

#[derive(Debug, Clone)]
pub enum StashedFunction<'ctx> {
    Closure(StashedClosure<'ctx>),
    Callback(StashedCallback<'ctx>),
}

impl<'ctx> From<StashedClosure<'ctx>> for StashedFunction<'ctx> {
    fn from(closure: StashedClosure<'ctx>) -> Self {
        Self::Closure(closure)
    }
}

impl<'ctx> From<StashedCallback<'ctx>> for StashedFunction<'ctx> {
    fn from(callback: StashedCallback<'ctx>) -> Self {
        Self::Callback(callback)
    }
}

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed<'ctx> = StashedFunction<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        match self {
            Function::Closure(closure) => StashedFunction::Closure(closure.stash(mc, roots)),
            Function::Callback(callback) => StashedFunction::Callback(callback.stash(mc, roots)),
        }
    }
}

impl<'ctx> Fetchable<'ctx> for StashedFunction<'ctx> {
    type Fetched<'gc> = Function<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedFunction::Closure(closure) => Function::Closure(closure.fetch(roots)),
            StashedFunction::Callback(callback) => Function::Callback(callback.fetch(roots)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StashedValue<'ctx> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(StashedString<'ctx>),
    Table(StashedTable<'ctx>),
    Function(StashedFunction<'ctx>),
    Thread(StashedThread<'ctx>),
    UserData(StashedUserData<'ctx>),
}

impl<'ctx> StashedValue<'ctx> {
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

impl<'ctx> From<bool> for StashedValue<'ctx> {
    fn from(v: bool) -> StashedValue<'ctx> {
        StashedValue::Boolean(v)
    }
}

impl<'ctx> From<i64> for StashedValue<'ctx> {
    fn from(v: i64) -> StashedValue<'ctx> {
        StashedValue::Integer(v)
    }
}

impl<'ctx> From<f64> for StashedValue<'ctx> {
    fn from(v: f64) -> StashedValue<'ctx> {
        StashedValue::Number(v)
    }
}

impl<'ctx> From<StashedString<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedString<'ctx>) -> StashedValue<'ctx> {
        StashedValue::String(v)
    }
}

impl<'ctx> From<StashedTable<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedTable<'ctx>) -> StashedValue<'ctx> {
        StashedValue::Table(v)
    }
}

impl<'ctx> From<StashedFunction<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedFunction<'ctx>) -> StashedValue<'ctx> {
        StashedValue::Function(v)
    }
}

impl<'ctx> From<StashedClosure<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedClosure<'ctx>) -> StashedValue<'ctx> {
        StashedValue::Function(StashedFunction::Closure(v))
    }
}

impl<'ctx> From<StashedCallback<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedCallback<'ctx>) -> StashedValue<'ctx> {
        StashedValue::Function(StashedFunction::Callback(v))
    }
}

impl<'ctx> From<StashedUserData<'ctx>> for StashedValue<'ctx> {
    fn from(v: StashedUserData<'ctx>) -> StashedValue<'ctx> {
        StashedValue::UserData(v)
    }
}

impl<'gc> Stashable<'gc> for Value<'gc> {
    type Stashed<'ctx> = StashedValue<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
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

impl<'ctx> Fetchable<'ctx> for StashedValue<'ctx> {
    type Fetched<'gc> = Value<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
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

pub enum StashedError<'ctx> {
    Lua(StashedValue<'ctx>),
    Runtime(RuntimeError),
}

impl<'ctx> From<StashedValue<'ctx>> for StashedError<'ctx> {
    fn from(error: StashedValue<'ctx>) -> Self {
        Self::Lua(error)
    }
}

impl<'ctx> From<RuntimeError> for StashedError<'ctx> {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'ctx, E: Into<anyhow::Error>> From<E> for StashedError<'ctx> {
    fn from(error: E) -> Self {
        RuntimeError::from(error).into()
    }
}

impl<'gc> Stashable<'gc> for Error<'gc> {
    type Stashed<'ctx> = StashedError<'ctx>;

    fn stash<'ctx>(
        self,
        mc: &Mutation<'gc>,
        roots: StashedRootSet<'ctx, 'gc>,
    ) -> Self::Stashed<'ctx> {
        match self {
            Error::Lua(err) => StashedError::Lua(err.0.stash(mc, roots)),
            Error::Runtime(err) => StashedError::Runtime(err),
        }
    }
}

impl<'ctx> Fetchable<'ctx> for StashedError<'ctx> {
    type Fetched<'gc> = Error<'gc>;

    fn fetch<'gc>(&self, roots: StashedRootSet<'ctx, 'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedError::Lua(err) => Error::from_value(err.fetch(roots)),
            StashedError::Runtime(err) => err.clone().into(),
        }
    }
}
