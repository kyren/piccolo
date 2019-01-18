use failure::Error;

use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::lua::LuaContext;

/// A trait that describes a sequence of VM actions to perform with an eventual result.
///
/// This trait is similar to the `Future` trait in that it is not designed to be used directly, but
/// rather chained together using combinators and returned to an outer handler that will drive it to
/// completion.  There are three reasons for this design:
///
/// 1. The gc-arena design requires that the `mutate` method is exited before any garbage collection
///    can take place, but since VM code should not be able to delay garbage collection arbitrarily
///    long, we still need a way to execute some code, then garbage collect, then continue where we
///    left off.  This means that we must have a chained series of operations where in between each
///    link in the chain, we can exit the `mutate` method and perform garbage collection if
///    necessary.  This is also useful for sequencing separate allocating API calls and callbacks,
///    not just VM code, in such a way that garbage collection can occur with as fine of a
///    granularity as necessary.
///
/// 2. Full interoperability with Lua coroutines is difficult in a language without guaranteed TCO,
///    but there still needs to be a way to continue to another Lua or Rust function without
///    arbitrarily growing the Rust stack.  Even disregarding coroutines, this mechanism also allows
///    there not to be arbitrary limits on the Rust callback depth.
///
/// 3. It allows for an asynchronous API.  Using this, Rust code can be treated similarly to Lua VM
///    code in that it can be paused, resumed, and incrementally executed.  With a normal chain of
///    Rust -> Lua -> Rust -> Lua calls, it would not be possible to stop executing a script without
///    returning through all of the real Rust frames in the call stack, but it *is* possible to stop
///    arbitrarily at any point in a Sequence and resume later.
pub trait Sequence<'gc>: Collect {
    type Item;

    /// Perform a single unit of work, returning `Some` on completion, whether succsessful or not.
    /// Calling `pump` again after completion is an API violation and may panic.
    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Error>>;
}

pub fn sequence_fn<'gc, F, R>(f: F) -> SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> Result<R, Error>,
{
    SequenceFn::new(f)
}

pub fn sequence_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<C, F>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> Result<R, Error>,
{
    SequenceFnWith::new(c, f)
}

/// Extension trait for `Sequence` that provides useful combinator methods, similarly to
/// `FutureExt`.  A key difference with `FutureExt` comes from `Sequence` requiring `Collect`: it is
/// currently not possible for arbitrary Rust closures to implement `Collect`, so there are
/// `xxx_with` variant methods that allow you to manually close over a type `C: Collect` to manually
/// capture `Collect` implementing types.  Tuples up to size 16 automatically implement `Collect`,
/// so this is a convenient way to manually capture a reasonable number of such values.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    fn finally<F, R>(self, f: F) -> Finally<Self, F>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> Result<R, Error>,
    {
        Finally::new(self, f)
    }

    fn finally_with<C, F, R>(self, c: C, f: F) -> FinallyWith<Self, C, F>
    where
        C: Collect,
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> Result<R, Error>,
    {
        FinallyWith::new(self, c, f)
    }

    fn and_then<F, N>(self, f: F) -> AndThen<Self, F, N>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        AndThen::new(self, f)
    }

    fn and_then_with<C, F, N>(self, c: C, f: F) -> AndThenWith<Self, C, F, N>
    where
        C: Collect,
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        AndThenWith::new(self, c, f)
    }

    fn then<F, N>(self, f: F) -> Then<Self, F, N>
    where
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                Result<Self::Item, Error>,
            ) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        Then::new(self, f)
    }

    fn then_with<C, F, N>(self, c: C, f: F) -> ThenWith<Self, C, F, N>
    where
        C: Collect,
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                Result<Self::Item, Error>,
            ) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        ThenWith::new(self, c, f)
    }
}

impl<'gc, T> SequenceExt<'gc> for T where T: Sequence<'gc> {}

impl<'gc, T: ?Sized + Sequence<'gc>> Sequence<'gc> for Box<T> {
    type Item = T::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Error>> {
        T::pump(&mut (*self), mc, lc)
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceFn<F>(Option<StaticCollect<F>>);

impl<F> SequenceFn<F> {
    fn new(f: F) -> SequenceFn<F> {
        SequenceFn(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        Some(self.0.take().expect("cannot pump a finished sequence").0(
            mc, lc,
        ))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceFnWith<C, F>(Option<(C, StaticCollect<F>)>);

impl<C, F> SequenceFnWith<C, F> {
    fn new(c: C, f: F) -> SequenceFnWith<C, F> {
        SequenceFnWith(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<C, F>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        let (c, f) = self.0.take().expect("cannot pump a finished sequence");
        Some(f.0(mc, lc, c))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Finally<S, F>(Option<(S, StaticCollect<F>)>);

impl<S, F> Finally<S, F> {
    fn new(s: S, f: F) -> Finally<S, F> {
        Finally(Some((s, StaticCollect(f))))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Finally<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(Ok(r)) => Some(f(mc, lc, r)),
                Some(Err(e)) => Some(Err(e)),
                None => {
                    self.0 = Some((a, StaticCollect(f)));
                    None
                }
            },
            None => panic!("cannot pump a finished sequence"),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct FinallyWith<S, C, F>(Option<(S, C, StaticCollect<F>)>);

impl<S, C, F> FinallyWith<S, C, F> {
    fn new(s: S, c: C, f: F) -> FinallyWith<S, C, F> {
        FinallyWith(Some((s, c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for FinallyWith<S, C, F>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> R,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, c, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(Ok(r)) => Some(Ok(f(mc, lc, c, r))),
                Some(Err(e)) => Some(Err(e)),
                None => None,
            },
            None => panic!("cannot pump a finished sequence"),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct AndThen<S, F, N>(Chain<S, StaticCollect<F>, N>);

impl<S, F, N> AndThen<S, F, N> {
    fn new(s: S, f: F) -> AndThen<S, F, N> {
        AndThen(Chain::new(s, StaticCollect(f)))
    }
}

impl<'gc, S, F, N> Sequence<'gc> for AndThen<S, F, N>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> Result<N, Error>,
    N: Sequence<'gc>,
{
    type Item = N::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<N::Item, Error>> {
        self.0.pump(mc, lc, |mc, lc, f, r| f.0(mc, lc, r?))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct AndThenWith<S, C, F, N>(Chain<S, (C, StaticCollect<F>), N>);

impl<S, C, F, N> AndThenWith<S, C, F, N> {
    fn new(s: S, c: C, f: F) -> AndThenWith<S, C, F, N> {
        AndThenWith(Chain::new(s, (c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, N> Sequence<'gc> for AndThenWith<S, C, F, N>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> Result<N, Error>,
    N: Sequence<'gc>,
{
    type Item = N::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<N::Item, Error>> {
        self.0.pump(mc, lc, |mc, lc, (c, f), r| f.0(mc, lc, c, r?))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Then<S, F, N>(Chain<S, StaticCollect<F>, N>);

impl<S, F, N> Then<S, F, N> {
    fn new(s: S, f: F) -> Then<S, F, N> {
        Then(Chain::new(s, StaticCollect(f)))
    }
}

impl<'gc, S, F, N> Sequence<'gc> for Then<S, F, N>
where
    S: Sequence<'gc>,
    F: 'static
        + FnOnce(
            MutationContext<'gc, '_>,
            LuaContext<'gc>,
            Result<S::Item, Error>,
        ) -> Result<N, Error>,
    N: Sequence<'gc>,
{
    type Item = N::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<N::Item, Error>> {
        self.0.pump(mc, lc, |mc, lc, f, r| f.0(mc, lc, r))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct ThenWith<S, C, F, N>(Chain<S, (C, StaticCollect<F>), N>);

impl<S, C, F, N> ThenWith<S, C, F, N> {
    fn new(s: S, c: C, f: F) -> ThenWith<S, C, F, N> {
        ThenWith(Chain::new(s, (c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, N> Sequence<'gc> for ThenWith<S, C, F, N>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static
        + FnOnce(
            MutationContext<'gc, '_>,
            LuaContext<'gc>,
            C,
            Result<S::Item, Error>,
        ) -> Result<N, Error>,
    N: Sequence<'gc>,
{
    type Item = N::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<N::Item, Error>> {
        self.0.pump(mc, lc, |mc, lc, (c, f), r| f.0(mc, lc, c, r))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum Chain<S, C, N> {
    First(S, Option<C>),
    Second(N),
    Done,
}

impl<S, C, N> Chain<S, C, N> {
    fn new(s: S, c: C) -> Chain<S, C, N> {
        Chain::First(s, Some(c))
    }
}

impl<'gc, S, C, N> Chain<S, C, N>
where
    S: Sequence<'gc>,
    C: Collect,
    N: Sequence<'gc>,
{
    fn pump<F>(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
        f: F,
    ) -> Option<Result<N::Item, Error>>
    where
        F: FnOnce(
            MutationContext<'gc, '_>,
            LuaContext<'gc>,
            C,
            Result<S::Item, Error>,
        ) -> Result<N, Error>,
    {
        match self {
            Chain::First(s, c) => match s.pump(mc, lc) {
                Some(r) => match f(mc, lc, c.take().unwrap(), r) {
                    Ok(n) => {
                        *self = Chain::Second(n);
                        None
                    }
                    Err(e) => Some(Err(e)),
                },
                None => None,
            },
            Chain::Second(n) => match n.pump(mc, lc) {
                Some(r) => {
                    *self = Chain::Done;
                    Some(r)
                }
                None => None,
            },
            Chain::Done => panic!("cannot pump a finished sequence"),
        }
    }
}
