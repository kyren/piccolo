use gc_arena::{Collect, MutationContext};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::Sequence;

use super::and_then::{AndThen, AndThenWith};
use super::flatten::Flatten;
use super::then::{Then, ThenWith};

/// Extension trait for `Sequence` that provides useful combinator methods, similarly to
/// `FutureExt`.  A key difference with `FutureExt` comes from `Sequence` requiring `Collect`: it is
/// currently not possible for arbitrary Rust closures to implement `Collect`, so there are
/// `xxx_with` variant methods that allow you to manually close over a type `C: Collect` to manually
/// capture `Collect` implementing types.  Tuples up to size 16 automatically implement `Collect`,
/// so this is a convenient way to manually capture a reasonable number of such values.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    fn then<F, R>(self, f: F) -> Then<Self, F>
    where
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                Result<Self::Item, Error>,
            ) -> Result<R, Error>,
    {
        Then::new(self, f)
    }

    fn then_with<C, F, R>(self, c: C, f: F) -> ThenWith<Self, C, F>
    where
        C: Collect,
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                C,
                Result<Self::Item, Error>,
            ) -> Result<R, Error>,
    {
        ThenWith::new(self, c, f)
    }

    fn and_then<F, R>(self, f: F) -> AndThen<Self, F>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> Result<R, Error>,
    {
        AndThen::new(self, f)
    }

    fn and_then_with<C, F, R>(self, c: C, f: F) -> AndThenWith<Self, C, F>
    where
        C: Collect,
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> Result<R, Error>,
    {
        AndThenWith::new(self, c, f)
    }

    fn flatten(self) -> Flatten<'gc, Self> {
        Flatten::new(self)
    }

    fn sequence<F, N>(self, f: F) -> Flatten<'gc, Then<Self, F>>
    where
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                Result<Self::Item, Error>,
            ) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        self.then(f).flatten()
    }

    fn sequence_with<C, F, N>(self, c: C, f: F) -> Flatten<'gc, ThenWith<Self, C, F>>
    where
        C: Collect,
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                C,
                Result<Self::Item, Error>,
            ) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        self.then_with(c, f).flatten()
    }

    fn and_sequence<F, N>(self, f: F) -> Flatten<'gc, AndThen<Self, F>>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        self.and_then(f).flatten()
    }

    fn and_seqence_with<C, F, N>(self, c: C, f: F) -> Flatten<'gc, AndThenWith<Self, C, F>>
    where
        C: Collect,
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> Result<N, Error>,
        N: Sequence<'gc>,
    {
        self.and_then_with(c, f).flatten()
    }
}

impl<'gc, T> SequenceExt<'gc> for T where T: Sequence<'gc> {}
