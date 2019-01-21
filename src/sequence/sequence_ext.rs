use gc_arena::{Collect, MutationContext};

use crate::{IntoSequence, LuaContext, Sequence};

use super::and_then::{AndThen, AndThenWith};
use super::map::{Map, MapError};
use super::then::{Then, ThenWith};

/// Extension trait for `Sequence` that provides useful combinator methods, similarly to
/// `FutureExt`.  A key difference with `FutureExt` comes from `Sequence` requiring `Collect`: it is
/// currently not possible for arbitrary Rust closures to implement `Collect`, so there are
/// `xxx_with` variant methods that allow you to manually close over a type `C: Collect` to manually
/// capture `Collect` implementing types.  Tuples up to size 16 automatically implement `Collect`,
/// so this is a convenient way to manually capture a reasonable number of such values.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    /// Map a function over result of this sequence.  The given function is run as soon as the
    /// result is produced in the *same* call to `Sequence::step`, so there does not necessarily
    /// have to be a `Collect` bound on the `R` result type.
    fn map<F, R>(self, f: F) -> Map<Self, F>
    where
        F: 'static + FnOnce(Self::Item) -> R,
    {
        Map::new(self, f)
    }

    /// Similar to `map`, but maps the given function over the error of this sequence, if it results
    /// in an error.
    fn map_err<F, R>(self, f: F) -> MapError<Self, F>
    where
        F: 'static + FnOnce(Self::Error) -> R,
    {
        MapError::new(self, f)
    }

    /// Execute another sequence step after this sequence completes, regardless of the success or
    /// failure status.  The given callback is executed during a separate `Sequence::step` call
    /// after this sequence completes, so the return value must implement `Collect` so that it can
    /// be stored between gc mutations.
    fn then<F, R>(self, f: F) -> Then<'gc, Self, F, R>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Result<Self::Item, Self::Error>) -> R,
        R: IntoSequence<'gc>,
    {
        Then::new(self, f)
    }

    /// Equivalent to `then` but calls the function with the given context parameter.
    fn then_with<C, F, R>(self, c: C, f: F) -> ThenWith<'gc, Self, C, F, R>
    where
        C: Collect,
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                LuaContext<'gc>,
                C,
                Result<Self::Item, Self::Error>,
            ) -> R,
        R: IntoSequence<'gc>,
    {
        ThenWith::new(self, c, f)
    }

    /// Similar to `then`, but only calls the given function if this sequence completes
    /// *successfully*.  Also similarly to `then`, it calls the given function in a separate
    /// `Sequence::step` work unit.
    fn and_then<F, R>(self, f: F) -> AndThen<'gc, Self, F, R>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> R,
        R: IntoSequence<'gc>,
    {
        AndThen::new(self, f)
    }

    /// Equivalent to `and_then`, but calls the function with the given context parameter.
    fn and_then_with<C, F, R>(self, c: C, f: F) -> AndThenWith<'gc, Self, C, F, R>
    where
        C: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> R,
        R: IntoSequence<'gc>,
    {
        AndThenWith::new(self, c, f)
    }
}

impl<'gc, T> SequenceExt<'gc> for T where T: Sequence<'gc> {}
