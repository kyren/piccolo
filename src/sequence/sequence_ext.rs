use gc_arena::{Collect, MutationContext};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::Sequence;

use super::and_then::{AndThen, AndThenWith};
use super::into_sequence::IntoSequence;
use super::map::{Map, MapWith};
use super::then::{Then, ThenWith};

/// Extension trait for `Sequence` that provides useful combinator methods, similarly to
/// `FutureExt`.  A key difference with `FutureExt` comes from `Sequence` requiring `Collect`: it is
/// currently not possible for arbitrary Rust closures to implement `Collect`, so there are
/// `xxx_with` variant methods that allow you to manually close over a type `C: Collect` to manually
/// capture `Collect` implementing types.  Tuples up to size 16 automatically implement `Collect`,
/// so this is a convenient way to manually capture a reasonable number of such values.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    fn map<F, R>(self, f: F) -> Map<Self, F>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> R,
    {
        Map::new(self, f)
    }

    fn map_with<C, F, R>(self, c: C, f: F) -> MapWith<Self, C, F>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Self::Item) -> R,
    {
        MapWith::new(self, c, f)
    }

    fn then<F, R>(self, f: F) -> Then<'gc, Self, F, R>
    where
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Result<Self::Item, Error>) -> R,
        R: IntoSequence<'gc>,
    {
        Then::new(self, f)
    }

    fn then_with<C, F, R>(self, c: C, f: F) -> ThenWith<'gc, Self, C, F, R>
    where
        C: Collect,
        F: 'static
            + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Result<Self::Item, Error>) -> R,
        R: IntoSequence<'gc>,
    {
        ThenWith::new(self, c, f)
    }

    fn and_then<F, R>(self, f: F) -> AndThen<'gc, Self, F, R>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Self::Item) -> R,
        R: IntoSequence<'gc>,
    {
        AndThen::new(self, f)
    }

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
