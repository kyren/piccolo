use gc_arena::{Collect, MutationContext};

use super::{
    flatten::Flatten,
    map::{Map, MapWith},
    then::{Then, ThenWith},
    Sequence,
};

/// Extension trait for `Sequence` that provides useful combinator methods.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    /// Map a function over result of this sequence.
    ///
    /// The given function is run in the same call to `Sequence::step` that produces the result of
    /// this sequence.
    fn map<F, R>(self, f: F) -> Map<Self, F>
    where
        F: 'static + FnOnce(Self::Output) -> R,
    {
        Map::new(self, f)
    }

    /// Equivalent to `SequencExt::map` but calls the function with a context parameter.
    ///
    /// The context parameter can be anything that implements `Collect`.  It exists to allow
    /// closures to manually close over variables that implement `Collect`, because there is no way
    /// currently for closure types to automatically implement `Collect` and are thus required to be
    /// 'static.
    fn map_with<C, F, R>(self, c: C, f: F) -> MapWith<Self, C, F>
    where
        C: Collect,
        F: 'static + FnOnce(C, Self::Output) -> R,
    {
        MapWith::new(self, c, f)
    }

    /// Execute a separate sequence step after this sequence completes.
    ///
    /// The given function is run in a separate `Sequence::step` call from the one that produces the
    /// result of this sequence.
    fn then<F, R>(self, f: F) -> Then<'gc, Self, F>
    where
        Self::Output: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, Self::Output) -> R,
    {
        Then::new(self, f)
    }

    /// Equivalent to `SequenceExt::then` but calls the function with the given context parameter.
    ///
    /// The context parameter can be anything that implements `Collect`, it exists to allow closures
    /// to manually close over variables that implement `Collect`, because there is no way currently
    /// for closure types to automatically themselves implement `Collect` and are thus required to
    /// be 'static.
    fn then_with<C, F, R>(self, c: C, f: F) -> ThenWith<'gc, Self, C, F>
    where
        C: Collect,
        Self::Output: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, Self::Output) -> R,
    {
        ThenWith::new(self, c, f)
    }

    /// Call a function on the result of this sequence, producing a new sequence to run.
    ///
    /// The given function is run in a separate `Sequence::step` call to the one that produces a
    /// result of this sequence, and the `Sequence` that this function results in is run in
    /// additional separate `Sequence::step` calls.
    fn chain<F, R>(self, f: F) -> Flatten<'gc, Then<'gc, Self, F>>
    where
        Self::Output: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, Self::Output) -> R,
        R: Sequence<'gc>,
    {
        Flatten::new(Then::new(self, f))
    }

    /// Equivalent to `SequenceExt::chain` but calls the function with the given context parameter.
    ///
    /// The context parameter can be anything that implements `Collect`, it exists to allow closures
    /// to manually close over variables that implement `Collect`, because there is no way currently
    /// for closure types to automatically themselves implement `Collect` and are thus required to
    /// be 'static.
    fn chain_with<C, F, R>(self, c: C, f: F) -> Flatten<'gc, ThenWith<'gc, Self, C, F>>
    where
        C: Collect,
        Self::Output: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, Self::Output) -> R,
        R: Sequence<'gc>,
    {
        Flatten::new(ThenWith::new(self, c, f))
    }

    /// If this sequence results in another sequence, this combinator flattens them so that they are
    /// executed one after another.
    fn flatten(self) -> Flatten<'gc, Self>
    where
        Self::Output: Sequence<'gc>,
    {
        Flatten::new(self)
    }

    /// Turn this sequence into a boxed sequence type.
    ///
    /// The return type is a `dyn Sequence` because where you would need to produce a boxed sequence
    /// you generally are doing this to purposefully forget what the particular sequence type is,
    /// and doing this here eases type inference.
    fn boxed(self) -> Box<dyn Sequence<'gc, Output = Self::Output> + 'gc>
    where
        Self: 'gc,
    {
        Box::new(self)
    }
}

impl<'gc, T> SequenceExt<'gc> for T where T: Sequence<'gc> {}
