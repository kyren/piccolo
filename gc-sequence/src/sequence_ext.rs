use gc_arena::{Collect, MutationContext};

use crate::{
    flatten::Flatten,
    map::Map,
    then::{Then, ThenWith},
    Sequence,
};

/// Extension trait for `Sequence` that provides useful combinator methods.
pub trait SequenceExt<'gc>: Sized + Sequence<'gc> {
    /// Map a function over result of this sequence.
    ///
    /// The given function is run as soon as the result is produced in the *same* call to
    /// `Sequence::step`, so there does not necessarily have to be a `Collect` bound on the `R`
    /// result type.
    fn map<F, R>(self, f: F) -> Map<Self, F>
    where
        F: 'static + FnOnce(Self::Output) -> R,
    {
        Map::new(self, f)
    }

    /// Execute another sequence step after this sequence completes.
    ///
    /// The given callback is executed during a separate `Sequence::step` call after this sequence
    /// completes, so the return value must implement `Collect` so that it can be stored between gc
    /// mutations.
    fn then<F, R>(self, f: F) -> Then<Self, F, R>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, Self::Output) -> R,
        R: Sequence<'gc>,
    {
        Then::new(self, f)
    }

    /// Equivalent to `then` but calls the function with the given context parameter.
    ///
    /// The context parameter can be anything that implements `Collect`, it exists to allow closures
    /// to manually close over variables that implement `Collect`, because there is no way currently
    /// for closure types to automatically themselves implement `Collect` and are thus required to
    /// be 'static.
    fn then_with<C, F, R>(self, c: C, f: F) -> ThenWith<Self, C, F, R>
    where
        C: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, Self::Output) -> R,
        R: Sequence<'gc>,
    {
        ThenWith::new(self, c, f)
    }

    /// If this sequence results in another sequence, this combinator flattens them so that they are
    /// executed one after another.  It is roughly equivalent to calling `s.then(|x| x)`.
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
