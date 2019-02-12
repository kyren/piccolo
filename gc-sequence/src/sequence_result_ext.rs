use gc_arena::{Collect, MutationContext};

use crate::{
    and_then::{AndThen, AndThenWith},
    flatten_result::FlattenOk,
    map_result::{MapError, MapOk, MapOkWith},
    Sequence,
};

/// Extension trait for `Sequences` producing a `Result` that provides useful combinator methods.
pub trait SequenceResultExt<'gc, I, E>: Sized + Sequence<'gc, Output = Result<I, E>> {
    /// Map a function over the result of this sequence if it is successful.
    ///
    /// Similarly to `SequenceExt::map`, this function is run in the same call to `Sequence::step`.
    fn map_ok<F, R>(self, f: F) -> MapOk<Self, F>
    where
        F: 'static + FnOnce(I) -> R,
    {
        MapOk::new(self, f)
    }

    /// Equivalent to `SequenceResultExt::map_ok`, but takes a context parameter.
    fn map_ok_with<C, F, R>(self, c: C, f: F) -> MapOkWith<Self, C, F>
    where
        F: 'static + FnOnce(C, I) -> R,
    {
        MapOkWith::new(self, c, f)
    }

    /// Map a function over the error result of this sequence, if it errors.
    ///
    /// Similarly to `SequenceExt::map`, this function is run in the same call to `Sequence::step`.
    fn map_err<F, R>(self, f: F) -> MapError<Self, F>
    where
        F: 'static + FnOnce(E) -> R,
    {
        MapError::new(self, f)
    }

    /// Execute another sequence step after this sequence completes successfully.
    ///
    /// Similar to `SequenceExt::then` but only calls the given function when this sequence
    /// completes successfully.
    fn and_then<F, R>(self, f: F) -> AndThen<Self, F, I>
    where
        I: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, I) -> Result<R, E>,
    {
        AndThen::new(self, f)
    }

    /// Equivalent to `and_then`, but calls the function with the given context parameter.
    fn and_then_with<C, F, R>(self, c: C, f: F) -> AndThenWith<Self, C, F, I>
    where
        C: Collect,
        I: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, I) -> Result<R, E>,
    {
        AndThenWith::new(self, c, f)
    }

    /// Call a function on the result of this sequence, producing a new sequence to run.
    ///
    /// Similar to `SequenceExt::chain` but only calls the given function when this sequence
    /// completes successfully.
    fn and_chain<F, R, I2>(self, f: F) -> FlattenOk<AndThen<Self, F, I>, R>
    where
        I: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, I) -> Result<R, E>,
        R: Sequence<'gc, Output = Result<I2, E>>,
    {
        FlattenOk::new(AndThen::new(self, f))
    }

    /// Equivalent to `and_then`, but calls the function with the given context parameter.
    fn and_chain_with<C, F, R, I2>(self, c: C, f: F) -> FlattenOk<AndThenWith<Self, C, F, I>, R>
    where
        C: Collect,
        I: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, I) -> Result<R, E>,
        R: Sequence<'gc, Output = Result<I2, E>>,
    {
        FlattenOk::new(AndThenWith::new(self, c, f))
    }

    /// Similar to `SequenceExt::flatten`, but this sequence must result in an `Ok(next_sequence)`.
    fn flatten_ok<I2>(self) -> FlattenOk<Self, I>
    where
        I: Sequence<'gc, Output = Result<I2, E>>,
    {
        FlattenOk::new(self)
    }
}

impl<'gc, T, I, E> SequenceResultExt<'gc, I, E> for T where T: Sequence<'gc, Output = Result<I, E>> {}
