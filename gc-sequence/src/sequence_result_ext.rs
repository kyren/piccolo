use gc_arena::{Collect, MutationContext};

use crate::{
    and_then::{AndThen, AndThenWith},
    flatten_result::FlattenResult,
    map_result::{MapError, MapOk},
    Sequence,
};

/// Extension trait for `Sequences` producing a `Result` that provides useful combinator methods.
pub trait SequenceResultExt<'gc, I, E>: Sized + Sequence<'gc, Output = Result<I, E>> {
    /// Map a function over the successful result of this sequence, if it is successful.
    ///
    /// Similarly to `SequenceExt::map`, this function is run in the *same* call to `Sequence::step`
    /// where this sequence finishes.
    fn map_ok<F, R>(self, f: F) -> MapOk<Self, F>
    where
        F: 'static + FnOnce(I) -> R,
    {
        MapOk::new(self, f)
    }

    /// Map a function over the error result of this sequence, if it errors.
    ///
    /// Similarly to `SequenceExt::map`, this function is run in the *same* call to `Sequence::step`
    /// where this sequence finishes.
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
    fn and_then<F, R, I2>(self, f: F) -> AndThen<Self, F, R>
    where
        F: 'static + FnOnce(MutationContext<'gc, '_>, I) -> R,
        R: Sequence<'gc, Output = Result<I2, E>>,
    {
        AndThen::new(self, f)
    }

    /// Equivalent to `and_then`, but calls the function with the given context parameter.
    fn and_then_with<C, F, R, I2>(self, c: C, f: F) -> AndThenWith<Self, C, F, R>
    where
        C: Collect,
        F: 'static + FnOnce(MutationContext<'gc, '_>, C, I) -> R,
        R: Sequence<'gc, Output = Result<I2, E>>,
    {
        AndThenWith::new(self, c, f)
    }

    /// Similar to `SequenceExt::flatten`, but this sequence must result in an `Ok(next_sequence)`.
    fn flatten_result<I2>(self) -> FlattenResult<Self, I>
    where
        I: Sequence<'gc, Output = Result<I2, E>>,
    {
        FlattenResult::new(self)
    }
}

impl<'gc, T, I, E> SequenceResultExt<'gc, I, E> for T where T: Sequence<'gc, Output = Result<I, E>> {}
