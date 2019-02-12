use gc_arena::{Collect, MutationContext};

/// A trait that describes a sequence of actions to perform, in between which garbage collection may
/// take place.
///
/// This trait is similar to the `Future` trait in that it is not designed to be used directly, but
/// rather chained together using combinators and run to completion with a sequencer.
pub trait Sequence<'gc>: Collect {
    type Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output>;
}

impl<'gc, T: ?Sized + Sequence<'gc>> Sequence<'gc> for Box<T> {
    type Output = T::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        T::step(&mut (*self), mc)
    }
}
