use std::marker::PhantomData;

use super::Sequence;

/// A trait to construct `Sequence` types.  This trait is used via the `gen_sequence` macro to make
/// consuming a `Sequence` less painful.
///
/// Generally types taking a `Sequence` need to take something like:
///
/// `S: for<'gc> Sequence<'gc> + 'gc`
///
/// which is not valid Rust!  There is no way to say that a type must implement `Sequence` for all
/// lifetimes `'gc`, and also *must be valid for whatever that lifetime `'gc` is*.  When `Sequence`s
/// are produced in luster, generally they will be stored in a `Box<Sequence<'gc> + 'gc>` and pumped
/// to completion.  `Sequence`s implement `Collect` and are assumed to at some point capture `'gc`
/// braneded types from a `Sequence::pump` call, so it is important that they be convertible to
/// `Box<Sequence<'gc> + 'gc>` rather than require `'static`.
///
/// You can write this requirement in other ways, like using HRTBs in a `Fn` trait (this also can
/// require using `PhantomData`):
///
/// `F: for<'gc> Fn(PhantomData<&'gc ()>) -> Sequence<'gc> + 'gc`
///
/// but this is somewhat unweildy.  This trait is much easier to use than such a bound, and the
/// accompanying `gen_sequence` macro will produce a type implementing this for any expression that
/// results in a `Sequence`.
pub trait GenSequence {
    type Item;
    type Error;

    fn gen_sequence<'gc>(self) -> Box<Sequence<'gc, Item = Self::Item, Error = Self::Error> + 'gc>;
}

#[derive(Copy, Clone)]
pub struct GenSequenceFn<F, I, E>(pub F)
where
    F: for<'gc> FnOnce(PhantomData<&'gc ()>) -> Box<Sequence<'gc, Item = I, Error = E> + 'gc>;

impl<F, I, E> GenSequence for GenSequenceFn<F, I, E>
where
    F: for<'gc> FnOnce(PhantomData<&'gc ()>) -> Box<Sequence<'gc, Item = I, Error = E> + 'gc>,
{
    type Item = I;
    type Error = E;

    fn gen_sequence<'gc>(self) -> Box<Sequence<'gc, Item = Self::Item, Error = Self::Error> + 'gc> {
        self.0(PhantomData)
    }
}

#[macro_export]
macro_rules! gen_sequence {
    ($f:expr) => {
        $crate::sequence::GenSequenceFn(|_| Box::new($f))
    };
}
