use std::collections::HashMap;
use std::hash::{BuildHasher, Hash};

use collect::Collect;
use context::CollectionContext;

/// If a type will never hold `Gc` pointers, you can use this macro to provide a simple empty
/// `Collect` implementation.
#[macro_export]
macro_rules! unsafe_empty_collect {
    ($type:ty) => {
        unsafe impl Collect for $type {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    };
}

unsafe_empty_collect!(());
unsafe_empty_collect!(bool);
unsafe_empty_collect!(u8);
unsafe_empty_collect!(u16);
unsafe_empty_collect!(u32);
unsafe_empty_collect!(u64);
unsafe_empty_collect!(i8);
unsafe_empty_collect!(i16);
unsafe_empty_collect!(i32);
unsafe_empty_collect!(i64);
unsafe_empty_collect!(f32);
unsafe_empty_collect!(f64);
unsafe_empty_collect!(String);

unsafe impl<T: ?Sized> Collect for &'static T {
    #[inline]
    fn needs_trace() -> bool {
        false
    }

    #[inline]
    fn trace(&self, _cc: CollectionContext) {}
}

unsafe impl<T: Collect> Collect for Box<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        (**self).trace(cc)
    }
}

unsafe impl<T: Collect> Collect for Box<[T]> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self.iter() {
            t.trace(cc)
        }
    }
}

unsafe impl<T: Collect> Collect for Vec<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self {
            t.trace(cc)
        }
    }
}

unsafe impl<K, V, S> Collect for HashMap<K, V, S>
where
    K: Eq + Hash + Collect,
    V: Collect,
    S: BuildHasher,
{
    #[inline]
    fn needs_trace() -> bool {
        K::needs_trace() || V::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for (k, v) in self {
            k.trace(cc);
            v.trace(cc);
        }
    }
}

macro_rules! impl_array_collect {
    ($sz:expr) => {
        unsafe impl<T: Collect> Collect for [T; $sz] {
            #[inline]
            fn needs_trace() -> bool {
                T::needs_trace()
            }

            #[inline]
            fn trace(&self, cc: CollectionContext) {
                for t in self {
                    t.trace(cc)
                }
            }
        }
    };
}

impl_array_collect!(1);
impl_array_collect!(2);
impl_array_collect!(3);
impl_array_collect!(4);
impl_array_collect!(5);
impl_array_collect!(6);
impl_array_collect!(7);
impl_array_collect!(8);
impl_array_collect!(9);
impl_array_collect!(10);
impl_array_collect!(11);
impl_array_collect!(12);
impl_array_collect!(13);
impl_array_collect!(14);
impl_array_collect!(15);
impl_array_collect!(16);
impl_array_collect!(17);
impl_array_collect!(18);
impl_array_collect!(19);
impl_array_collect!(20);
impl_array_collect!(21);
impl_array_collect!(22);
impl_array_collect!(23);
impl_array_collect!(24);
impl_array_collect!(25);
impl_array_collect!(26);
impl_array_collect!(27);
impl_array_collect!(28);
impl_array_collect!(29);
impl_array_collect!(30);
impl_array_collect!(31);
impl_array_collect!(32);
