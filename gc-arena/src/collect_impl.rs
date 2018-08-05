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

/// If a type is static, we know that it can never hold `Gc` pointers, so it is safe to provide a
/// simple empty `Collect` implementation.
/// `Collect` implementation.
#[macro_export]
macro_rules! static_collect {
    ($type:ty) => {
        unsafe impl Collect for $type
        where
            $type: 'static,
        {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    };
}

static_collect!(bool);
static_collect!(u8);
static_collect!(u16);
static_collect!(u32);
static_collect!(u64);
static_collect!(usize);
static_collect!(i8);
static_collect!(i16);
static_collect!(i32);
static_collect!(i64);
static_collect!(isize);
static_collect!(f32);
static_collect!(f64);
static_collect!(String);

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

unsafe impl<T: Collect> Collect for Option<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        if let Some(t) = self.as_ref() {
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

macro_rules! impl_tuple {
    () => (
        unsafe impl Collect for () {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    );

    ($($name:ident)+) => (
        unsafe impl<$($name,)*> Collect for ($($name,)*)
            where $($name: Collect,)*
        {
            #[inline]
            fn needs_trace() -> bool {
                $($name::needs_trace() ||)* false
            }

            #[allow(non_snake_case)]
            #[inline]
            fn trace(&self, cc: CollectionContext) {
                let ($($name,)*) = self;
                $($name.trace(cc);)*
            }
        }
    );
}

impl_tuple!{}
impl_tuple!{A}
impl_tuple!{A B}
impl_tuple!{A B C}
impl_tuple!{A B C D}
impl_tuple!{A B C D E}
impl_tuple!{A B C D E F}
impl_tuple!{A B C D E F G}
impl_tuple!{A B C D E F G H}
impl_tuple!{A B C D E F G H I}
impl_tuple!{A B C D E F G H I J}
impl_tuple!{A B C D E F G H I J K}
impl_tuple!{A B C D E F G H I J K L}
