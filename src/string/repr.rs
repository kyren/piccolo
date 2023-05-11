use std::{alloc, slice};

use gc_arena::{Collect, Gc, MutationContext};

// Represents `String` as either a pointer to an external / owned slice pointer or a size prefixed
// inline array.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct String<'gc>(Gc<'gc, Header>);

#[derive(Copy, Clone, Collect)]
#[collect(require_static)]
enum Header {
    Indirect(*const [u8]),
    Inline(usize),
}

impl<'gc> String<'gc> {
    pub fn from_buffer(mc: MutationContext<'gc, '_>, s: Box<[u8]>) -> String<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        #[repr(transparent)]
        struct Owned(Header);

        impl Drop for Owned {
            fn drop(&mut self) {
                match self.0 {
                    Header::Indirect(ptr) => unsafe {
                        drop(Box::from_raw(ptr as *mut [u8]));
                    },
                    Header::Inline(_) => unreachable!(),
                }
            }
        }

        let owned = Owned(Header::Indirect(Box::into_raw(s)));
        // SAFETY: We know we can cast to `InlineHeader` because `Owned` is `#[repr(transparent)]`
        String(unsafe { Gc::cast::<Header>(Gc::new(mc, owned)) })
    }

    pub fn from_slice(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        fn create<'gc, const N: usize>(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
            #[derive(Collect)]
            #[collect(require_static)]
            #[repr(C)]
            struct InlineString<const N: usize> {
                header: Header,
                array: [u8; N],
            }

            assert!(s.len() <= N);
            let mut string = InlineString {
                header: Header::Inline(s.len()),
                array: [0; N],
            };
            string.array[0..s.len()].copy_from_slice(s);

            let string = Gc::new(mc, string);
            // SAFETY: We know we can cast to `Header` because `InlineString` is `#[repr(C)]`
            // and `header` is the first field.
            unsafe { String(Gc::cast::<Header>(string)) }
        }

        macro_rules! try_sizes {
            ($($size:expr),*) => {
                $(if s.len() <= $size {
                    return create::<$size>(mc, s);
                })*
            };
        }

        try_sizes!(0, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024);

        Self::from_buffer(mc, Box::from(s))
    }

    pub fn from_static(mc: MutationContext<'gc, '_>, s: &'static [u8]) -> String<'gc> {
        String(Gc::new(mc, Header::Indirect(s)))
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            match *self.0 {
                Header::Indirect(p) => &(*p),
                Header::Inline(len) => {
                    // This is probably ridiculous overkill, since the offset I think should
                    // always be 1, but precisely calculate the offset of the inline array using
                    // `alloc::Layout`
                    let layout = alloc::Layout::new::<Header>();
                    let (_, offset) = layout
                        .extend(alloc::Layout::array::<u8>(len).unwrap())
                        .unwrap();
                    let data =
                        (Gc::as_ptr(self.0) as *const u8).offset(offset as isize) as *const u8;
                    slice::from_raw_parts(data, len)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::rootless_arena;

    use super::*;

    #[test]
    fn test_string_header() {
        rootless_arena(|mc| {
            let test1 = String::from_buffer(mc, Box::from(b"test 1".as_slice()));
            let test2 = String::from_buffer(mc, Box::from(b"test 2".as_slice()));

            let test3 = String::from_slice(mc, b"test 3");
            let test4 = String::from_slice(mc, b"test 4444 4444 4444 4444");

            let test5 = String::from_static(mc, b"test 55555 55555 55555 55555 55555");
            let test6 = String::from_static(mc, b"test 666666");

            assert_eq!(test1.as_bytes(), b"test 1");
            assert_eq!(test2.as_bytes(), b"test 2");
            assert_eq!(test3.as_bytes(), b"test 3");
            assert_eq!(test4.as_bytes(), b"test 4444 4444 4444 4444");
            assert_eq!(test5.as_bytes(), b"test 55555 55555 55555 55555 55555");
            assert_eq!(test6.as_bytes(), b"test 666666");
        });
    }
}
