use gc_arena::{Collect, Gc, MutationContext};

// Represents `String` as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct String<'gc>(Gc<'gc, Header>);

#[derive(Collect)]
#[collect(no_drop)]
#[repr(C)]
struct HeaderString<T> {
    header: Header,
    value: T,
}

#[derive(Collect)]
#[collect(require_static)]
struct Header {
    slice: *const [u8],
}

impl<'gc> String<'gc> {
    pub fn from_buffer(mc: MutationContext<'gc, '_>, s: Box<[u8]>) -> String<'gc> {
        // SAFETY: We convert the box into a raw pointer to avoid violating stacked borrows rules,
        // and to make it explicit that we require that the pointer be stable.
        #[derive(Collect)]
        #[collect(require_static)]
        struct Buffer(*mut [u8]);

        impl Drop for Buffer {
            fn drop(&mut self) {
                unsafe {
                    drop(Box::from_raw(self.0));
                }
            }
        }

        type BufferString = HeaderString<Buffer>;
        let buffer = Buffer(Box::into_raw(s));

        String(unsafe {
            Gc::cast::<Header>(Gc::new(
                mc,
                BufferString {
                    header: Header { slice: buffer.0 },
                    value: buffer,
                },
            ))
        })
    }

    pub fn from_slice(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        type InlineString<const N: usize> = HeaderString<[u8; N]>;

        fn create<'gc, const N: usize>(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
            assert!(s.len() <= N);
            let mut string = InlineString {
                header: Header { slice: &[] },
                value: [0; N],
            };
            string.value[0..s.len()].copy_from_slice(s);

            let string = Gc::new(mc, string);
            // SAFETY: We know we have unique access to the `InlineString` since we just allocated
            // it. We know that the `slice` ptr in the header will not be invalidated because it
            // is moved into the `Gc` allocation *before* the slice pointer is calculated, and `Gc`
            // allocations are stable.
            unsafe {
                (*(Gc::as_ptr(string) as *mut InlineString<N>)).header.slice =
                    &string.value[0..s.len()];
                String(Gc::cast::<Header>(string))
            }
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
        String(unsafe {
            Gc::cast::<Header>(Gc::new(
                mc,
                HeaderString {
                    header: Header { slice: s },
                    value: (),
                },
            ))
        })
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { &*(self.0.slice) }
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
