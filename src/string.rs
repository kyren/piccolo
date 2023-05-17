use std::{
    alloc,
    borrow::Borrow,
    error::Error as StdError,
    fmt,
    hash::{Hash, Hasher},
    io::Write,
    ops::Deref,
    slice, str,
    string::String as StdString,
};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};
use rustc_hash::FxHashSet;

use crate::Value;

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
    pub fn from_buffer(mc: &Mutation<'gc>, s: Box<[u8]>) -> String<'gc> {
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

    pub fn from_slice<S: ?Sized + AsRef<[u8]>>(mc: &Mutation<'gc>, s: &S) -> String<'gc> {
        fn create<'gc, const N: usize>(mc: &Mutation<'gc>, s: &[u8]) -> String<'gc> {
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

        let s = s.as_ref();

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

    pub fn from_static<S: ?Sized + AsRef<[u8]>>(mc: &Mutation<'gc>, s: &'static S) -> String<'gc> {
        String(Gc::new(mc, Header::Indirect(s.as_ref())))
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

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum StringError {
    Concat { bad_type: &'static str },
}

impl StdError for StringError {}

impl fmt::Display for StringError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StringError::Concat { bad_type } => write!(fmt, "cannot concat {}", bad_type),
        }
    }
}

impl<'gc> fmt::Debug for String<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("String(")?;
        if let Ok(s) = str::from_utf8(self.as_bytes()) {
            fmt::Debug::fmt(s, fmt)?;
        } else {
            fmt::Debug::fmt(self.as_bytes(), fmt)?;
        }
        fmt.write_str(")")?;
        Ok(())
    }
}

impl<'gc> fmt::Display for String<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(StdString::from_utf8_lossy(self.as_bytes()).as_ref())
    }
}

impl<'gc> String<'gc> {
    pub fn concat(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<String<'gc>, StringError> {
        let mut bytes = Vec::new();
        for value in values {
            match value {
                Value::Nil => write!(&mut bytes, "nil").unwrap(),
                Value::Boolean(b) => write!(&mut bytes, "{}", b).unwrap(),
                Value::Integer(i) => write!(&mut bytes, "{}", i).unwrap(),
                Value::Number(n) => write!(&mut bytes, "{}", n).unwrap(),
                Value::String(s) => bytes.extend(s.as_bytes()),
                Value::Table(_) => return Err(StringError::Concat { bad_type: "table" }),
                Value::Function(_) => {
                    return Err(StringError::Concat {
                        bad_type: "function",
                    });
                }
                Value::Thread(_) => {
                    return Err(StringError::Concat { bad_type: "thread" });
                }
                Value::UserData(_) => {
                    return Err(StringError::Concat {
                        bad_type: "userdata",
                    });
                }
            }
        }
        Ok(String::from_slice(mc, &bytes))
    }

    pub fn len(&self) -> i64 {
        self.as_bytes().len().try_into().unwrap()
    }
}

impl<'gc> Deref for String<'gc> {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<'gc> AsRef<[u8]> for String<'gc> {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<'gc> Borrow<[u8]> for String<'gc> {
    fn borrow(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<'gc, T> PartialEq<T> for String<'gc>
where
    T: ?Sized + AsRef<[u8]>,
{
    fn eq(&self, other: &T) -> bool {
        self.as_bytes() == other.as_ref()
    }
}

impl<'gc> Eq for String<'gc> {}

impl<'gc> Hash for String<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_bytes().hash(state);
    }
}

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct InternedStringSet<'gc>(Gc<'gc, RefLock<FxHashSet<String<'gc>>>>);

impl<'gc> InternedStringSet<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> InternedStringSet<'gc> {
        InternedStringSet(Gc::new(mc, RefLock::new(FxHashSet::default())))
    }

    pub fn new_string(&self, mc: &Mutation<'gc>, s: &[u8]) -> String<'gc> {
        if let Some(found) = self.0.borrow().get(s) {
            return *found;
        }

        let s = String::from_slice(mc, s);
        self.0.borrow_mut(mc).insert(s);
        s
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
