use std::{
    borrow::Borrow,
    error::Error as StdError,
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    io::Write,
    ops::Deref,
    str,
};

use gc_arena::{unsize, Collect, Gc, GcCell, MutationContext};
use rustc_hash::FxHashSet;

use crate::Value;

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

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum String<'gc> {
    Static(&'static [u8]),
    Short(Gc<'gc, [u8]>),
    Long(Gc<'gc, Box<[u8]>>),
}

impl<'gc> Debug for String<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            String::Static(_) => fmt.write_str("Static")?,
            String::Short(_) => fmt.write_str("Short")?,
            String::Long(_) => fmt.write_str("Long")?,
        }
        fmt.write_str("(")?;
        if let Ok(s) = str::from_utf8(self.as_bytes()) {
            Debug::fmt(s, fmt)?;
        } else {
            Debug::fmt(self.as_bytes(), fmt)?;
        }
        fmt.write_str(")")?;
        Ok(())
    }
}

impl<'gc> String<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        let len = s.len();
        if len <= 8 {
            let mut b = [0; 8];
            b[..len].copy_from_slice(s);
            String::Short(unsize!(Gc::allocate(mc, b) => [u8]))
        } else if len <= 32 {
            let mut b = [0; 32];
            b[..len].copy_from_slice(s);
            String::Short(unsize!(Gc::allocate(mc, b) => [u8]))
        } else {
            String::Long(Gc::allocate(mc, s.to_vec().into_boxed_slice()))
        }
    }

    pub fn new_static(s: &'static [u8]) -> String<'gc> {
        String::Static(s)
    }

    pub fn concat(
        mc: MutationContext<'gc, '_>,
        values: &[Value<'gc>],
    ) -> Result<String<'gc>, StringError> {
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
            }
        }
        Ok(String::new(mc, &bytes))
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            String::Static(b) => b,
            String::Short(b) => b,
            String::Long(b) => b,
        }
    }

    pub fn len(&self) -> i64 {
        fn as_i64(len: usize) -> i64 {
            if len <= std::i64::MAX as usize {
                len as i64
            } else {
                panic!("string is too long")
            }
        }

        as_i64(match self {
            String::Static(b) => b.len(),
            String::Short(b) => b.len(),
            String::Long(b) => b.len(),
        })
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
    T: AsRef<[u8]>,
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
pub struct InternedStringSet<'gc>(GcCell<'gc, FxHashSet<String<'gc>>>);

impl<'gc> InternedStringSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> InternedStringSet<'gc> {
        InternedStringSet(GcCell::allocate(mc, FxHashSet::default()))
    }

    pub fn new_string(&self, mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        if let Some(found) = self.0.read().get(s) {
            return *found;
        }

        let s = String::new(mc, s);
        self.0.write(mc).insert(s);
        s
    }
}
