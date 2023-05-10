use std::{
    borrow::Borrow,
    error::Error as StdError,
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    io::Write,
    ops::Deref,
    str,
};

use gc_arena::{unsize, Collect, Gc, MutationContext, RefLock};
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
    Inline(Gc<'gc, [u8]>),
    Buffer(Gc<'gc, Box<[u8]>>),
}

impl<'gc> Debug for String<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            String::Static(_) => fmt.write_str("Static")?,
            String::Inline(_) => fmt.write_str("Inline")?,
            String::Buffer(_) => fmt.write_str("Buffer")?,
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
    pub fn from_slice(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        macro_rules! alloc_lens {
            ($($i:expr),*) => {
                match s.len() {
                    $($i => String::Inline(
                        unsize!(Gc::new(mc, <[u8; $i]>::try_from(s).unwrap()) => [u8])
                    ),)*
                    _ => String::Buffer(Gc::new(mc, Box::from(s))),
                }
            };
        }

        alloc_lens!(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32
        )
    }

    pub fn from_static(s: &'static [u8]) -> String<'gc> {
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
                Value::UserData(_) => {
                    return Err(StringError::Concat {
                        bad_type: "userdata",
                    });
                }
            }
        }
        Ok(String::from_slice(mc, &bytes))
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            String::Static(b) => b,
            String::Inline(b) => b,
            String::Buffer(b) => b,
        }
    }

    pub fn len(&self) -> i64 {
        match self {
            String::Static(b) => b.len(),
            String::Inline(b) => b.len(),
            String::Buffer(b) => b.len(),
        }
        .try_into()
        .unwrap()
    }
}

impl<'gc> From<&'static str> for String<'gc> {
    fn from(s: &'static str) -> Self {
        Self::from_static(s.as_bytes())
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
pub struct InternedStringSet<'gc>(Gc<'gc, RefLock<FxHashSet<String<'gc>>>>);

impl<'gc> InternedStringSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> InternedStringSet<'gc> {
        InternedStringSet(Gc::new(mc, RefLock::new(FxHashSet::default())))
    }

    pub fn new_string(&self, mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        if let Some(found) = self.0.borrow().get(s) {
            return *found;
        }

        let s = String::from_slice(mc, s);
        self.0.borrow_mut(mc).insert(s);
        s
    }
}
