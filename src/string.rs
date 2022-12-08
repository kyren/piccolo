use std::borrow::Borrow;
use std::error::Error as StdError;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::ops::Deref;
use std::str;

use rustc_hash::FxHashSet;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

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
    Short8(u8, Gc<'gc, [u8; 8]>),
    Short32(u8, Gc<'gc, [u8; 32]>),
    Long(Gc<'gc, Box<[u8]>>),
    Static(&'static [u8]),
}

impl<'gc> Debug for String<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            String::Short8(_, _) => fmt.write_str("Short8")?,
            String::Short32(_, _) => fmt.write_str("Short32")?,
            String::Long(_) => fmt.write_str("Long")?,
            String::Static(_) => fmt.write_str("Static")?,
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
        String::new_mapped(mc, s, |from, idx| from[idx])
    }

    pub fn new_mapped<F>(mc: MutationContext<'gc, '_>, s: &[u8], f: F) -> String<'gc>
    where
        F: Fn(&[u8], usize) -> u8,
    {
        let len = s.len();
        if len <= 8 {
            let mut b = [0; 8];
            for i in 0..len {
                b[i] = f(s, i);
            }
            String::Short8(len as u8, Gc::allocate(mc, b))
        } else if len <= 32 {
            let mut b = [0; 32];
            for i in 0..len {
                b[i] = f(s, i);
            }
            String::Short32(len as u8, Gc::allocate(mc, b))
        } else {
            let mut v = Vec::with_capacity(len);
            for i in 0..len {
                v.push(f(s, i));
            }
            String::Long(Gc::allocate(mc, v.into_boxed_slice()))
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
            String::Short8(l, b) => &b[0..*l as usize],
            String::Short32(l, b) => &b[0..*l as usize],
            String::Long(b) => b,
            String::Static(b) => b,
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

        match self {
            String::Short8(l, _) | String::Short32(l, _) => *l as i64,
            String::Long(b) => as_i64(b.len()),
            String::Static(b) => as_i64(b.len()),
        }
    }

    pub fn upper(&self, mc: MutationContext<'gc, '_>) -> String<'gc> {
        String::new_mapped(mc, self.as_bytes(), |from, idx| {
            from[idx].to_ascii_uppercase()
        })
    }

    pub fn lower(&self, mc: MutationContext<'gc, '_>) -> String<'gc> {
        String::new_mapped(mc, self.as_bytes(), |from, idx| {
            from[idx].to_ascii_lowercase()
        })
    }

    pub fn reverse(&self, mc: MutationContext<'gc, '_>) -> String<'gc> {
        String::new_mapped(mc, self.as_bytes(), |from, idx| from[from.len() - idx - 1])
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
