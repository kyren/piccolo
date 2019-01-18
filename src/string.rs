use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::ops::Deref;

use rustc_hash::FxHashSet;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::value::Value;

#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum StringError {
    Concat { bad_type: &'static str },
}

impl fmt::Display for StringError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StringError::Concat { bad_type } => write!(fmt, "cannot concat {}", bad_type),
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub enum String<'gc> {
    Short8(u8, Gc<'gc, [u8; 8]>),
    Short32(u8, Gc<'gc, [u8; 32]>),
    Long(Gc<'gc, Box<[u8]>>),
    Static(&'static [u8]),
}

impl<'gc> String<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, s: &[u8]) -> String<'gc> {
        let len = s.len();
        if len <= 8 {
            let mut b = [0; 8];
            b[..len].copy_from_slice(s);
            String::Short8(len as u8, Gc::allocate(mc, b))
        } else if len <= 32 {
            let mut b = [0; 32];
            b[..len].copy_from_slice(s);
            String::Short32(len as u8, Gc::allocate(mc, b))
        } else {
            String::Long(Gc::allocate(mc, s.to_vec().into_boxed_slice()))
        }
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
                Value::Closure(_) => {
                    return Err(StringError::Concat {
                        bad_type: "closure",
                    });
                }
            }
        }
        Ok(String::Long(Gc::allocate(mc, bytes.into_boxed_slice())))
    }

    pub fn new_static(&self, s: &'static [u8]) -> String<'gc> {
        String::Static(s)
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            String::Short8(l, b) => &b[0..*l as usize],
            String::Short32(l, b) => &b[0..*l as usize],
            String::Long(b) => b,
            String::Static(b) => b,
        }
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
#[collect(require_copy)]
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
