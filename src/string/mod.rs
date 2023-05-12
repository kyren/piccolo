mod repr;

pub use repr::String;

use std::{
    borrow::Borrow,
    error::Error as StdError,
    fmt,
    hash::{Hash, Hasher},
    io::Write,
    ops::Deref,
    str,
    string::String as StdString,
};

use gc_arena::{lock::RefLock, Collect, Gc, MutationContext};
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
