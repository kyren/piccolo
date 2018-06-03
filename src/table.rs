use std::hash::{Hash, Hasher};
use std::{f64, mem};

use fnv::FnvHashMap;
use gc_arena::{Gc, MutationContext};

use value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Table<'gc>(Gc<'gc, TableParts<'gc>>);

#[derive(Fail, Debug)]
#[fail(display = "table key is NaN")]
pub struct TableKeyNaN;

impl<'gc> PartialEq for Table<'gc> {
    fn eq(&self, other: &Table<'gc>) -> bool {
        &*self.0 as *const TableParts == &*other.0 as *const TableParts
    }
}

impl<'gc> Eq for Table<'gc> {}

impl<'gc> Hash for Table<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.0 as *const TableParts).hash(state);
    }
}

impl<'gc> Table<'gc> {
    pub fn new(&self, mc: MutationContext<'gc>) -> Table<'gc> {
        Table(Gc::allocate(
            mc,
            TableParts {
                array: Vec::new(),
                map: FnvHashMap::default(),
            },
        ))
    }
}

#[derive(Debug, Collect)]
struct TableParts<'gc> {
    array: Vec<Value<'gc>>,
    map: FnvHashMap<HashValue<'gc>, Value<'gc>>,
}

// Value which implements Hash and Eq, and cannot contain NaN values.
#[derive(Debug, Collect, PartialEq)]
struct HashValue<'gc>(Value<'gc>);

impl<'gc> Eq for HashValue<'gc> {}

impl<'gc> Hash for HashValue<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Nil => {
                Hash::hash(&1, state);
            }
            Value::Boolean(b) => {
                Hash::hash(&2, state);
                b.hash(state);
            }
            Value::Integer(i) => {
                Hash::hash(&3, state);
                i.hash(state);
            }
            Value::Number(n) => {
                Hash::hash(&4, state);
                canonical_float_bytes(*n).hash(state);
            }
            Value::String(s) => {
                Hash::hash(&5, state);
                s.hash(state);
            }
            Value::Table(t) => {
                Hash::hash(&6, state);
                t.hash(state);
            }
        }
    }
}

impl<'gc> HashValue<'gc> {
    fn new(value: Value<'gc>) -> Result<HashValue<'gc>, TableKeyNaN> {
        if let Value::Number(n) = value {
            if n.is_nan() {
                return Err(TableKeyNaN);
            }
        }
        Ok(HashValue(value))
    }
}

// Parameter must not be NaN, should return a bit-pattern which is always equal when the
// corresponding f64s are equal (-0.0 and 0.0 return the same bit pattern).
fn canonical_float_bytes(f: f64) -> u64 {
    debug_assert!(!f.is_nan());
    unsafe {
        if f == 0.0 {
            mem::transmute(0.0f64)
        } else {
            mem::transmute(f)
        }
    }
}
