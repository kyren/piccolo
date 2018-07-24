use std::hash::{Hash, Hasher};
use std::mem;

use fnv::FnvHashMap;
use num_traits;

use gc_arena::{GcCell, MutationContext};

use value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Table<'gc>(GcCell<'gc, TableParts<'gc>>);

#[derive(Fail, Debug)]
pub enum InvalidTableKey {
    #[fail(display = "table key is NaN")]
    IsNaN,
    #[fail(display = "table key is Nil")]
    IsNil,
}

impl<'gc> PartialEq for Table<'gc> {
    fn eq(&self, other: &Table<'gc>) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<'gc> Eq for Table<'gc> {}

impl<'gc> Hash for Table<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

impl<'gc> Table<'gc> {
    pub fn new(&self, mc: MutationContext<'gc>) -> Table<'gc> {
        Table(GcCell::allocate(
            mc,
            TableParts {
                array: Vec::new(),
                map: FnvHashMap::default(),
            },
        ))
    }

    pub fn get(&self, key: Value<'gc>) -> Value<'gc> {
        let parts = self.0.read();
        match TableKey::new(key, parts.array.len()) {
            Ok(TableKey::Array(i)) => parts.array[i],
            Ok(TableKey::Map(k)) => parts.map.get(&k).cloned().unwrap_or(Value::Nil),
            Err(_) => Value::Nil,
        }
    }

    pub fn set(&self, mc: MutationContext<'gc>, key: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, InvalidTableKey> {
        let mut parts = self.0.write(mc);
        match TableKey::new(key, parts.array.len())? {
            TableKey::Array(i) => Ok(mem::replace(&mut parts.array[i], value)),
            TableKey::Map(k) => if parts.map.len() < parts.map.capacity() {
                Ok(parts.map.insert(k, value).unwrap_or(Value::Nil))
            } else {
                unimplemented!()
            }
        }
    }
}

#[derive(Debug, Collect)]
struct TableParts<'gc> {
    array: Vec<Value<'gc>>,
    map: FnvHashMap<HashValue<'gc>, Value<'gc>>,
}

// Value which implements Hash and Eq, and cannot contain Nil or NaN values.
#[derive(Debug, Collect, PartialEq)]
struct HashValue<'gc>(Value<'gc>);

impl<'gc> Eq for HashValue<'gc> {}

impl<'gc> Hash for HashValue<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Nil => unreachable!(),
            Value::Boolean(b) => {
                Hash::hash(&1, state);
                b.hash(state);
            }
            Value::Integer(i) => {
                Hash::hash(&2, state);
                i.hash(state);
            }
            Value::Number(n) => {
                Hash::hash(&3, state);
                canonical_float_bytes(*n).hash(state);
            }
            Value::String(s) => {
                Hash::hash(&4, state);
                s.hash(state);
            }
            Value::Table(t) => {
                Hash::hash(&5, state);
                t.hash(state);
            }
        }
    }
}

impl<'gc> HashValue<'gc> {
    fn new(value: Value<'gc>) -> Result<HashValue<'gc>, InvalidTableKey> {
        match value {
            Value::Nil => Err(InvalidTableKey::IsNil),
            Value::Number(n) if n.is_nan() => Err(InvalidTableKey::IsNaN),
            v => Ok(HashValue(v)),
        }
    }
}

// Parameter must not be NaN, should return a bit-pattern which is always equal when the
// corresponding f64s are equal (-0.0 and 0.0 return the same bit pattern).
fn canonical_float_bytes(f: f64) -> u64 {
    assert!(!f.is_nan());
    unsafe {
        if f == 0.0 {
            mem::transmute(0.0f64)
        } else {
            mem::transmute(f)
        }
    }
}

enum TableKey<'gc> {
    Array(usize),
    Map(HashValue<'gc>),
}

impl<'gc> TableKey<'gc> {
    fn new(key: Value<'gc>, array_len: usize) -> Result<TableKey<'gc>, InvalidTableKey> {
        let mut array_index = None;
        match key {
            Value::Integer(i) => array_index = num_traits::cast::<_, usize>(i),
            Value::Number(f) => {
                if let Some(i) = num_traits::cast::<_, usize>(f) {
                    if i as f64 == f {
                        array_index = Some(i);
                    }
                }
            }
            _ => {}
        }

        if let Some(ind) = array_index {
            if ind < array_len {
                return Ok(TableKey::Array(ind))
            }
        }

        Ok(TableKey::Map(HashValue::new(key)?))
    }
}
