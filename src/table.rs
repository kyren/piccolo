use std::hash::{Hash, Hasher};
use std::mem;

use fnv::FnvHashMap;
use num_traits;

use gc_arena::{GcCell, MutationContext};

use value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Table<'gc>(GcCell<'gc, TableState<'gc>>);

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
        Table(GcCell::allocate(mc, TableState::default()))
    }

    pub fn get(&self, key: Value<'gc>) -> Value<'gc> {
        self.0.read().get(key)
    }

    pub fn set(
        &self,
        mc: MutationContext<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        self.0.write(mc).set(key, value)
    }
}

#[derive(Debug, Collect, Default)]
struct TableState<'gc> {
    array: Vec<Value<'gc>>,
    map: FnvHashMap<HashValue<'gc>, Value<'gc>>,
}

impl<'gc> TableState<'gc> {
    fn get(&self, key: Value<'gc>) -> Value<'gc> {
        if let Some(index) = to_array_index(key) {
            if index < self.array.len() {
                return self.array[index];
            }
        }

        if let Ok(key) = HashValue::new(key) {
            self.map.get(&key).cloned().unwrap_or(Value::Nil)
        } else {
            Value::Nil
        }
    }

    fn set(&mut self, key: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, InvalidTableKey> {
        let index_key = to_array_index(key);
        if let Some(index) = index_key {
            if index < self.array.len() {
                return Ok(mem::replace(&mut self.array[index], value));
            }
        }

        let hash_key = HashValue::new(key)?;
        if value == Value::Nil {
            Ok(self.map.remove(&hash_key).unwrap_or(Value::Nil))
        } else if self.map.len() < self.map.capacity() {
            Ok(self.map.insert(hash_key, value).unwrap_or(Value::Nil))
        } else {
            // If a new element does not fit in either the array or map part of the table, we need
            // to grow.  First, we find the total count of array candidate elements across the array
            // part, the map part, and the newly inserted key.

            const USIZE_BITS: usize = mem::size_of::<usize>() * 8;

            // Count of array-candidate elements based on the highest bit in the index
            let mut array_counts = [0; USIZE_BITS];
            // Total count of all array-candidate elements
            let mut array_total = 0;

            for (i, e) in self.array.iter().enumerate() {
                if *e != Value::Nil {
                    array_counts[highest_bit(i)] += 1;
                    array_total += 1;
                }
            }

            for (k, _) in &self.map {
                if let Some(i) = to_array_index(k.0) {
                    array_counts[highest_bit(i)] += 1;
                    array_total += 1;
                }
            }

            if let Some(i) = index_key {
                array_counts[highest_bit(i)] += 1;
                array_total += 1;
            }

            // Then, we compute the new optimal size for the array by finding the largest array size
            // such that at least half of the elements in the array would be in use.

            let mut optimal_size = 0;
            let mut total = 0;
            for i in 0..USIZE_BITS {
                if (1 << i) / 2 >= array_total {
                    break;
                }

                if array_counts[i] > 0 {
                    total += array_counts[i];
                    if total > (1 << i) / 2 {
                        optimal_size = 1 << i;
                    }
                }
            }

            let old_array_size = self.array.len();
            let old_map_size = self.map.len();
            if optimal_size > old_array_size {
                // If we're growing the array part, we need to grow the array and take any newly valid
                // array keys from the map part.

                self.array.reserve(optimal_size - old_array_size);
                let capacity = self.array.capacity();
                self.array.resize(capacity, Value::Nil);

                let array = &mut self.array;
                self.map.retain(|k, v| {
                    if let Some(i) = to_array_index(k.0) {
                        if i < array.len() {
                            array[i] = *v;
                            return false;
                        }
                    }
                    true
                });
            } else {
                // If we aren't growing the array, we're adding a new element to the map that won't
                // fit in the advertised capacity.  The capacity of std::collections::HashMap is
                // just a lower-bound, so we may actually be able to insert past the capacity
                // without the advertised capacity growing, so to make sure that we don't try to
                // grow repeatedly, we need to make sure the capacity actually increases.  We simply
                // double the capacity here.
                self.map.reserve(old_map_size);
            }

            // Now we can insert the new key value pair
            if let Some(index) = index_key {
                if index < self.array.len() {
                    return Ok(mem::replace(&mut self.array[index], value));
                }
            }
            Ok(self.map.insert(hash_key, value).unwrap_or(Value::Nil))
        }
    }
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

// If the given key can live in the array part of the table (integral value between 1 and
// usize::MAX), returns the associated array index.
fn to_array_index<'gc>(key: Value<'gc>) -> Option<usize> {
    let i = match key {
        Value::Integer(i) => num_traits::cast::<_, usize>(i)?,
        Value::Number(f) => {
            let i = num_traits::cast::<_, usize>(f)?;
            if i as f64 != f {
                return None;
            }
            i
        }
        _ => {
            return None;
        }
    };

    if i > 0 {
        Some(i - 1)
    } else {
        None
    }
}

// Returns the place of the highest set bit in the given i, i = 0 returns 0, i = 1 returns 1, i = 2
// returns 2, i = 3 returns 2, and so on.
fn highest_bit(mut i: usize) -> usize {
    const LOG_2: [u8; 256] = [
        0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    ];

    let mut hb = 0;
    while i >= 256 {
        hb += 8;
        i = i >> 8;
    }

    hb + LOG_2[i] as usize
}
