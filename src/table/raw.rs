use std::{fmt, hash::Hash, i64, mem};

use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Gc, Mutation};
use hashbrown::{hash_map, HashMap};
use thiserror::Error;

use crate::{Callback, Closure, Function, String, Table, Thread, UserData, Value};

#[derive(Debug, Copy, Clone, Error)]
pub enum InvalidTableKey {
    #[error("table key is NaN")]
    IsNaN,
    #[error("table key is Nil")]
    IsNil,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum NextValue<'gc> {
    /// The key provided to [`Table::next`] was found and there is an element present after it.
    ///
    /// As a special case, this may be returned if the key provided to [`Table::next`] was an
    /// integer that is a valid index into the array part of the table, *whether or not* that key
    /// is actually present. This matches the behavior of PUC-Rio Lua and is necessary to allow any
    /// field to be set to `Nil` during iteration.
    Found { key: Value<'gc>, value: Value<'gc> },
    /// The key provided to [`Table::next`] was found and it was the last entry in iteration order.
    Last,
    /// The key provided to [`Table::next`] did not match an existing entry.
    NotFound,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct RawTable<'gc> {
    array: vec::Vec<Value<'gc>, MetricsAlloc<'gc>>,
    // TODO: It would be safer to use `hashbrown::HashTable` and access the inner raw table when
    // necessary, but `HashTable` does not allow access to the inner raw table yet.
    map: HashMap<Key<'gc>, Value<'gc>, (), MetricsAlloc<'gc>>,
    #[collect(require_static)]
    hash_builder: ahash::random_state::RandomState,
}

impl<'gc> fmt::Debug for RawTable<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.array
                    .iter()
                    .enumerate()
                    .map(|(i, v)| {
                        (
                            Value::Integer((i + 1).try_into().unwrap()).debug_shallow(),
                            v.debug_shallow(),
                        )
                    })
                    .chain({
                        self.map.iter().filter_map(|(k, v)| {
                            if let Key::Live(k) = k {
                                Some((k.to_value().debug_shallow(), v.debug_shallow()))
                            } else {
                                None
                            }
                        })
                    }),
            )
            .finish()
    }
}

impl<'gc> RawTable<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self::with_capacity(mc, 0, 0)
    }

    pub fn with_capacity(mc: &Mutation<'gc>, array_capacity: usize, map_capacity: usize) -> Self {
        let mut array = vec::Vec::new_in(MetricsAlloc::new(mc));
        array.resize(array_capacity, Value::Nil);

        let map = HashMap::with_capacity_and_hasher_in(map_capacity, (), MetricsAlloc::new(mc));

        let hash_builder = ahash::random_state::RandomState::new();

        Self {
            array,
            map,
            hash_builder,
        }
    }

    pub fn get(&self, key: Value<'gc>) -> Value<'gc> {
        if let Some(index) = to_array_index(key) {
            if index < self.array.len() {
                return self.array[index];
            }
        }

        if let Ok(key) = CanonicalKey::new(key) {
            if let Some((_, v)) = self
                .map
                .raw_entry()
                .from_hash(self.hash_builder.hash_one(key), |k| k.eq(key))
            {
                *v
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        }
    }

    pub fn set(
        &mut self,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        // If the key is an array candidate and less than the current length of the array, it will
        // go there.
        let index_key = to_array_index(key);
        if let Some(index) = index_key {
            if index < self.array.len() {
                return Ok(mem::replace(&mut self.array[index], value));
            }
        }

        let table_key = CanonicalKey::new(key)?;
        let hash = self.hash_builder.hash_one(table_key);

        // If the value is nil then we are removing from the map part, which cannot fail.
        if value.is_nil() {
            return Ok(
                if let hash_map::RawEntryMut::Occupied(mut occupied) = self
                    .map
                    .raw_entry_mut()
                    .from_hash(hash, |k| k.eq(table_key))
                {
                    let (k, v) = occupied.get_key_value_mut();
                    if let Some(dead) = k.kill() {
                        *k = dead;
                    }
                    mem::take(v)
                } else {
                    Value::Nil
                },
            );
        }

        // If there is an existing entry in the map part, replace it, otherwise try to fit a new
        // entry.
        let raw_map = self.map.raw_table_mut();
        if let Some(bucket) = raw_map.find(hash, |(k, _)| k.eq(table_key)) {
            let (k, v) = unsafe { bucket.as_mut() };
            if k.is_dead_key() {
                // Resurrect the key if it is dead.
                *k = Key::Live(table_key);
            }
            return Ok(mem::replace(v, value));
        } else if raw_map
            .try_insert_no_grow(hash, (Key::Live(table_key), value))
            .is_ok()
        {
            return Ok(Value::Nil);
        }

        // If a new element does not fit in either the array or map part of the table, we need to
        // grow.

        if let Some(index_key) = index_key {
            // We have an array-candidate key, so we'd like to grow the array and place it there,
            // if possible.

            // First, we find the total count of array candidate elements across the array part, the
            // map part, and the newly inserted key.

            const USIZE_BITS: usize = mem::size_of::<usize>() * 8;

            // Count of array-candidate elements based on the highest bit in the index
            let mut array_counts = [0; USIZE_BITS];
            // Total count of all array-candidate elements
            let mut array_total = 0;

            for (i, e) in self.array.iter().enumerate() {
                if !e.is_nil() {
                    array_counts[highest_bit(i)] += 1;
                    array_total += 1;
                }
            }

            for (&key, &value) in &self.map {
                if !value.is_nil() {
                    if let Some(i) = to_array_index(
                        key.live_key()
                            .expect("dead keys must have Nil values")
                            .to_value(),
                    ) {
                        array_counts[highest_bit(i)] += 1;
                        array_total += 1;
                    }
                }
            }

            array_counts[highest_bit(index_key)] += 1;
            array_total += 1;

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

            // If we can fit our new key in an optimally sized array, resize the array and do that.
            if optimal_size > index_key {
                self.grow_array(optimal_size - self.array.len());
                // If the value is non-nil, it should have been replaced in the map part without
                // needing to grow growing.
                debug_assert!(self.array[index_key].is_nil());
                self.array[index_key] = value;
                return Ok(Value::Nil);
            }
        }

        // If we can't grow the array, we need to grow the map and place the key there. We
        // explicitly double the size of the map.
        self.reserve_map(self.map.len().max(1));

        // Now we can insert the new key value pair
        self.map
            .raw_table_mut()
            .try_insert_no_grow(hash, (Key::Live(table_key), value))
            .unwrap();

        Ok(Value::Nil)
    }

    /// Returns a 'border' for this table.
    ///
    /// See [`Table::length`] for a more full description of what this means.
    pub fn length(&self) -> i64 {
        // Binary search for a border. Entry at max must be Nil, min must be 0 or entry at min must
        // be != Nil.
        fn binary_search<F: Fn(i64) -> bool>(mut min: i64, mut max: i64, is_nil: F) -> i64 {
            while max - min > 1 {
                let mid = min + (max - min) / 2;
                if is_nil(mid) {
                    max = mid;
                } else {
                    min = mid;
                }
            }
            min
        }

        let array_len: i64 = self.array.len().try_into().unwrap();

        if !self.array.is_empty() && self.array[array_len as usize - 1].is_nil() {
            // If the array part ends in a Nil, there must be a border inside it
            binary_search(0, array_len, |i| self.array[i as usize - 1].is_nil())
        } else if self.map.is_empty() {
            // If the array part does not end in a nil but the map part is empty, then the array
            // length is a border.
            array_len
        } else {
            // Otherwise, we check the map part for a border. We need to find some nil value in the
            // map part as the max for a binary search.
            let min = array_len;
            let mut max = array_len.checked_add(1).unwrap();
            while self
                .map
                .raw_entry()
                .from_hash(
                    self.hash_builder.hash_one(CanonicalKey::Integer(max)),
                    |k| k.eq(CanonicalKey::Integer(max)),
                )
                .is_some_and(|(_, v)| !v.is_nil())
            {
                if max == i64::MAX {
                    // If we can't find a nil entry by doubling, then the table is pathological. We
                    // return the favor with a pathological answer: i64::MAX + 1 can't exist in the
                    // table, therefore it is Nil, so since the table contains i64::MAX, i64::MAX is
                    // a border.
                    return i64::MAX;
                } else if let Some(double_max) = max.checked_mul(2) {
                    max = double_max;
                } else {
                    max = i64::MAX;
                }
            }

            // We have found a max where table[max] == nil, so we can now binary search
            binary_search(min, max, |i| {
                match self
                    .map
                    .raw_entry()
                    .from_hash(self.hash_builder.hash_one(CanonicalKey::Integer(i)), |k| {
                        k.eq(CanonicalKey::Integer(i))
                    }) {
                    Some((_, v)) => v.is_nil(),
                    None => true,
                }
            })
        }
    }

    /// Returns the next key for this table in iteration order following the given `key`.
    ///
    /// See [`Table::next`] for a more full description of what this means.
    pub fn next(&self, key: Value<'gc>) -> NextValue<'gc> {
        // `start_index` being set means that we will scan for the first non-nil entry greater than
        // or equal to this (0-based) index.
        let start_index = if let Some(index_key) = to_array_index(key) {
            if index_key < self.array.len() {
                // In order to satisfy our rule that setting an entry to `Nil` is always allowed
                // during iteration, we must allow being provided a missing entry in the array
                // portion.
                Some(index_key + 1)
            } else {
                None
            }
        } else if key.is_nil() {
            Some(0)
        } else {
            None
        };

        let raw_table = self.map.raw_table();

        // If `start_index` is set, then we search the array portion past `start_index` for any
        // non-nil values, otherwise we return the first entry with a non-nil value in the map
        // portion (which always follows the array portion in our iteration order).
        if let Some(start_index) = start_index {
            for i in start_index..self.array.len() {
                if !self.array[i].is_nil() {
                    return NextValue::Found {
                        key: Value::Integer((i + 1).try_into().unwrap()),
                        value: self.array[i],
                    };
                }
            }

            unsafe {
                for bucket_index in 0..raw_table.buckets() {
                    if raw_table.is_bucket_full(bucket_index) {
                        let (key, value) = *raw_table.bucket(bucket_index).as_ref();
                        if !value.is_nil() {
                            return NextValue::Found {
                                key: key
                                    .live_key()
                                    .expect("dead keys must have e values")
                                    .to_value(),
                                value,
                            };
                        }
                    }
                }
            }

            return NextValue::Last;
        }

        // Otherwise, if we were given a key present in the map portion, we return the key following
        // it in bucket order.
        if let Ok(table_key) = CanonicalKey::new(key) {
            if let Some(bucket) = raw_table.find(self.hash_builder.hash_one(table_key), |(k, _)| {
                k.eq(table_key)
            }) {
                unsafe {
                    let bucket_index = raw_table.bucket_index(&bucket);
                    for i in bucket_index + 1..raw_table.buckets() {
                        if raw_table.is_bucket_full(i) {
                            let (key, value) = *raw_table.bucket(i).as_ref();
                            if !value.is_nil() {
                                return NextValue::Found {
                                    key: key
                                        .live_key()
                                        .expect("dead keys must have Nil values")
                                        .to_value(),
                                    value,
                                };
                            }
                        }
                    }
                }
                return NextValue::Last;
            }
        }

        NextValue::NotFound
    }

    /// Return the array part of the table as a slice.
    ///
    /// All integer keys that *can* fit into the array part of the table will always be stored in
    /// the array. This means that if you have an integer key `1 <= key <= array.len()`, then you
    /// can look up the value for this key with `array[key - 1]`. Such keys that are not present in
    /// the table will have a value in the array of `Value::Nil`.
    ///
    /// The length of the array part of the table and the length of the table itself are
    /// independent. There may be integral keys *outside* of the array part of the table, even for
    /// sequences.
    ///
    /// If you need to treat the table as purely array-like, and need to ensure that the entire
    /// sequence fits in the array part of the table, you can grow the array part of the table to be
    /// equal to its current length. Remember that non-sequence tables can be pathological, and can
    /// have arbitrarily large borders (numbers returned by `RawTable::length()`).
    pub fn array(&self) -> &[Value<'gc>] {
        &self.array
    }

    /// Return the array part of the table as a mutable slice.
    ///
    /// All values that *can* be stored in the array part of the table will always be stored
    /// there, so this comes with no invariants that you must uphold. Writing a value to `i` in the
    /// table is equivalent to setting the key `i + 1` in the table, and writing `Value::Nil` is
    /// equivalent to removing the key.
    pub fn array_mut(&mut self) -> &mut [Value<'gc>] {
        &mut self.array
    }

    /// Grow the array part of the table to accommodate for at *least* `additional` more elements.
    ///
    /// The array part always has a length equal to its capacity, but may have trailing nil values
    /// at the end.
    ///
    /// The map part of the table is forbidden from having keys that could be in the array part, so
    /// this may move elements from the map part of the table into the array part.
    pub fn grow_array(&mut self, additional: usize) {
        self.array.reserve(additional);
        self.array.resize(self.array.capacity(), Value::Nil);

        // We need to take any newly valid array keys from the map part.
        self.map.retain(|k, v| {
            if v.is_nil() {
                // If our entry is dead, remove it.
                return false;
            }

            let key = k.live_key().expect("all dead keys should have a Nil value");

            // If our live key is an array index that fits in the array portion, move the entry to
            // the array portion.
            if let Some(i) = to_array_index(key.to_value()) {
                if i < self.array.len() {
                    self.array[i] = *v;
                    return false;
                }
            }

            true
        });
    }

    /// Reserve space in the map part of the table for at least `additional` more elements.
    pub fn reserve_map(&mut self, additional: usize) {
        if additional > self.map.capacity() - self.map.len() {
            // We always filter out all dead keys when growing the map.
            self.map.retain(|_, v| !v.is_nil());

            self.map.raw_table_mut().reserve(additional, |(key, _)| {
                self.hash_builder.hash_one(
                    key.live_key()
                        .expect("all keys must be live when table is grown"),
                )
            });
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(no_drop)]
enum CanonicalKey<'gc> {
    Boolean(bool),
    Integer(i64),
    Number(u64),
    String(String<'gc>),
    Table(Table<'gc>),
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
    Thread(Thread<'gc>),
    UserData(UserData<'gc>),
}

impl<'gc> CanonicalKey<'gc> {
    fn new(value: Value<'gc>) -> Result<Self, InvalidTableKey> {
        Ok(match value {
            Value::Nil => {
                return Err(InvalidTableKey::IsNil);
            }
            Value::Number(n) => {
                // NaN keys are disallowed, f64 keys where their closest i64 representation is equal
                // to themselves when cast back to f64 are considered integer keys.
                if n.is_nan() {
                    return Err(InvalidTableKey::IsNaN);
                } else if let Some(i) = f64_to_i64(n) {
                    CanonicalKey::Integer(i)
                } else {
                    CanonicalKey::Number(canonical_float_bytes(n))
                }
            }
            Value::Boolean(b) => CanonicalKey::Boolean(b),
            Value::Integer(i) => CanonicalKey::Integer(i),
            Value::String(s) => CanonicalKey::String(s),
            Value::Table(t) => CanonicalKey::Table(t),
            Value::Function(Function::Closure(c)) => CanonicalKey::Closure(c),
            Value::Function(Function::Callback(c)) => CanonicalKey::Callback(c),
            Value::Thread(t) => CanonicalKey::Thread(t),
            Value::UserData(u) => CanonicalKey::UserData(u),
        })
    }

    fn to_value(self) -> Value<'gc> {
        match self {
            CanonicalKey::Boolean(b) => b.into(),
            CanonicalKey::Integer(i) => i.into(),
            CanonicalKey::Number(n) => f64::from_bits(n).into(),
            CanonicalKey::String(s) => s.into(),
            CanonicalKey::Table(t) => t.into(),
            CanonicalKey::Closure(c) => c.into(),
            CanonicalKey::Callback(c) => c.into(),
            CanonicalKey::Thread(t) => t.into(),
            CanonicalKey::UserData(u) => u.into(),
        }
    }
}

// A table key which may be "live" or "dead".
//
// "Removed" keys in tables do not actually have their entry removed, instead the value is set to
// Nil and the key is set to a dead key if it is a GC object.
//
// This is done to make iteration predictable in the presence of any table mutation that does not
// cause the table to grow.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
enum Key<'gc> {
    Live(CanonicalKey<'gc>),
    Dead(#[collect(require_static)] *const ()),
}

impl<'gc> Key<'gc> {
    fn kill(self) -> Option<Key<'gc>> {
        if let Key::Live(v) = self {
            match v {
                CanonicalKey::Boolean(_) | CanonicalKey::Integer(_) | CanonicalKey::Number(_) => {
                    None
                }
                CanonicalKey::String(s) => Some(Key::Dead(Gc::as_ptr(s.into_inner()) as *const ())),
                CanonicalKey::Table(t) => Some(Key::Dead(Gc::as_ptr(t.into_inner()) as *const ())),
                CanonicalKey::Closure(c) => {
                    Some(Key::Dead(Gc::as_ptr(c.into_inner()) as *const ()))
                }
                CanonicalKey::Callback(c) => {
                    Some(Key::Dead(Gc::as_ptr(c.into_inner()) as *const ()))
                }
                CanonicalKey::Thread(t) => Some(Key::Dead(Gc::as_ptr(t.into_inner()) as *const ())),
                CanonicalKey::UserData(u) => {
                    Some(Key::Dead(Gc::as_ptr(u.into_inner()) as *const ()))
                }
            }
        } else {
            Some(self)
        }
    }

    fn is_dead_key(&self) -> bool {
        self.live_key().is_none()
    }

    fn live_key(self) -> Option<CanonicalKey<'gc>> {
        match self {
            Key::Live(v) => Some(v),
            Key::Dead(_) => None,
        }
    }

    fn eq(self, key: CanonicalKey<'gc>) -> bool {
        match (self, key) {
            (Key::Live(a), b) => a == b,
            (Key::Dead(a), b) => match b {
                CanonicalKey::String(s) => a == Gc::as_ptr(s.into_inner()) as *const (),
                CanonicalKey::Table(t) => a == Gc::as_ptr(t.into_inner()) as *const (),
                CanonicalKey::Closure(c) => a == Gc::as_ptr(c.into_inner()) as *const (),
                CanonicalKey::Callback(c) => a == Gc::as_ptr(c.into_inner()) as *const (),
                CanonicalKey::Thread(t) => a == Gc::as_ptr(t.into_inner()) as *const (),
                CanonicalKey::UserData(u) => a == Gc::as_ptr(u.into_inner()) as *const (),
                _ => false,
            },
        }
    }
}

// Returns the closest i64 to a given f64 such that casting the i64 back to an f64 results in an
// equal value, if such an integer exists.
fn f64_to_i64(n: f64) -> Option<i64> {
    let i = n as i64;
    if i as f64 == n {
        Some(i)
    } else {
        None
    }
}

// Parameter must not be NaN, should return a bit-pattern which is always equal when the
// corresponding f64s are equal (-0.0 and 0.0 return the same bit pattern).
fn canonical_float_bytes(f: f64) -> u64 {
    assert!(!f.is_nan());
    if f == 0.0 {
        0.0f64.to_bits()
    } else {
        f.to_bits()
    }
}

// If the given key can live in the array part of the table (integral value between 1 and
// usize::MAX), returns the associated array index.
fn to_array_index<'gc>(key: Value<'gc>) -> Option<usize> {
    let i = match key {
        Value::Integer(i) => i,
        Value::Number(f) => f64_to_i64(f)?,
        _ => return None,
    };

    if i > 0 {
        Some(usize::try_from(i).ok()? - 1)
    } else {
        None
    }
}

// Returns the place of the highest set bit in the given i, i = 0 returns 0, i = 1 returns 1, i = 2
// returns 2, i = 3 returns 2, and so on.
fn highest_bit(i: usize) -> usize {
    i.checked_ilog2().map(|i| i + 1).unwrap_or(0) as usize
}
