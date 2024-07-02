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
    Found { key: Value<'gc>, value: Value<'gc> },
    Last,
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
                    .map(|(i, v)| (Value::Integer((i + 1).try_into().unwrap()), *v))
                    .chain({
                        self.map.iter().filter_map(|(k, v)| {
                            if let Key::Live(k) = k {
                                Some((k.to_value(), *v))
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
        Self {
            array: vec::Vec::new_in(MetricsAlloc::new(mc)),
            map: HashMap::with_hasher_in((), MetricsAlloc::new(mc)),
            hash_builder: ahash::random_state::RandomState::new(),
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
        let index_key = to_array_index(key);
        if let Some(index) = index_key {
            if index < self.array.len() {
                return Ok(mem::replace(&mut self.array[index], value));
            }
        }

        fn set_reserved_value<'gc>(
            map: &mut HashMap<Key<'gc>, Value<'gc>, (), MetricsAlloc<'gc>>,
            hash: u64,
            key: CanonicalKey<'gc>,
            value: Value<'gc>,
        ) -> Value<'gc> {
            match map.raw_entry_mut().from_hash(hash, |k| k.eq(key)) {
                hash_map::RawEntryMut::Occupied(occupied) => {
                    let (k, v) = occupied.into_key_value();
                    if k.is_dead_key() {
                        // Resurrect the key if it is dead.
                        *k = Key::Live(key);
                    }
                    mem::replace(v, value)
                }
                hash_map::RawEntryMut::Vacant(vacant) => {
                    vacant.insert_with_hasher(hash, Key::Live(key), value, |_| {
                        panic!("map slot must be pre-reserved")
                    });
                    Value::Nil
                }
            }
        }

        let table_key = CanonicalKey::new(key)?;
        let hash = self.hash_builder.hash_one(table_key);
        Ok(if value.is_nil() {
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
            }
        } else if self.map.len() < self.map.capacity() {
            set_reserved_value(&mut self.map, hash, table_key, value)
        } else {
            self.grow(index_key);

            // Now we can insert the new key value pair
            match index_key {
                Some(index) if index < self.array.len() => {
                    return Ok(mem::replace(&mut self.array[index], value));
                }
                _ => set_reserved_value(&mut self.map, hash, table_key, value),
            }
        })
    }

    fn grow(&mut self, index_key: Option<usize>) {
        // If a new element does not fit in either the array or map part of the table, we need
        // to grow. First, we find the total count of array candidate elements across the array
        // part, the map part, and the newly inserted key.

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
            // If we're growing the array part, we need to grow the array and take any newly
            // valid array keys from the map part.

            self.array.reserve(optimal_size - old_array_size);
            let capacity = self.array.capacity();
            self.array.resize(capacity, Value::Nil);

            let array = &mut self.array;
            self.map.retain(|k, v| {
                if v.is_nil() {
                    // If our entry is dead, remove it.
                    return false;
                }

                let key = k.live_key().expect("all dead keys should have a Nil value");

                // If our live key is an array index that fits in the array portion,
                // move the entry to the array portion.
                if let Some(i) = to_array_index(key.to_value()) {
                    if i < array.len() {
                        array[i] = *v;
                        return false;
                    }
                }

                true
            });
        } else {
            // If we aren't growing the array, we're adding a new element to the map that won't
            // fit in the advertised capacity. We explicitly double the map size here.
            self.map.raw_table_mut().reserve(old_map_size, |(key, _)| {
                self.hash_builder.hash_one(
                    key.live_key()
                        .expect("all keys must be live when table is grown"),
                )
            });
        }
    }

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
            // If there is no border in the array but the map part is empty, then the array length
            // is a border
            array_len
        } else {
            // Otherwise, we must check the map part for a border. We need to find some nil value in
            // the map part as the max for a binary search.
            let min = array_len;
            let mut max = array_len.checked_add(1).unwrap();
            while self
                .map
                .raw_entry()
                .from_hash(
                    self.hash_builder.hash_one(CanonicalKey::Integer(max)),
                    |k| k.eq(CanonicalKey::Integer(max)),
                )
                .is_some()
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

    pub fn next(&self, key: Value<'gc>) -> NextValue<'gc> {
        let start_index = if let Some(index_key) = to_array_index(key) {
            if index_key < self.array.len() {
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

    /// Try to efficiently remove a key from the array part of the
    /// table.  (`key` is one-indexed; if it is None, the length of
    /// the array is used instead.)
    ///
    /// If successful, returns the removed value; otherwise, indicates
    /// whether the operation is possible to implement with a fallback,
    /// or is impossible due to an out-of-range index.
    ///
    /// Additionally, always returns the computed length of the array
    /// from before the operation.
    pub fn array_remove_shift(
        &mut self,
        key: Option<i64>,
    ) -> (RawArrayOpResult<Value<'gc>>, usize) {
        let length = self.length() as usize;
        (self.array_remove_shift_inner(length, key), length)
    }

    fn array_remove_shift_inner(
        &mut self,
        length: usize,
        key: Option<i64>,
    ) -> RawArrayOpResult<Value<'gc>> {
        let index;
        if let Some(k) = key {
            if k == 0 && length == 0 || k == length as i64 + 1 {
                return RawArrayOpResult::Success(Value::Nil);
            } else if k >= 1 && k <= length as i64 {
                index = (k - 1) as usize;
            } else {
                return RawArrayOpResult::Failed;
            }
        } else {
            if length == 0 {
                return RawArrayOpResult::Success(Value::Nil);
            } else {
                index = length - 1;
            }
        }
        if length > self.array.len() {
            return RawArrayOpResult::Possible;
        }

        let value = mem::replace(&mut self.array[index], Value::Nil);
        if length - index > 1 {
            self.array[index..length].rotate_left(1);
        }
        RawArrayOpResult::Success(value)
    }

    /// Try to efficiently insert a key and value into the array part
    /// of the table.  (`key` is one-indexed; if it is `None`, the
    /// length of the array is used instead.)
    ///
    /// The returned [`RawArrayOpResult`] indicates whether the
    /// operation was successful, or if it failed,
    /// whether the operation is possible to implement with a fallback,
    /// or is impossible due to an out-of-range index.
    ///
    /// Additionally, always returns the computed length of the array
    /// from before the operation.
    pub fn array_insert_shift(
        &mut self,
        key: Option<i64>,
        value: Value<'gc>,
    ) -> (RawArrayOpResult<()>, usize) {
        let length = self.length() as usize;
        (self.array_insert_shift_inner(length, key, value), length)
    }

    fn array_insert_shift_inner(
        &mut self,
        length: usize,
        key: Option<i64>,
        value: Value<'gc>,
    ) -> RawArrayOpResult<()> {
        let index;
        if let Some(k) = key {
            if k >= 1 && k <= length as i64 + 1 {
                index = (k - 1) as usize;
            } else {
                return RawArrayOpResult::Failed;
            }
        } else {
            index = length;
        }
        if length > self.array.len() {
            return RawArrayOpResult::Possible;
        }

        assert!(index <= length);

        if length == self.array.len() {
            // If the array is full, try to grow it.
            self.grow(Some(index));
            if length >= self.array.len() {
                // If resize didn't grow the array, use fallback impl
                return RawArrayOpResult::Possible;
            }
        }

        // We know here that length < array.len(), so we shift each
        // element to the right by one.
        // array[length] == nil, which gets rotated back to array[index];
        // we replace it with the value to insert.
        self.array[index..=length].rotate_right(1);
        self.array[index] = value;
        RawArrayOpResult::Success(())
    }

    pub fn reserve_array(&mut self, additional: usize) {
        self.array.reserve(additional);
    }

    pub fn reserve_map(&mut self, additional: usize) {
        if additional > self.map.capacity() - self.map.len() {
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

#[derive(PartialEq, Debug)]
pub enum RawArrayOpResult<T> {
    Success(T),
    Possible,
    Failed,
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

#[cfg(test)]
mod tests {
    use gc_arena::rootless_arena;

    use super::*;

    fn dbg_table(table: &RawTable<'_>) -> std::string::String {
        format!("{:?} {:?}", table.array, table.map)
    }

    // This test will break if the table logic changes, but
    // it would implicitly break in that case anyways.
    #[test]
    fn test_raw_table_insert() {
        rootless_arena(|mc| {
            let mut table = RawTable::new(mc);

            assert_eq!("[] {}", dbg_table(&table));

            table.set(Value::Integer(1), Value::Integer(1)).unwrap();

            table.set(Value::Integer(5), Value::Integer(5)).unwrap();

            table.set(Value::Integer(2), Value::Integer(2)).unwrap();
            table.set(Value::Integer(3), Value::Integer(3)).unwrap();

            assert_eq!(
                "[Integer(1), Integer(2), Integer(3), Nil] {Live(Integer(5)): Integer(5)}",
                dbg_table(&table)
            );

            assert_eq!(table.length(), 3);
            assert_eq!(
                (RawArrayOpResult::Success(()), 3),
                table.array_insert_shift(Some(4), Value::Integer(4))
            );

            assert_eq!(
                "[Integer(1), Integer(2), Integer(3), Integer(4)] {Live(Integer(5)): Integer(5)}",
                dbg_table(&table)
            );

            assert_eq!(
                (RawArrayOpResult::Possible, 5),
                table.array_insert_shift(Some(5), Value::Integer(5))
            );

            assert_eq!(
                "[Integer(1), Integer(2), Integer(3), Integer(4)] {Live(Integer(5)): Integer(5)}",
                dbg_table(&table)
            );
        });
    }
}
