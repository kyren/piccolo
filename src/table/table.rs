use std::{
    hash::{Hash, Hasher},
    i64, mem,
};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::{Context, IntoValue, Value};

use super::raw::{InvalidTableKey, NextValue, RawTable};

pub type TableInner<'gc> = RefLock<TableState<'gc>>;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Table<'gc>(Gc<'gc, TableInner<'gc>>);

impl<'gc> PartialEq for Table<'gc> {
    fn eq(&self, other: &Table<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Table<'gc> {}

impl<'gc> Hash for Table<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc> Table<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Table<'gc> {
        Self::from_parts(mc, RawTable::new(mc), None)
    }

    pub fn from_parts(
        mc: &Mutation<'gc>,
        entries: RawTable<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Table<'gc> {
        Self(Gc::new(mc, RefLock::new(TableState { entries, metatable })))
    }

    pub fn from_inner(inner: Gc<'gc, TableInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, TableInner<'gc>> {
        self.0
    }

    pub fn get<K: IntoValue<'gc>>(self, ctx: Context<'gc>, key: K) -> Value<'gc> {
        self.get_value(key.into_value(ctx))
    }

    pub fn set<K: IntoValue<'gc>, V: IntoValue<'gc>>(
        self,
        ctx: Context<'gc>,
        key: K,
        value: V,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        self.set_value(&ctx, key.into_value(ctx), value.into_value(ctx))
    }

    pub fn get_value(self, key: Value<'gc>) -> Value<'gc> {
        self.0.borrow().entries.get(key)
    }

    pub fn set_value(
        self,
        mc: &Mutation<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        self.0.borrow_mut(&mc).entries.set(key, value)
    }

    /// Returns a 'border' for this table.
    ///
    /// A 'border' for a table is any i >= 0 where:
    /// `(i == 0 or table[i] ~= nil) and table[i + 1] == nil`
    ///
    /// If a table has exactly one border, it is called a 'sequence', and this border is the table's
    /// length.
    pub fn length(self) -> i64 {
        self.0.borrow().entries.length()
    }

    /// Returns the next value after this key in the table order.
    ///
    /// The table order in the map portion of the table is defined by the incidental order of the
    /// internal bucket list. This order may change whenever the bucket list changes size, such
    /// as when inserting into the table, so relying on the order while inserting may result in
    /// unspecified (but not unsafe) behavior.
    ///
    /// If given Nil, it will return the first pair in the table. If given a key that is present
    /// in the table, it will return the next pair in iteration order. If given a key that is not
    /// present in the table, the behavior is unspecified.
    pub fn next(self, key: Value<'gc>) -> NextValue<'gc> {
        self.0.borrow().entries.next(key)
    }

    /// Iterate over the key-value pairs of the table.
    ///
    /// Internally uses the `Table::next` method and thus matches the behavior of Lua.
    pub fn iter(self) -> Iter<'gc> {
        Iter::new(self)
    }

    pub fn metatable(self) -> Option<Table<'gc>> {
        self.0.borrow().metatable
    }

    pub fn set_metatable(
        self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        mem::replace(&mut self.0.borrow_mut(mc).metatable, metatable)
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Iter<'gc> {
    table: Table<'gc>,
    prev: Option<Value<'gc>>,
}

impl<'gc> Iter<'gc> {
    pub fn new(table: Table<'gc>) -> Self {
        Self {
            table,
            prev: Some(Value::Nil),
        }
    }
}

impl<'gc> Iterator for Iter<'gc> {
    type Item = (Value<'gc>, Value<'gc>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.table.next(self.prev.take()?) {
            NextValue::Found { key, value } => {
                self.prev = Some(key);
                Some((key, value))
            }
            _ => None,
        }
    }
}

impl<'gc> IntoIterator for Table<'gc> {
    type Item = (Value<'gc>, Value<'gc>);
    type IntoIter = Iter<'gc>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TableState<'gc> {
    pub entries: RawTable<'gc>,
    pub metatable: Option<Table<'gc>>,
}
