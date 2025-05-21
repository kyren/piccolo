use std::{
    fmt,
    hash::{Hash, Hasher},
    i64, mem,
};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::{Context, FromValue, IntoValue, TypeError, Value};

use super::raw::{InvalidTableKey, NextValue, RawTable};

pub type TableInner<'gc> = RefLock<TableState<'gc>>;

/// The primary Lua data structure.
///
/// A `Table` is a combination of an array and a map. It a map of [`Value`]s to other `Value`s, but
/// as an optimization, when keys are integral and start from 1, it stores them within an internal
/// array.
///
/// Entries with values of [`Value::Nil`] are transparently removed from the table.
///
/// When a `Table` has only integral keys starting from 1, and every value is non-nil, then it is
/// also known as a "sequence" and acts similar to how an array would in other languages.
///
/// All Lua tables can have another associated table called a "metatable" which governs how they
/// act in Lua code. In Lua code, operations on a table can trigger special "metamethods" in the
/// metatable (if they are present).
///
/// On the Rust side, all methods on `Table` are "raw", which in Lua jargon means that they
/// never trigger metamethods. This MUST be true, because `piccolo` does not (and cannot)
/// silently trigger running Lua code. In order to trigger metamethods, you must use the
/// [`meta_ops`](crate::meta_ops) module and manually run any triggered code on an
/// [`Executor`](crate::Executor).
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
        raw_table: RawTable<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Table<'gc> {
        Self(Gc::new(
            mc,
            RefLock::new(TableState {
                raw_table,
                metatable,
            }),
        ))
    }

    pub fn from_inner(inner: Gc<'gc, TableInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, TableInner<'gc>> {
        self.0
    }

    pub fn get<K: IntoValue<'gc>, V: FromValue<'gc>>(
        self,
        ctx: Context<'gc>,
        key: K,
    ) -> Result<V, TypeError> {
        V::from_value(ctx, self.get_value(ctx, key))
    }

    pub fn set<K: IntoValue<'gc>, V: IntoValue<'gc>>(
        self,
        ctx: Context<'gc>,
        key: K,
        value: V,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        self.set_raw(&ctx, key.into_value(ctx), value.into_value(ctx))
    }

    pub fn get_value<K: IntoValue<'gc>>(self, ctx: Context<'gc>, key: K) -> Value<'gc> {
        self.get_raw(key.into_value(ctx))
    }

    /// A convenience method over [`Table::set`] for setting a string field of a table.
    ///
    /// It behaves exactly the same as [`Table::set`], except since this only accepts string keys,
    /// we know it cannot possibly error.
    pub fn set_field<V: IntoValue<'gc>>(
        self,
        ctx: Context<'gc>,
        key: &'static str,
        value: V,
    ) -> Value<'gc> {
        self.set(ctx, key, value).unwrap()
    }

    /// Get a value from this table without any automatic type conversion.
    pub fn get_raw(self, key: Value<'gc>) -> Value<'gc> {
        self.0.borrow().raw_table.get(key)
    }

    /// Set a value in this table without any automatic type conversion.
    pub fn set_raw(
        self,
        mc: &Mutation<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Result<Value<'gc>, InvalidTableKey> {
        self.0.borrow_mut(&mc).raw_table.set(key, value)
    }

    /// Returns a 'border' for this table.
    ///
    /// A 'border' for a table is any i >= 0 where:
    /// `(i == 0 or table[i] ~= nil) and table[i + 1] == nil`
    ///
    /// If a table has exactly one border, it is called a 'sequence', and this border is the table's
    /// length.
    pub fn length(self) -> i64 {
        self.0.borrow().raw_table.length()
    }

    /// Returns the next value after this key in an unspecified table iteration order.
    ///
    /// The table order in the map portion of the table is defined by the incidental order of the
    /// internal bucket list. This order is only guaranteed to be stable if there are no inserts
    /// into the table, so iterating using this method while simultaneously inserting may result in
    /// unspecified (but never unsafe) behavior. As a special case, removing from the table (setting
    /// a value to [`Value::Nil`]) is *always* allowed, even for the currently returned key.
    ///
    /// If given `Nil`, it will return the first pair in the table. If given a key that is present
    /// in the table, it will return the next pair in iteration order. If given a key that is not
    /// present in the table, the behavior is unspecified.
    pub fn next(self, key: Value<'gc>) -> NextValue<'gc> {
        self.0.borrow().raw_table.next(key)
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

#[derive(Collect)]
#[collect(no_drop)]
pub struct TableState<'gc> {
    pub raw_table: RawTable<'gc>,
    pub metatable: Option<Table<'gc>>,
}

impl<'gc> fmt::Debug for TableState<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct ShallowTableDebug<'gc>(Table<'gc>);

        impl<'gc> fmt::Debug for ShallowTableDebug<'gc> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let t = self.0.into_inner().borrow();
                match t.metatable {
                    Some(meta) => f
                        .debug_struct("TableState")
                        .field("raw_table", &t.raw_table)
                        .field(
                            "metatable",
                            &Some(format_args!("Table({:p})", Gc::as_ptr(meta.into_inner()))),
                        )
                        .finish(),
                    None => f
                        .debug_struct("TableState")
                        .field("raw_table", &t.raw_table)
                        .field("metatable", &None::<()>)
                        .finish(),
                }
            }
        }

        f.debug_struct("TableState")
            .field("raw_table", &self.raw_table)
            .field(
                "metatable",
                &self.metatable.as_ref().map(|t| ShallowTableDebug(*t)),
            )
            .finish()
    }
}
