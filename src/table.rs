use std::hash::{Hash, Hasher};

use fnv::FnvHashMap;
use gc_arena::Gc;

use value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Table<'gc>(Gc<'gc, TableParts<'gc>>);

#[derive(Debug, Collect)]
struct TableParts<'gc> {
    array: Vec<Value<'gc>>,
    map: FnvHashMap<HashValue<'gc>, Value<'gc>>,
}

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

#[derive(Debug, Collect, PartialEq)]
struct HashValue<'gc>(Value<'gc>);

impl<'gc> Eq for HashValue<'gc> {}

impl<'gc> Hash for HashValue<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unimplemented!()
    }
}
