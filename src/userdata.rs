use std::hash::{Hash, Hasher};

use gc_arena::{barrier, lock, Collect, Mutation, Root, Rootable};
use thiserror::Error;

use crate::{any::AnyValue, Table};

#[derive(Debug, Copy, Clone, Error)]
#[error("UserData type mismatch")]
pub struct BadUserDataType;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct AnyUserData<'gc>(AnyValue<'gc, lock::Lock<Option<Table<'gc>>>>);

impl<'gc> PartialEq for AnyUserData<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<'gc> Eq for AnyUserData<'gc> {}

impl<'gc> Hash for AnyUserData<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

#[derive(Collect)]
#[collect(require_static)]
struct StaticRoot<R> {
    root: R,
}

impl<'a, R: 'static> Rootable<'a> for StaticRoot<R> {
    type Root = StaticRoot<R>;
}

impl<'gc> AnyUserData<'gc> {
    pub fn new<R>(mc: &Mutation<'gc>, val: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a>,
    {
        AnyUserData(AnyValue::new::<R>(mc, None.into(), val))
    }

    pub fn new_static<R: 'static>(mc: &Mutation<'gc>, val: R) -> Self {
        Self::new::<StaticRoot<R>>(mc, StaticRoot { root: val })
    }

    pub fn is<R>(&self) -> bool
    where
        R: for<'a> Rootable<'a>,
    {
        self.0.is::<R>()
    }

    pub fn is_static<R: 'static>(&self) -> bool {
        self.is::<StaticRoot<R>>()
    }

    pub fn downcast<'a, R>(&'a self) -> Result<&'gc Root<'gc, R>, BadUserDataType>
    where
        R: for<'b> Rootable<'b>,
    {
        self.0.downcast::<R>().ok_or(BadUserDataType)
    }

    pub fn downcast_write<'a, R>(
        &'a self,
        mc: &Mutation<'gc>,
    ) -> Result<&'gc barrier::Write<Root<'gc, R>>, BadUserDataType>
    where
        R: for<'b> Rootable<'b>,
    {
        self.0.downcast_write::<R>(mc).ok_or(BadUserDataType)
    }

    pub fn downcast_static<'a, R: 'static>(&'a self) -> Result<&'gc R, BadUserDataType> {
        self.0
            .downcast::<StaticRoot<R>>()
            .map(|r| &r.root)
            .ok_or(BadUserDataType)
    }

    pub fn metatable(&self) -> Option<Table<'gc>> {
        self.0.metadata().get()
    }

    pub fn set_metatable(
        &self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        self.0.write_metadata(mc).unlock().replace(metatable)
    }

    pub fn as_ptr(&self) -> *const () {
        self.0.as_ptr()
    }
}
