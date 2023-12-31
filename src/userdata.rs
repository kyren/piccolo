use std::{
    hash::{Hash, Hasher},
    mem,
};

use gc_arena::{barrier, lock, Collect, Gc, Mutation, Root, Rootable};
use thiserror::Error;

use crate::{
    any::{Any, AnyInner},
    Table,
};

#[derive(Debug, Copy, Clone, Error)]
#[error("UserData type mismatch")]
pub struct BadUserDataType;

#[derive(Debug, Copy, Clone, Default, Collect)]
#[collect(no_drop)]
pub struct UserDataMeta<'gc> {
    pub metatable: Option<Table<'gc>>,
}

pub type UserDataMetaState<'gc> = lock::Lock<UserDataMeta<'gc>>;
pub type UserDataInner<'gc> = AnyInner<UserDataMetaState<'gc>>;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct UserData<'gc>(Any<'gc, UserDataMetaState<'gc>>);

impl<'gc> PartialEq for UserData<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<'gc> Eq for UserData<'gc> {}

impl<'gc> Hash for UserData<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
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

impl<'gc> UserData<'gc> {
    pub fn new<R>(mc: &Mutation<'gc>, val: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a>,
    {
        UserData(Any::new::<R>(mc, val))
    }

    pub fn new_static<R: 'static>(mc: &Mutation<'gc>, val: R) -> Self {
        Self::new::<StaticRoot<R>>(mc, StaticRoot { root: val })
    }

    pub fn from_inner(inner: Gc<'gc, UserDataInner<'gc>>) -> Self {
        Self(Any::from_inner(inner))
    }

    pub fn into_inner(self) -> Gc<'gc, UserDataInner<'gc>> {
        self.0.into_inner()
    }

    pub fn is<R>(self) -> bool
    where
        R: for<'a> Rootable<'a>,
    {
        self.0.is::<R>()
    }

    pub fn is_static<R: 'static>(self) -> bool {
        self.is::<StaticRoot<R>>()
    }

    pub fn downcast<R>(self) -> Result<&'gc Root<'gc, R>, BadUserDataType>
    where
        R: for<'b> Rootable<'b>,
    {
        self.0.downcast::<R>().ok_or(BadUserDataType)
    }

    pub fn downcast_write<R>(
        self,
        mc: &Mutation<'gc>,
    ) -> Result<&'gc barrier::Write<Root<'gc, R>>, BadUserDataType>
    where
        R: for<'b> Rootable<'b>,
    {
        self.0.downcast_write::<R>(mc).ok_or(BadUserDataType)
    }

    pub fn downcast_static<R: 'static>(self) -> Result<&'gc R, BadUserDataType> {
        self.0
            .downcast::<StaticRoot<R>>()
            .map(|r| &r.root)
            .ok_or(BadUserDataType)
    }

    pub fn metatable(self) -> Option<Table<'gc>> {
        self.0.metadata().get().metatable
    }

    pub fn set_metatable(
        self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        let md = self.0.write_metadata(mc).unlock();
        let mut v = md.get();
        let old_metatable = mem::replace(&mut v.metatable, metatable);
        md.set(v);
        old_metatable
    }
}
