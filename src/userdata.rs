use std::{
    cell::{Ref, RefMut},
    hash::{Hash, Hasher},
    mem,
};

use gc_arena::{Collect, Mutation, Root, Rootable};
use thiserror::Error;

use crate::{any::AnyCell, Table};

#[derive(Debug, Copy, Clone, Collect, Error)]
#[collect(require_static)]
pub enum UserDataError {
    #[error("UserData type mismatch")]
    WrongType,
    #[error("UserData already incompatibly borrowed")]
    BorrowError,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct AnyUserData<'gc>(AnyCell<'gc, Option<Table<'gc>>>);

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

impl<'gc> AnyUserData<'gc> {
    pub fn new<R>(mc: &Mutation<'gc>, val: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a>,
    {
        AnyUserData(AnyCell::new::<R>(mc, None, val))
    }

    pub fn is<R>(&self) -> bool
    where
        R: for<'a> Rootable<'a>,
    {
        self.0.is::<R>()
    }

    pub fn read<'a, R>(&'a self) -> Result<Ref<'a, Root<'gc, R>>, UserDataError>
    where
        R: for<'b> Rootable<'b>,
    {
        match self.0.read_data::<R>() {
            Some(Ok(r)) => Ok(r),
            Some(Err(_)) => Err(UserDataError::BorrowError),
            None => Err(UserDataError::WrongType),
        }
    }

    pub fn write<'a, R>(
        &'a self,
        mc: &Mutation<'gc>,
    ) -> Result<RefMut<'a, Root<'gc, R>>, UserDataError>
    where
        R: for<'b> Rootable<'b>,
    {
        match self.0.write_data::<R>(mc) {
            Some(Ok(r)) => Ok(r),
            Some(Err(_)) => Err(UserDataError::BorrowError),
            None => Err(UserDataError::WrongType),
        }
    }

    pub fn metatable(&self) -> Option<Table<'gc>> {
        *self.0.read_metadata().unwrap()
    }

    pub fn set_metatable(
        &self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        mem::replace(&mut self.0.write_metadata(mc).unwrap(), metatable)
    }

    pub fn as_ptr(&self) -> *const () {
        self.0.as_ptr()
    }
}
