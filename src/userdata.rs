use std::{
    cell::{Ref, RefMut},
    error::Error as StdError,
    fmt,
    hash::{Hash, Hasher},
    mem,
};

use gc_arena::{Collect, Mutation, Root, Rootable};

use crate::{any::AnyCell, Table};

#[derive(Debug, Copy, Clone)]
pub enum UserDataError {
    WrongType,
    BorrowError,
}

impl StdError for UserDataError {}

impl fmt::Display for UserDataError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UserDataError::WrongType => write!(fmt, "UserData type mismatch"),
            UserDataError::BorrowError => write!(fmt, "UserData already incompatibly borrowed"),
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct AnyUserData<'gc>(pub AnyCell<'gc, UserMetadata<'gc>>);

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
        Root<'gc, R>: Sized,
    {
        AnyUserData(AnyCell::<UserMetadata<'gc>>::new::<R>(
            mc,
            UserMetadata { metatable: None },
            val,
        ))
    }

    pub fn read<'a, R>(&'a self) -> Result<Ref<'a, Root<'gc, R>>, UserDataError>
    where
        R: for<'b> Rootable<'b>,
        Root<'gc, R>: Sized,
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
        Root<'gc, R>: Sized,
    {
        match self.0.write_data::<R>(mc) {
            Some(Ok(r)) => Ok(r),
            Some(Err(_)) => Err(UserDataError::BorrowError),
            None => Err(UserDataError::WrongType),
        }
    }

    pub fn metatable(&self) -> Option<Table<'gc>> {
        self.0.read_metadata().unwrap().metatable
    }

    pub fn set_metatable(
        &self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        mem::replace(&mut self.0.write_metadata(mc).unwrap().metatable, metatable)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct UserMetadata<'gc> {
    pub metatable: Option<Table<'gc>>,
}
