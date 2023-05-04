use std::{
    cell::{Ref, RefMut},
    error::Error as StdError,
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{Collect, Gc, MutationContext, Rootable};

use crate::any::{self, AnyValue};

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

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct UserData<'gc>(pub(crate) Gc<'gc, dyn AnyValue<'gc>>);

impl<'gc> fmt::Debug for UserData<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("UserData")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for UserData<'gc> {
    fn eq(&self, other: &UserData<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for UserData<'gc> {}

impl<'gc> Hash for UserData<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc> UserData<'gc> {
    pub fn new<R>(mc: MutationContext<'gc, '_>, val: <R as Rootable<'gc>>::Root) -> Self
    where
        R: for<'a> Rootable<'a> + ?Sized + 'static,
    {
        UserData(any::new::<R>(mc, val))
    }

    pub fn read<'a, R>(&'a self) -> Result<Ref<'a, <R as Rootable<'gc>>::Root>, UserDataError>
    where
        R: for<'b> Rootable<'b> + ?Sized + 'static,
    {
        match any::read::<R>(&self.0) {
            Some(Ok(r)) => Ok(r),
            Some(Err(_)) => Err(UserDataError::BorrowError),
            None => Err(UserDataError::WrongType),
        }
    }

    pub fn write<'a, R>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, <R as Rootable<'gc>>::Root>, UserDataError>
    where
        R: for<'b> Rootable<'b> + ?Sized + 'static,
    {
        match any::write::<R>(mc, &self.0) {
            Some(Ok(r)) => Ok(r),
            Some(Err(_)) => Err(UserDataError::BorrowError),
            None => Err(UserDataError::WrongType),
        }
    }
}
