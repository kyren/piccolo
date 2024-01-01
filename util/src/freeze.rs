use std::{cell::RefCell, marker::PhantomData, mem, rc::Rc};

use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Error)]
pub enum AccessError {
    #[error("frozen value accessed outside of enclosing scope")]
    Expired,
    #[error("already borrowed incompatibly")]
    BadBorrow,
}

/// Safely erase a lifetime from a value and temporarily store it in a shared handle.
///
/// Works by providing only limited access to the held value within an enclosing call to
/// `FrozenScope::scope`. All cloned handles will refer to the same underlying value with its
/// lifetime erased.
///
/// Useful for passing non-'static values into things that do not understand the Rust lifetime
/// system and need unrestricted sharing, such as scripting languages.
pub struct Frozen<F: for<'f> Freeze<'f>> {
    inner: Rc<RefCell<Option<<F as Freeze<'static>>::Frozen>>>,
}

pub trait Freeze<'f>: 'static {
    type Frozen: 'f;
}

pub struct DynFreeze<T: ?Sized>(PhantomData<T>);

impl<'f, T: ?Sized + for<'a> Freeze<'a>> Freeze<'f> for DynFreeze<T> {
    type Frozen = <T as Freeze<'f>>::Frozen;
}

#[macro_export]
#[doc(hidden)]
macro_rules! __scripting_Freeze {
    ($f:lifetime => $frozen:ty) => {
        $crate::freeze::DynFreeze::<
            dyn for<$f> $crate::freeze::Freeze<$f, Frozen = $frozen>,
        >
    };
    ($frozen:ty) => {
        $crate::freeze::Freeze!['freeze => $frozen]
    };
}

pub use crate::__scripting_Freeze as Freeze;

impl<F: for<'a> Freeze<'a>> Clone for Frozen<F> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<F: for<'a> Freeze<'a>> Default for Frozen<F> {
    fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(None)),
        }
    }
}

impl<F: for<'a> Freeze<'a>> Frozen<F> {
    /// Creates a new *invalid* `Frozen` handle.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn in_scope<'f, R>(value: <F as Freeze<'f>>::Frozen, cb: impl FnOnce(Self) -> R) -> R {
        let f = Self::new();
        let p = f.clone();
        FrozenScope::new().freeze(&f, value).scope(move || cb(p))
    }

    /// Returns true if this value is currently set by an enclosing `FrozenScope::scope`.
    pub fn is_valid(&self) -> bool {
        if let Ok(b) = self.inner.try_borrow() {
            b.is_some()
        } else {
            true
        }
    }

    pub fn try_with<R>(
        &self,
        f: impl for<'f> FnOnce(&<F as Freeze<'f>>::Frozen) -> R,
    ) -> Result<R, AccessError> {
        Ok(f(self
            .inner
            .try_borrow()
            .map_err(|_| AccessError::BadBorrow)?
            .as_ref()
            .ok_or(AccessError::Expired)?))
    }

    /// # Panics
    /// Panics if this handle is not currently valid or if the held value is already borrowed
    /// mutably.
    pub fn with<R>(&self, f: impl for<'f> FnOnce(&<F as Freeze<'f>>::Frozen) -> R) -> R {
        self.try_with(f).unwrap()
    }

    pub fn try_with_mut<R>(
        &self,
        f: impl for<'f> FnOnce(&mut <F as Freeze<'f>>::Frozen) -> R,
    ) -> Result<R, AccessError> {
        Ok(f(self
            .inner
            .try_borrow_mut()
            .map_err(|_| AccessError::BadBorrow)?
            .as_mut()
            .ok_or(AccessError::Expired)?))
    }

    /// # Panics
    /// Panics if this handle is not currently valid or if the held value is already borrowed.
    pub fn with_mut<R>(&self, f: impl for<'f> FnOnce(&mut <F as Freeze<'f>>::Frozen) -> R) -> R {
        self.try_with_mut(f).unwrap()
    }
}

/// Struct that enables setting the contents of multiple `Frozen<F>` handles for the body of a
/// single callback.
pub struct FrozenScope<D = ()>(D);

impl Default for FrozenScope<()> {
    fn default() -> Self {
        FrozenScope(())
    }
}

impl FrozenScope<()> {
    pub fn new() -> Self {
        Self(())
    }
}

impl<D: DropGuard> FrozenScope<D> {
    /// Sets the given frozen value for the duration of the `FrozenScope::scope` call.
    pub fn freeze<'h, 'f, F: for<'a> Freeze<'a>>(
        self,
        handle: &'h Frozen<F>,
        value: <F as Freeze<'f>>::Frozen,
    ) -> FrozenScope<(FreezeGuard<'h, 'f, F>, D)> {
        FrozenScope((
            FreezeGuard {
                value: Some(value),
                handle,
            },
            self.0,
        ))
    }

    /// Inside this call, all of the handles set with `FrozenScope::freeze` will be valid and can be
    /// accessed with `Frozen::with` and `Frozen::with_mut`. The provided handles (and all clones of
    /// them) are invalidated before this call to `FrozenScope::scope` returns.
    ///
    /// # Panics
    /// Panics if any of the provided handles are already set inside another, outer
    /// `FrozenScope::scope` call or if any handles were set with `FrozenScope::freeze` more than
    /// once. The given handles must be used with only one `FrozenScope` at a time.
    pub fn scope<R>(mut self, cb: impl FnOnce() -> R) -> R {
        // SAFETY: Safety depends on a few things...
        //
        // 1) We turn non-'static values into a 'static ones, outside code should never be able to
        //    observe the held 'static value, because it lies about the true lifetime.
        //
        // 2) The only way to interact with the held 'static value is through `Frozen::[try_]with`
        //    and `Frozen::[try_]with_mut`, both of which require a callback that works with the
        //    frozen type for *any* lifetime. This interaction is safe because the callbacks must
        //    work for any lifetime, so they must work with the lifetime we have erased.
        //
        // 3) The 'static `Frozen<F>` handles must have their values unset before the body of
        //    this function ends because we only know they live for at least the body of this
        //    function, and we use drop guards for this.
        unsafe {
            self.0.set();
        }
        let r = cb();
        drop(self.0);
        r
    }
}

pub trait DropGuard {
    // Sets the held `Frozen` handle to the held value.
    //
    // SAFETY:
    // This is unsafe because the `Frozen` handle can now be used to access the value independent of
    // its lifetime and the borrow checker cannot check this.
    //
    // Implementers of this trait *must* unset the handle's held value when the value is dropped.
    //
    // Users of this trait *must* drop it before the lifetime of the held value ends.
    unsafe fn set(&mut self);
}

impl DropGuard for () {
    unsafe fn set(&mut self) {}
}

impl<A: DropGuard, B: DropGuard> DropGuard for (A, B) {
    unsafe fn set(&mut self) {
        self.0.set();
        self.1.set();
    }
}

pub struct FreezeGuard<'h, 'f, F: for<'a> Freeze<'a>> {
    value: Option<<F as Freeze<'f>>::Frozen>,
    handle: &'h Frozen<F>,
}

impl<'h, 'f, F: for<'a> Freeze<'a>> Drop for FreezeGuard<'h, 'f, F> {
    fn drop(&mut self) {
        if let Ok(mut v) = self.handle.inner.try_borrow_mut() {
            *v = None;
        } else {
            // This should not be possible to trigger safely, because users cannot hold
            // `Ref` or `RefMut` handles from the inner `RefCell` in the first place,
            // and `Frozen` does not implement Send so we can't be in the body of
            // `Frozen::with[_mut]` in another thread. However, if it somehow happens that
            // we cannot drop the held value, this means that there is a live reference to
            // it somewhere so we are forced to abort the process.
            eprintln!("impossible! freeze lock held during drop guard, aborting!");
            std::process::abort()
        }
    }
}

impl<'h, 'f, F: for<'a> Freeze<'a>> DropGuard for FreezeGuard<'h, 'f, F> {
    unsafe fn set(&mut self) {
        assert!(
            !self.handle.is_valid(),
            "handle already used in another `FrozenScope::scope` call"
        );
        *self.handle.inner.borrow_mut() = Some(mem::transmute::<
            <F as Freeze<'f>>::Frozen,
            <F as Freeze<'static>>::Frozen,
        >(self.value.take().unwrap()));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_freeze_works() {
        struct F<'a>(&'a i32);

        let i = 4;
        Frozen::<Freeze![F<'freeze>]>::in_scope(F(&i), |f| {
            f.with(|f| {
                assert_eq!(*f.0, 4);
            });
        });
    }

    #[test]
    fn test_freeze_expires() {
        struct F<'a>(&'a i32);

        type FrozenF = Frozen<Freeze![F<'freeze>]>;

        let mut outer: Option<FrozenF> = None;

        let i = 4;
        FrozenF::in_scope(F(&i), |f| {
            outer = Some(f.clone());
        });

        assert_eq!(
            outer.unwrap().try_with(|f| {
                assert_eq!(*f.0, 4);
            }),
            Err(AccessError::Expired)
        );
    }
}
