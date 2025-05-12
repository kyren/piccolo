use alloc::rc::Rc;
use core::{cell::RefCell, marker::PhantomData, mem};

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
/// `FreezeGuard::scope`. All cloned handles will refer to the same underlying value with its
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

    /// Shorthand for creating a new handle and using a `FreezeGuard`, sets the value for a new
    /// handle for the body of the given callback.
    pub fn in_scope<'f, R>(value: <F as Freeze<'f>>::Frozen, cb: impl FnOnce(Self) -> R) -> R {
        let f = Self::new();
        let p = f.clone();
        FreezeGuard::new(&f, value).scope(move || cb(p))
    }

    /// Returns true if this value is currently set by an enclosing `FreezeGuard::scope`.
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

/// Sets a `Frozen` handle to an actual value, allowing it to be accessed within a limited scope.
pub struct FreezeGuard<'h, 'f, F: for<'a> Freeze<'a>> {
    value: Option<<F as Freeze<'f>>::Frozen>,
    handle: &'h Frozen<F>,
}

impl<'h, 'f, F: for<'a> Freeze<'a>> FreezeGuard<'h, 'f, F> {
    /// Create a new `FreezeGuard` from a `Frozen` handle and the given value.
    ///
    /// This does *not* make the `Frozen` handle valid immediately. The value will only be
    /// accessible from the `Frozen` handle within a call to `FreezeGuard::scope`.
    pub fn new(handle: &'h Frozen<F>, value: <F as Freeze<'f>>::Frozen) -> Self {
        Self {
            value: Some(value),
            handle,
        }
    }

    /// Make the `Frozen` handle valid for the body of this callback.
    ///
    /// We must limit the time in which the handle is actually valid because we cannot rely
    /// on `FreezeGuard` being dropped by the user before the lifetime of the held value is
    /// invalidated. If we did this, then the handle could be forgotten with e.g. `mem::forget`,
    /// and the handle could be valid outside of the value's proper lifetime, which could lead to
    /// UB.
    ///
    /// Instead, by limiting the validity to only the body of this callback, we can prove that the
    /// handle is only valid within the value's allowed lifetime.
    pub fn scope<R>(&mut self, cb: impl FnOnce() -> R) -> R {
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
        let _guard = unsafe { DropGuard::new(self) };
        cb()
    }

    /// Consume this `FreezeGuard`, taking the held value out.
    pub fn into_value(mut self) -> <F as Freeze<'f>>::Frozen {
        self.value
            .take()
            .expect("`FreezeGuard::into_value()` called when set")
    }
}

impl<'h, 'f, F: for<'a> Freeze<'a>> ScopeGuard for FreezeGuard<'h, 'f, F> {
    unsafe fn set(&mut self) {
        assert!(
            !self.handle.is_valid(),
            "handle already used in another `FreezeGuard::scope` call"
        );
        *self.handle.inner.borrow_mut() = Some(mem::transmute::<
            <F as Freeze<'f>>::Frozen,
            <F as Freeze<'static>>::Frozen,
        >(self.value.take().unwrap()));
    }

    fn unset(&mut self) {
        if let Ok(mut v) = self.handle.inner.try_borrow_mut() {
            unsafe {
                self.value = Some(mem::transmute::<
                    <F as Freeze<'static>>::Frozen,
                    <F as Freeze<'f>>::Frozen,
                >(
                    v.take()
                        .expect("`FreezeGuard::unset` called without being set first"),
                ));
            }
        } else {
            // If the value is locked, then there is a live reference to it somewhere in the body of
            // a `Fozen::with[_mut]` call. We can no longer guarantee our invariants and are forced
            // to abort.
            //
            // This is impossible to trigger safely without calling the private `set` / `unset`
            // methods manually.

            struct PanicOnDrop;
            impl Drop for PanicOnDrop {
                fn drop(&mut self) {
                    panic!("intentionally double panicking to abort.")
                }
            }

            #[allow(unused)]
            let guard = PanicOnDrop;

            panic!("freeze lock held during `FreezeGuard::unset`, aborting!");

            #[allow(unreachable_code)]
            drop(guard);
        }
    }
}

#[allow(private_bounds)]
pub trait FrozenScopeGuard: ScopeGuard {}

impl<T: ScopeGuard> FrozenScopeGuard for T {}

/// Struct that enables setting the contents of *multiple* `Frozen` handles for the body of a single
/// callback.
///
/// This is a simpler way to set multiple `Frozen` handles than dealing with multiple `FreezeGuard`s
/// and nested multiple calls to `FreezeGuard::scope`, but it behaves identically otherwise.
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

impl<D: FrozenScopeGuard> FrozenScope<D> {
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
    ///
    /// Panics if any of the provided handles are already set inside another, outer
    /// `FrozenScope::scope` call or if any handles were set with `FrozenScope::freeze` more than
    /// once. The given handles must be used with only one `FrozenScope` at a time.
    pub fn scope<R>(&mut self, cb: impl FnOnce() -> R) -> R {
        let _guard = unsafe { DropGuard::new(&mut self.0) };
        cb()
    }
}

trait ScopeGuard {
    /// Sets the held `Frozen` handle to the held value.
    ///
    /// # Panics
    ///
    /// Panics if called multiple times without `Scope::unset` being called in-between.
    ///
    /// # SAFETY
    ///
    /// This is unsafe because the `Frozen` handle can now be used to access the value independent
    /// of its lifetime and the borrow checker cannot check this.
    ///
    /// Users of this trait *must* call `Scope::unset` to unset it *before* the lifetime of the
    /// held value ends.
    unsafe fn set(&mut self);

    /// Unsets the value from the held `Frozen` handle.
    ///
    /// The value may be set again with `Scope::set`.
    ///
    /// # Panics
    ///
    /// Panics if called without `Scope::set` being called first.
    ///
    /// # Aborts
    ///
    /// Will *abort* the process if `Scope::unset` is called within the body of `Freeze::with` (or
    /// similar).
    fn unset(&mut self);
}

impl ScopeGuard for () {
    unsafe fn set(&mut self) {}
    fn unset(&mut self) {}
}

impl<A: ScopeGuard, B: ScopeGuard> ScopeGuard for (A, B) {
    unsafe fn set(&mut self) {
        self.0.set();
        self.1.set();
    }

    fn unset(&mut self) {
        self.0.unset();
        self.1.unset();
    }
}

struct DropGuard<'a, S: ScopeGuard>(&'a mut S);

impl<'a, S: ScopeGuard> DropGuard<'a, S> {
    unsafe fn new(s: &'a mut S) -> Self {
        unsafe {
            s.set();
        }
        Self(s)
    }
}

impl<'a, S: ScopeGuard> Drop for DropGuard<'a, S> {
    fn drop(&mut self) {
        self.0.unset();
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

    #[test]
    fn scope_works() {
        type FrozenI32 = Frozen<Freeze![&'freeze i32]>;

        let i = 4;
        let j = 5;

        let fi = FrozenI32::new();
        let fj = FrozenI32::new();

        let mut fscope = FrozenScope::new().freeze(&fi, &i).freeze(&fj, &j);

        fscope.scope(|| {
            fi.with(|f| assert_eq!(**f, 4));
            fj.with(|f| assert_eq!(**f, 5));
        });

        fscope.scope(|| {
            fi.with(|f| assert_eq!(**f, 4));
            fj.with(|f| assert_eq!(**f, 5));
        });
    }
}
