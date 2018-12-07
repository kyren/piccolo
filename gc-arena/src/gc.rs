use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::util::{GcBox, Invariant};

/// A garbage collected pointer to a type T.  Implements Copy, and is implemented as a plain machine
/// pointer.  You can only allocate `Gc` pointers through an `Allocator` inside an arena type, and
/// through "generativity" such `Gc` pointers may not escape the arena they were born in or be
/// stored inside TLS.  This, combined with correct `Collect` implementations, means that `Gc`
/// pointers will never be dangling and are always safe to access.
pub struct Gc<'gc, T: 'gc + Collect> {
    pub(crate) ptr: NonNull<GcBox<T>>,
    _invariant: Invariant<'gc>,
}

impl<'gc, T: Collect + 'gc> Debug for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Gc").field("ptr", &self.ptr).finish()
    }
}

impl<'gc, T: Collect + 'gc> Copy for Gc<'gc, T> {}

impl<'gc, T: Collect + 'gc> Clone for Gc<'gc, T> {
    fn clone(&self) -> Gc<'gc, T> {
        *self
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for Gc<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        unsafe {
            cc.trace(self.ptr);
        }
    }
}

impl<'gc, T: Collect + 'gc> Deref for Gc<'gc, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr.as_ref().value.get() }
    }
}

impl<'gc, T: 'gc + Collect> Gc<'gc, T> {
    pub fn allocate(mc: MutationContext<'gc, '_>, t: T) -> Gc<'gc, T> {
        Gc {
            ptr: unsafe { mc.allocate(t) },
            _invariant: PhantomData,
        }
    }

    /// When implementing `Collect` on types with internal mutability containing `Gc` pointers, this
    /// method must be used to ensure safe mutability.  Safe to call, but only necessary from unsafe
    /// code.
    pub fn write_barrier(mc: MutationContext<'gc, '_>, gc: Self) {
        unsafe {
            mc.write_barrier(gc.ptr);
        }
    }
}
