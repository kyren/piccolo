use std::cell::{Ref, RefCell, RefMut};

use collect::Collect;
use context::{CollectionContext, MutationContext};
use gc::Gc;

/// A garbage collected pointer to a type T that may be safely mutated.  When a type that may hold
/// `Gc` pointers is mutated, it may adopt new `Gc` pointers, and in order for this to be safe this
/// must be accompanied by a call to `Gc::write_barrier`.  This type wraps the given `T` in a
/// `RefCell` in such a way that writing to the `RefCell` is always accompanied by a call to
/// `Gc::write_barrier`.
pub struct GcCell<'gc, T: 'gc + Collect>(Gc<'gc, GcRefCell<T>>);

impl<'gc, T: Collect + 'gc> Copy for GcCell<'gc, T> {}

impl<'gc, T: Collect + 'gc> Clone for GcCell<'gc, T> {
    fn clone(&self) -> GcCell<'gc, T> {
        *self
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for GcCell<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        self.0.trace(cc)
    }
}

impl<'gc, T: 'gc + Collect> GcCell<'gc, T> {
    pub fn allocate(allocator: MutationContext<'gc>, t: T) -> GcCell<'gc, T> {
        GcCell(Gc::allocate(allocator, GcRefCell(RefCell::new(t))))
    }

    pub fn read(&self) -> Ref<T> {
        (*self.0).0.borrow()
    }

    pub fn write<'a>(&'a self, allocator: MutationContext<'gc>) -> RefMut<'a, T>
    where
        'gc: 'a,
    {
        Gc::write_barrier(allocator, self.0);
        (*self.0).0.borrow_mut()
    }
}

struct GcRefCell<T: Collect>(RefCell<T>);

unsafe impl<'gc, T: Collect + 'gc> Collect for GcRefCell<T> {
    fn trace(&self, cc: CollectionContext) {
        self.0.borrow().trace(cc);
    }
}
