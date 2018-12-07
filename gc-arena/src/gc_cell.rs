use std::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::gc::Gc;

/// A garbage collected pointer to a type T that may be safely mutated.  When a type that may hold
/// `Gc` pointers is mutated, it may adopt new `Gc` pointers, and in order for this to be safe this
/// must be accompanied by a call to `Gc::write_barrier`.  This type wraps the given `T` in a
/// `RefCell` in such a way that writing to the `RefCell` is always accompanied by a call to
/// `Gc::write_barrier`.
#[derive(Debug)]
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
    pub fn allocate(mc: MutationContext<'gc, '_>, t: T) -> GcCell<'gc, T> {
        GcCell(Gc::allocate(
            mc,
            GcRefCell {
                cell: RefCell::new(t),
            },
        ))
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.cell.as_ptr()
    }

    pub fn read(&self) -> Ref<T> {
        self.0.cell.borrow()
    }

    pub fn try_read<'a>(&'a self) -> Result<Ref<'a, T>, BorrowError> {
        self.0.cell.try_borrow()
    }

    pub fn write<'a>(&'a self, mc: MutationContext<'gc, '_>) -> RefMut<'a, T>
    where
        'gc: 'a,
    {
        Gc::write_barrier(mc, self.0);
        self.0.cell.borrow_mut()
    }

    pub fn try_write<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, T>, BorrowMutError> {
        let mb = self.0.cell.try_borrow_mut()?;
        Gc::write_barrier(mc, self.0);
        Ok(mb)
    }
}

#[derive(Debug)]
struct GcRefCell<T: Collect> {
    cell: RefCell<T>,
}

unsafe impl<'gc, T: Collect + 'gc> Collect for GcRefCell<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.borrow().trace(cc);
    }
}
