use gc_arena::{barrier::Write, lock::RefLock, Collect, Gc, GcWeak, Mutation};

pub trait Finalize<'gc> {
    fn finalize(&self, mc: &Mutation<'gc>);

    fn needs_write_barrier(&self) -> bool {
        false
    }
}

pub trait FinalizeWrite<'gc> {
    fn finalize_write(this: &Write<Self>, mc: &Mutation<'gc>);
}

impl<'gc, T: FinalizeWrite<'gc>> Finalize<'gc> for T {
    fn finalize(&self, mc: &Mutation<'gc>) {
        // SAFETY: The write barrier has been called because `Self::needs_write_barrier()` returns
        // true.
        Self::finalize_write(unsafe { Write::assume(self) }, mc);
    }

    fn needs_write_barrier(&self) -> bool {
        true
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Finalizers<'gc>(Gc<'gc, RefLock<Vec<GcWeak<'gc, dyn Finalize<'gc> + 'gc>>>>);

impl<'gc> Finalizers<'gc> {
    pub(crate) fn new(mc: &Mutation<'gc>) -> Self {
        Finalizers(Gc::new(mc, RefLock::new(Vec::new())))
    }

    pub fn add(&self, mc: &Mutation<'gc>, ptr: GcWeak<'gc, dyn Finalize<'gc> + 'gc>) {
        self.0.borrow_mut(mc).push(ptr);
    }

    pub(crate) fn finalize(&self, mc: &Mutation<'gc>) {
        self.0.borrow_mut(mc).retain(|&w| {
            if let Some(s) = w.upgrade(mc) {
                if !w.is_marked() {
                    if s.needs_write_barrier() {
                        Gc::write(mc, s);
                    }
                    s.finalize(mc);
                    false
                } else {
                    true
                }
            } else {
                panic!("finalization was missed");
            }
        });
    }
}
