use gc_arena::{lock::RefLock, Collect, Finalization, Gc, GcWeak, Mutation};

use crate::{thread::ThreadInner, Thread};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Finalizers<'gc>(Gc<'gc, RefLock<FinalizersState<'gc>>>);

impl<'gc> Finalizers<'gc> {
    pub(crate) fn new(mc: &Mutation<'gc>) -> Self {
        Finalizers(Gc::new(mc, RefLock::default()))
    }

    pub(crate) fn register_thread(&self, mc: &Mutation<'gc>, ptr: Gc<'gc, ThreadInner<'gc>>) {
        self.0.borrow_mut(mc).threads.push(Gc::downgrade(ptr));
    }

    // Returns `true` when some finalization work has been performed, so more may need to be done in
    // another iteration.
    pub(crate) fn finalize(&self, fc: &Finalization<'gc>) -> bool {
        let mut dropped = false;
        let mut state = self.0.borrow_mut(fc);
        state.threads.retain(|&ptr| {
            let ptr = ptr.upgrade(fc).expect("thread finalization was missed");
            if Gc::is_dead(fc, ptr) {
                Thread::from_inner(ptr).reset(fc).unwrap();
                dropped = true;
                false
            } else {
                true
            }
        });
        dropped
    }
}

#[derive(Default, Collect)]
#[collect(no_drop)]
struct FinalizersState<'gc> {
    threads: Vec<GcWeak<'gc, ThreadInner<'gc>>>,
}
