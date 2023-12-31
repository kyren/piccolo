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

    pub(crate) fn finalize(&self, fc: &Finalization<'gc>) {
        let mut state = self.0.borrow_mut(fc);
        state.threads.retain(|&ptr| {
            let ptr = ptr.upgrade(fc).expect("thread finalization was missed");
            if Gc::is_dead(fc, ptr) {
                Thread::from_inner(ptr).reset(fc).unwrap();
                false
            } else {
                true
            }
        });
    }
}

#[derive(Default, Collect)]
#[collect(no_drop)]
struct FinalizersState<'gc> {
    threads: Vec<GcWeak<'gc, ThreadInner<'gc>>>,
}
