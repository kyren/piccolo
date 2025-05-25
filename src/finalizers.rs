use gc_arena::{lock::RefLock, Collect, Finalization, Gc, GcWeak, Mutation};

use crate::{thread::ThreadInner, Thread};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Finalizers<'gc>(Gc<'gc, RefLock<FinalizersState<'gc>>>);

impl<'gc> Finalizers<'gc> {
    const THREAD_ERR: &'static str = "thread finalization was missed";

    pub(crate) fn new(mc: &Mutation<'gc>) -> Self {
        Finalizers(Gc::new(mc, RefLock::default()))
    }

    pub(crate) fn register_thread(&self, mc: &Mutation<'gc>, ptr: Gc<'gc, ThreadInner<'gc>>) {
        self.0.borrow_mut(mc).threads.push(Gc::downgrade(ptr));
    }

    /// First stage of two-stage finalization.
    ///
    /// This stage can cause resurrection, so the arena must be *fully re-marked* before stage two
    /// (`Finalizers::finalize`).
    pub(crate) fn prepare(&self, fc: &Finalization<'gc>) {
        let state = self.0.borrow();
        for &ptr in &state.threads {
            let thread = Thread::from_inner(ptr.upgrade(fc).expect(Self::THREAD_ERR));
            thread.resurrect_live_upvalues(fc).unwrap();
        }
    }

    /// Second stage of two-stage finalization.
    ///
    /// Assuming stage one was called (`Finalizers::prepare`) and the arena fully re-marked, this
    /// method will *not* cause any resurrection.
    ///
    /// The arena must *immediately* transition to `CollectionPhase::Collecting` afterwards to not
    /// miss any finalizers.
    pub(crate) fn finalize(&self, fc: &Finalization<'gc>) {
        let mut state = self.0.borrow_mut(fc);
        state.threads.retain(|&ptr| {
            let ptr = ptr.upgrade(fc).expect(Self::THREAD_ERR);
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
