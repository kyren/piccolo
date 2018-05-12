use std::cell::{Cell, RefCell, UnsafeCell};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{mem, usize, f64};

use arena::ArenaParameters;
use collect::Collect;
use util::{GcBox, GcColor, GcFlags, Invariant};

/// Handle value given by arena callbacks during construction / mutation.  Allows allocating new
/// `Gc` pointers and internally mutating values held by `Gc` pointers.
#[derive(Copy, Clone)]
pub struct MutationContext<'gc> {
    _invariant: Invariant<'gc>,
    context: &'gc Context,
}

impl<'gc> MutationContext<'gc> {
    pub(crate) fn allocate<T: 'gc + Collect>(&self, t: T) -> NonNull<GcBox<T>> {
        unsafe { self.context.allocate(t) }
    }

    pub(crate) fn write_barrier<T: 'gc + Collect>(&self, ptr: NonNull<GcBox<T>>) {
        unsafe { self.context.write_barrier(ptr) }
    }
}

/// Handle value given by arena callbacks during garbage collection, which must be passed through
/// `Collect::trace` implementations.
#[derive(Copy, Clone)]
pub struct CollectionContext<'gc> {
    context: &'gc Context,
}

impl<'gc> CollectionContext<'gc> {
    pub(crate) fn trace<T: 'gc + Collect>(&self, ptr: NonNull<GcBox<T>>) {
        unsafe { self.context.trace(ptr) }
    }
}

// Main gc context type, public because it must be accessible from the `make_arena!` macro.
#[doc(hidden)]
pub struct Context {
    parameters: ArenaParameters,

    phase: Cell<Phase>,
    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,

    all: Cell<Option<NonNull<GcBox<Collect>>>>,
    sweep: Cell<Option<NonNull<GcBox<Collect>>>>,
    sweep_prev: Cell<Option<NonNull<GcBox<Collect>>>>,

    gray: RefCell<Vec<NonNull<GcBox<Collect>>>>,
    gray_again: RefCell<Vec<NonNull<GcBox<Collect>>>>,
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            let mut all = self.all.get();
            while let Some(ptr) = all {
                let gc_box = ptr.as_ref();
                Box::from_raw(gc_box.value.get());
                all = gc_box.next.get();
            }
        }
    }
}

impl Context {
    pub unsafe fn new(parameters: ArenaParameters) -> Context {
        Context {
            parameters,
            phase: Cell::new(Phase::Wake),
            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(0),
            allocation_debt: Cell::new(0.0),
            all: Cell::new(None),
            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            gray: RefCell::new(Vec::new()),
            gray_again: RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub unsafe fn mutation_context<'gc>(&'gc self) -> MutationContext<'gc> {
        MutationContext {
            _invariant: PhantomData,
            context: self,
        }
    }

    #[inline]
    pub unsafe fn collection_context<'gc>(&'gc self) -> CollectionContext<'gc> {
        CollectionContext { context: self }
    }

    #[inline]
    pub fn allocation_debt(&self) -> f64 {
        self.allocation_debt.get()
    }

    // Do some collection work until we have either reached the target amount of work or are in the
    // sleeping gc phase.  The unit of "work" here is a byte count of objects either turned black or
    // freed, so to completely collect a heap with 1000 bytes of objects should take 1000 units of
    // work, whatever percentage of them are live or not.
    pub unsafe fn do_collection<R: Collect>(&self, root: &R, work: f64) {
        let mut work_left = work;
        let cc = self.collection_context();

        while work_left > 0.0 {
            match self.phase.get() {
                Phase::Wake => {
                    // In the Wake phase, we trace the root object and add its children to the gray
                    // queue, and transition to the propagate phase.
                    root.trace(cc);
                    work_left -= mem::size_of::<R>() as f64;
                    self.phase.set(Phase::Propagate);
                }
                Phase::Propagate => {
                    if let Some(ptr) = self.gray.borrow_mut().pop() {
                        // If we have objects in the normal gray queue, take one, trace it, and turn
                        // it black.
                        let gc_box = ptr.as_ref();
                        (*gc_box.value.get()).trace(cc);
                        gc_box.flags.set_color(GcColor::Black);
                        work_left -= mem::size_of_val(gc_box) as f64;
                    } else {
                        // If we have no objects left in the normal gray queue, we enter the atomic
                        // phase.
                        self.phase.set(Phase::Atomic);
                    }
                }
                Phase::Atomic => {
                    // During the atomic phase, the normal gray queue should be cleared but the
                    // 'gray again' queue may have values in it.  We need to process this queue, and
                    // any new gray values that enter the normal gray queue until both are empty.
                    loop {
                        let ptr = if let Some(ptr) = self.gray_again.borrow_mut().pop() {
                            ptr
                        } else if let Some(ptr) = self.gray.borrow_mut().pop() {
                            ptr
                        } else {
                            // If we have no gray values left, we can enter the sweep phase.
                            self.phase.set(Phase::Sweep);
                            break;
                        };

                        let gc_box = ptr.as_ref();
                        (*gc_box.value.get()).trace(cc);
                        gc_box.flags.set_color(GcColor::Black);
                    }
                }
                Phase::Sweep => {
                    if let Some(sweep_ptr) = self.sweep.get() {
                        let sweep = sweep_ptr.as_ref();
                        let sweep_size = mem::size_of_val(sweep);

                        let next_ptr = sweep.next.get();
                        self.sweep.set(next_ptr);

                        // If the next object in the sweep list is white, we need to remove it from
                        // the main list and destruct it, otherwise it should be black, and we
                        // simply turn it white again.
                        if sweep.flags.color() == GcColor::White {
                            // If the next object in the sweep portion of the main list is white, we
                            // need to remove it from the main object list and destruct it.
                            if let Some(sweep_prev) = self.sweep_prev.get() {
                                sweep_prev.as_ref().next.set(next_ptr);
                            } else {
                                // If `sweep_prev` is None, then the sweep pointer is also the
                                // beginning of the main object list, so we need to adjust it.
                                debug_assert_eq!(self.all.get(), Some(sweep_ptr));
                                self.all.set(next_ptr);
                            }
                            self.total_allocated
                                .set(self.total_allocated.get() - sweep_size);
                            work_left -= sweep_size as f64;
                            Box::from_raw(sweep_ptr.as_ptr());
                        } else {
                            // If the next object in the sweep portion of the main list is black, we
                            // need to keep it but turn it back white.  No gray objects should be in
                            // this part of the main list, they should be added to the beginning of
                            // the list before the sweep pointer, so it should not be possible for
                            // us to encounter them here.
                            debug_assert_eq!(sweep.flags.color(), GcColor::Black);
                            self.sweep_prev.set(Some(sweep_ptr));
                            self.remembered_size
                                .set(self.remembered_size.get() + sweep_size);
                            sweep.flags.set_color(GcColor::White);
                        }
                    } else {
                        // We are done sweeping, so enter the sleeping phase.
                        self.sweep_prev.set(None);
                        self.phase.set(Phase::Sleep);
                        self.wakeup_total.set(
                            self.total_allocated.get()
                                + ((self.remembered_size.get() as f64
                                    * self.parameters.pause_factor)
                                    .round()
                                    .min(usize::MAX as f64)
                                    as usize)
                                    .max(self.parameters.min_sleep),
                        );
                    }
                }
                Phase::Sleep => break,
            }
        }

        if self.phase.get() == Phase::Sleep {
            // Debt should not accumulate during sleep
            self.allocation_debt.set(0.0);
        } else {
            self.allocation_debt
                .set((self.allocation_debt.get() - work + work_left).max(0.0));
        }
    }

    unsafe fn allocate<T: Collect>(&self, t: T) -> NonNull<GcBox<T>> {
        let alloc_size = mem::size_of::<GcBox<T>>();
        self.total_allocated
            .set(self.total_allocated.get() + alloc_size);
        if self.phase.get() == Phase::Sleep {
            if self.total_allocated.get() > self.wakeup_total.get() {
                self.phase.set(Phase::Wake);
            }
        }

        if self.phase.get() != Phase::Sleep {
            self.allocation_debt.set(
                self.allocation_debt.get() + alloc_size as f64
                    + alloc_size as f64 / self.parameters.timing_factor,
            );
        }

        let gc_box = GcBox {
            flags: GcFlags::new(),
            next: Cell::new(self.all.get()),
            value: UnsafeCell::new(t),
        };
        gc_box.flags.set_needs_trace(T::needs_trace());
        let ptr = NonNull::new_unchecked(Box::into_raw(Box::new(gc_box)));
        self.all.set(Some(static_gc_box(ptr)));
        if self.phase.get() == Phase::Sweep {
            if self.sweep_prev.get().is_none() {
                self.sweep_prev.set(self.all.get());
            }
        }

        ptr
    }

    unsafe fn write_barrier<T: Collect>(&self, ptr: NonNull<GcBox<T>>) {
        // During the propagating phase, if we are mutating a black object, we may add a white
        // object to it and invalidate the black invariant.  Turn black obejcts to gray to prevent
        // this.
        let gc_box = ptr.as_ref();
        if self.phase.get() == Phase::Propagate && gc_box.flags.color() == GcColor::Black {
            gc_box.flags.set_color(GcColor::Gray);
            self.gray_again.borrow_mut().push(static_gc_box(ptr));
        }
    }

    unsafe fn trace<T: Collect>(&self, ptr: NonNull<GcBox<T>>) {
        let gc_box = ptr.as_ref();
        match gc_box.flags.color() {
            GcColor::Black | GcColor::Gray => {}
            GcColor::White => {
                if gc_box.flags.needs_trace() {
                    // A white traceable object is not in the gray queue, becomes gray and enters
                    // the normal gray queue.
                    gc_box.flags.set_color(GcColor::Gray);
                    self.gray.borrow_mut().push(static_gc_box(ptr));
                } else {
                    // A white object that doesn't need tracing simply becomes black.
                    gc_box.flags.set_color(GcColor::Black);
                }
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Phase {
    Wake,
    Propagate,
    Atomic,
    Sweep,
    Sleep,
}

unsafe fn static_gc_box<'gc, T: 'gc + Collect>(ptr: NonNull<GcBox<T>>) -> NonNull<GcBox<Collect>> {
    mem::transmute::<_, NonNull<GcBox<Collect>>>(ptr as NonNull<GcBox<Collect + 'gc>>)
}
