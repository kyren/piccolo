use std::cell::{Cell, RefCell, UnsafeCell};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{f64, mem, usize};

use arena::ArenaParameters;
use collect::Collect;
use util::{GcBox, GcColor, GcFlags, Invariant};

/// Handle value given by arena callbacks during construction and mutation.  Allows allocating new
/// `Gc` pointers and internally mutating values held by `Gc` pointers.
#[derive(Copy, Clone)]
pub struct MutationContext<'gc> {
    _invariant: Invariant<'gc>,
    context: &'gc Context,
}

impl<'gc> MutationContext<'gc> {
    pub(crate) unsafe fn allocate<T: 'gc + Collect>(&self, t: T) -> NonNull<GcBox<T>> {
        self.context.allocate(t)
    }

    pub(crate) unsafe fn write_barrier<T: 'gc + Collect>(&self, ptr: NonNull<GcBox<T>>) {
        self.context.write_barrier(ptr)
    }
}

/// Handle value given by arena callbacks during garbage collection, which must be passed through
/// `Collect::trace` implementations.
#[derive(Copy, Clone)]
pub struct CollectionContext<'gc> {
    context: &'gc Context,
}

impl<'gc> CollectionContext<'gc> {
    pub(crate) unsafe fn trace<T: 'gc + Collect>(&self, ptr: NonNull<GcBox<T>>) {
        self.context.trace(ptr)
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

    // If the garbage collector is currently in the sleep phase, transition to the wake phase.
    pub fn wake(&self) {
        if self.phase.get() == Phase::Sleep {
            self.phase.set(Phase::Wake);
        }
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
                    // We look for an object first in the normal gray queue, then the "gray again"
                    // queue.  Objects from the normal gray queue count as regular work, but objects
                    // which are gray a second time have already been counted as work, so we don't
                    // double count them.  Processing "gray again" objects later also gives them
                    // more time to be mutated again without triggering another write barrier.
                    let next_gray = if let Some(ptr) = self.gray.borrow_mut().pop() {
                        work_left -= mem::size_of_val(ptr.as_ref()) as f64;
                        Some(ptr)
                    } else if let Some(ptr) = self.gray_again.borrow_mut().pop() {
                        Some(ptr)
                    } else {
                        None
                    };

                    if let Some(ptr) = next_gray {
                        // If we have an object in the gray queue, take one, trace it, and turn it
                        // black.
                        let gc_box = ptr.as_ref();
                        (*gc_box.value.get()).trace(cc);
                        gc_box.flags.set_color(GcColor::Black);
                    } else {
                        // If we have no objects left in the normal gray queue, we enter the sleep
                        // phase.
                        self.phase.set(Phase::Sweep);
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
            // Do not let debt accumulate across cycles, when we enter sleep, zero the debt out.
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
                self.allocation_debt.get()
                    + alloc_size as f64
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
    Sweep,
    Sleep,
}

unsafe fn static_gc_box<'gc, T: 'gc + Collect>(ptr: NonNull<GcBox<T>>) -> NonNull<GcBox<Collect>> {
    mem::transmute::<_, NonNull<GcBox<Collect>>>(ptr as NonNull<GcBox<Collect + 'gc>>)
}
