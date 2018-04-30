#![allow(unused)]

use std::cell::{Cell, UnsafeCell};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{mem, usize};

/// GcObject impls must not access Gc pointers during their Drop impls, as they may be dangling, and
/// also must not change their held Gc pointers without writing through GcContext, so that any
/// necessary write barrier is executed.  Tracing of GcObject instances will not occur until the
/// instance is itself allocated into a Gc / Rgc pointer, so allocated Gc pointers must immediately
/// (without interleaving allocations) be placed into a GcObject which is itself also held by a Gc /
/// Rgc pointer.
pub unsafe trait GcObject {
    /// The `trace` method MUST trace through all owned Gc pointers, and every owned Gc pointer must
    /// be created from the same GcContext that holds this GcObject.
    unsafe fn trace(&self, context: &GcContext) {}
}

pub struct GcContext {
    // The garbage collector will wait until the live size reaches <previous live size> *
    // pause_multiplier before beginning a new collection.  Must be >= 1.0, setting this to 1.0
    // causes the collector to run continuously.
    pause_multiplier: f64,
    // The garbage collector will try and finish a collection by the time <current live size> *
    // timing_multiplier additional bytes are allocated.  For example, if the collection is started
    // when the GcContext has 100KB live data, and the timing_multiplier is 1.0, the collector
    // should finish its final phase of this collection when another 100KB has been allocated.  Must
    // be >= 0.0, setting this to 0.0 causes the collector to behave like a stop-the-world mark and
    // sweep.
    timing_multiplier: f64,

    total_allocated: Cell<usize>,
    phase: Cell<GcPhase>,
    current_white: Cell<GcColor>,

    // All garbage collected objects are kept in the all list
    all: Cell<Option<NonNull<GcBox<GcObject>>>>,

    // All light-gray or dark-gray objects must be held in the gray queue, and the gray queue will
    // hold no white or black objects.  The gray queue is split into two parts, the primary part
    // primarly holds objects that enter the queue by becoming dark-gray and are processed with
    // first priority.  The secondary part holds objects that enter the queue by becoming rooted,
    // and are processed secondarily, to give rooted objects the time to become un-rooted before
    // processing.  Also, if an object is taken from the primary queue but is mutably locked, it is
    // moved to the secondary queue to be processed later.  If the primary queue is empty, the
    // primary and secondary queue are swapped so that the secondary queue can be re-scanned.
    primary_gray: Cell<Option<NonNull<GcBox<GcObject>>>>,
    secondary_gray: Cell<Option<NonNull<GcBox<GcObject>>>>,

    // During the sweep phase, the `all` list is scanned.  Black objects are turned white and white
    // objects are freed.
    sweep_position: Cell<Option<NonNull<NonNull<GcBox<GcObject>>>>>,
}

impl GcContext {
    pub fn new(pause_multiplier: f64, timing_multiplier: f64) -> GcContext {
        assert!(pause_multiplier >= 1.0);
        assert!(timing_multiplier >= 0.0);

        GcContext {
            pause_multiplier,
            timing_multiplier,
            total_allocated: Cell::new(0),
            phase: Cell::new(GcPhase::Sleeping),
            current_white: Cell::new(GcColor::White1),
            all: Cell::new(None),
            primary_gray: Cell::new(None),
            secondary_gray: Cell::new(None),
            sweep_position: Cell::new(None),
        }
    }

    /// Move a value of type T into a Gc<T>.  Any further allocation may trigger a collection, so in
    /// general you must place this Gc into a GcObject before allocating again so that it does not
    /// become dangling.
    pub fn allocate<T: GcObject + 'static>(&self, value: T) -> Gc<T> {
        self.do_work();

        let gc_box = GcBox {
            header: GcBoxHeader {
                flags: GcFlags::new(self.current_white.get()),
                root_count: RootCount::new(),
                borrow_state: BorrowState::new(),
                next_all: Cell::new(None),
                next_gray: Cell::new(None),
            },
            data: UnsafeCell::new(value),
        };
        gc_box.header.next_all.set(self.all.get());

        let gc_box = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(gc_box))) };
        self.all.set(Some(gc_box));

        self.total_allocated.set(
            self.total_allocated
                .get()
                .checked_add(mem::size_of::<GcBox<T>>())
                .unwrap(),
        );

        Gc {
            gc_box,
            marker: PhantomData,
        }
    }

    /// Safely allocate a rooted Rgc<T>.
    pub fn allocate_root<T: GcObject + 'static>(&self, value: T) -> Rgc<T> {
        unsafe { self.root(self.allocate(value)) }
    }

    /// Root this Gc pointer, unsafe if this Gc pointer is dangling, and it must be created from the
    /// same GcContext.
    pub unsafe fn root<T: GcObject + 'static>(&self, gc: Gc<T>) -> Rgc<T> {
        {
            let header = &gc.gc_box.as_ref().header;
            if header.flags.color() == self.current_white.get() {
                // If our object is white, rooting it should turn it light-gray and place it into the
                // secondary gray queue.
                header.flags.set_color(GcColor::LightGray);
                header.next_gray.set(self.secondary_gray.get());
                self.secondary_gray.set(Some(gc.gc_box))
            }
            header.root_count.increment();
        }

        Rgc(gc)
    }

    /// Write to the contents of this Gc / Rgc.  Unsafe, because the contents must not be collected
    /// while the `GcWrite` is live, and the Gc / Rgc MUST be created from the same GcContext.
    /// Panics if the contents are already read or written on this or another cloned Gc.  Writing to
    /// a Gc will potentially execute a write barrier on the object, and will block tracing of the
    /// object for the lifetime of the `GcWrite`.
    pub unsafe fn write<'a, T: GcObject + 'static, R: AsRef<Gc<T>>>(
        &self,
        gc: &'a R,
    ) -> GcWrite<'a, T> {
        let gc = gc.as_ref();
        let gc_box = gc.gc_box.as_ref();
        // During the propagate phase, if we are writing to a black object, we need it to become
        // dark gray to uphold the black invariant.
        if self.phase.get() == GcPhase::Propagate && gc_box.header.flags.color() == GcColor::Black {
            gc_box.header.flags.set_color(GcColor::DarkGray);
            gc_box.header.next_gray.set(self.primary_gray.get());
            self.primary_gray.set(Some(gc.gc_box));
        }

        let borrow_state = &gc_box.header.borrow_state;
        assert!(
            borrow_state.mode() == BorrowMode::Unused,
            "cannot borrow Gc for writing, already borrowed"
        );
        borrow_state.set_writing();
        let value = &mut *gc_box.data.get();
        GcWrite {
            borrow_state,
            value,
        }
    }

    /// Only call inside `GcObject::trace`, and only on Gcs created from this GcContext.
    pub unsafe fn trace<T: GcObject + 'static>(&self, gc: &Gc<T>) {
        let gc_box = gc.gc_box.as_ref();
        match gc_box.header.flags.color() {
            GcColor::Black | GcColor::DarkGray => {}
            GcColor::LightGray => {
                // A light-gray object is already in the gray queue, just turn it dark gray.
                gc_box.header.flags.set_color(GcColor::DarkGray);
            }
            white @ GcColor::White1 | white @ GcColor::White2 => {
                // A white object is not in the gray queue, and dark-gray objects enter in the
                // primary queue.
                debug_assert_eq!(white, self.current_white.get());
                gc_box.header.flags.set_color(GcColor::DarkGray);
                gc_box.header.next_gray.set(self.primary_gray.get());
                self.primary_gray.set(Some(gc.gc_box));
            }
        }
    }
}

impl Default for GcContext {
    fn default() -> GcContext {
        const PAUSE_MULTIPLIER: f64 = 1.5;
        const TIMING_MULTIPLIER: f64 = 1.0;

        GcContext::new(PAUSE_MULTIPLIER, TIMING_MULTIPLIER)
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Gc<T: GcObject + ?Sized + 'static> {
    gc_box: NonNull<GcBox<T>>,
    marker: PhantomData<Rc<T>>,
}

impl<T: GcObject + ?Sized + 'static> Copy for Gc<T> {}

impl<T: GcObject + ?Sized + 'static> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        *self
    }
}

impl<T: GcObject + ?Sized + 'static> Gc<T> {
    /// Read the contents of this Gc.  Unsafe, because the contents must not be collected while the
    /// `GcRead` is live.  Panics if the contents are also being written on this or another cloned
    /// Gc.
    pub unsafe fn read(&self) -> GcRead<T> {
        let gc_box = self.gc_box.as_ref();
        let borrow_state = &gc_box.header.borrow_state;
        assert!(
            borrow_state.mode() != BorrowMode::Writing,
            "cannot borrow Gc for reading, already borrowed for writing"
        );
        borrow_state.inc_reading();
        let value = &*gc_box.data.get();
        GcRead {
            borrow_state,
            value,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Rgc<T: GcObject + ?Sized + 'static>(Gc<T>);

impl<T: GcObject + ?Sized + 'static> Clone for Rgc<T> {
    fn clone(&self) -> Rgc<T> {
        unsafe {
            let header = &self.0.gc_box.as_ref().header;
            header.root_count.increment();
            Rgc(self.0.clone())
        }
    }
}

impl<T: GcObject + ?Sized + 'static> Drop for Rgc<T> {
    fn drop(&mut self) {
        unsafe {
            self.0.gc_box.as_ref().header.root_count.decrement();
        }
    }
}

impl<T: GcObject + ?Sized + 'static> AsRef<Gc<T>> for Rgc<T> {
    fn as_ref(&self) -> &Gc<T> {
        &self.0
    }
}

impl<T: GcObject + ?Sized + 'static> Rgc<T> {
    pub fn unroot(&self) -> Gc<T> {
        self.0
    }

    /// Safe version of `Gc::read`, as we know that the pointer cannot be collected.
    pub fn read(&self) -> GcRead<T> {
        unsafe { self.0.read() }
    }
}

pub struct GcRead<'a, T: GcObject + ?Sized + 'static> {
    borrow_state: &'a BorrowState,
    value: &'a T,
}

impl<'a, T: GcObject + ?Sized> Deref for GcRead<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'a, T: GcObject + ?Sized> Drop for GcRead<'a, T> {
    fn drop(&mut self) {
        self.borrow_state.dec_reading();
    }
}

pub struct GcWrite<'a, T: GcObject + ?Sized + 'static> {
    borrow_state: &'a BorrowState,
    value: &'a mut T,
}

impl<'a, T: GcObject + ?Sized> Deref for GcWrite<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'a, T: GcObject + ?Sized> DerefMut for GcWrite<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.value
    }
}

impl<'a, T: GcObject + ?Sized> Drop for GcWrite<'a, T> {
    fn drop(&mut self) {
        self.borrow_state.set_unused();
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum GcPhase {
    Sleeping,
    Propagate,
    Atomic,
    Sweeping,
}

impl GcContext {
    fn do_work(&self) {
        // Unimplemented
    }
}

struct GcBox<T: GcObject + ?Sized + 'static> {
    header: GcBoxHeader,
    data: UnsafeCell<T>,
}

struct GcBoxHeader {
    flags: GcFlags,
    root_count: RootCount,
    borrow_state: BorrowState,
    next_all: Cell<Option<NonNull<GcBox<GcObject>>>>,
    next_gray: Cell<Option<NonNull<GcBox<GcObject>>>>,
}

struct GcFlags(Cell<u8>);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum GcColor {
    // White objects are unmarked and un-rooted.  At the end of the mark phase, all white objects
    // are unused and may be freed in the sweep phase.  The white color is swapped during sweeping
    // to distinguish between newly created white objects and unreachable white objects.
    White1,
    White2,
    // When a white object is rooted, it becomes light-gray and placed in the gray queue.  When it
    // is processed in the gray queue, if it is still rooted at the time of processing, its
    // sub-objects are traced and it becomes black.  If it is not rooted at the time of processing
    // it is turned white.
    LightGray,
    // When a white or light gray object is traced, it becomes dark-gray.  When a dark-gray object
    // is processed, its sub-objects are traced and it becomes black.
    DarkGray,
    // Black objects have been marked and their sub-objects must all be dark-gray or black.  If a
    // white or light-gray object is made a child of a black object (and the black invariant is
    // currently being held), a write barrier must be executed that either turns that child
    // dark-gray (forward) or turns the black object back dark-gray (back).
    Black,
}

impl GcFlags {
    fn new(color: GcColor) -> GcFlags {
        let flags = GcFlags(Cell::new(0));
        flags.set_color(color);
        flags
    }

    fn color(&self) -> GcColor {
        match self.0.get() & 0x7 {
            0x0 => GcColor::White1,
            0x1 => GcColor::White2,
            0x2 => GcColor::LightGray,
            0x3 => GcColor::DarkGray,
            0x4 => GcColor::Black,
            _ => unreachable!(),
        }
    }

    fn set_color(&self, color: GcColor) {
        self.0.set(
            (self.0.get() & !0x7) | match color {
                GcColor::White1 => 0x0,
                GcColor::White2 => 0x1,
                GcColor::LightGray => 0x2,
                GcColor::DarkGray => 0x3,
                GcColor::Black => 0x4,
            },
        )
    }
}

struct RootCount(Cell<usize>);

impl RootCount {
    // Creates new un-rooted RootCount
    fn new() -> RootCount {
        RootCount(Cell::new(0))
    }

    fn is_rooted(&self) -> bool {
        self.0.get() > 0
    }

    fn increment(&self) {
        self.0
            .set(self.0.get().checked_add(1).expect("overflow on root count"));
    }

    fn decrement(&self) {
        self.0.set(
            self.0
                .get()
                .checked_sub(1)
                .expect("underflow on root count"),
        );
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BorrowMode {
    Reading,
    Writing,
    Unused,
}

struct BorrowState(Cell<usize>);

impl BorrowState {
    // Creates a new BorrowState in mode Unused
    fn new() -> BorrowState {
        BorrowState(Cell::new(0))
    }

    fn mode(&self) -> BorrowMode {
        match self.0.get() {
            0 => BorrowMode::Unused,
            usize::MAX => BorrowMode::Writing,
            _ => BorrowMode::Reading,
        }
    }

    fn set_unused(&self) {
        self.0.set(0)
    }

    fn set_writing(&self) {
        self.0.set(usize::MAX)
    }

    // Only call when the mode is not `BorrowMode::Writing`
    fn inc_reading(&self) {
        debug_assert!(self.mode() != BorrowMode::Writing);
        self.0.set(
            self.0
                .get()
                .checked_add(1)
                .expect("too many GcRefs created"),
        );
    }

    // Only call when the mode is `BorrowMode::Reading`
    fn dec_reading(&self) {
        debug_assert!(self.mode() == BorrowMode::Reading);
        self.0.set(self.0.get() - 1);
    }
}
