#![allow(unused)]

use std::cell::{Cell, UnsafeCell};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{mem, process, usize};

/// A trait for garbage collected objects that contain `Gc` pointers.  It should not be possible to
/// cause unsafety by implementing this method incorrectly, but implementing it incorrectly may
/// cause held `Gc` pointers to be improperly collected.  Collected pointers cannot cause unsafety,
/// but they can cause panics.  `Gc` pointers may also be collected during drop, so `GcObject`
/// should not access held `Gc` pointers there.
pub trait GcObject: 'static {
    /// The `trace` should must trace through all owned Gc pointers, and every owned Gc pointer
    /// should be created from the same GcContext that holds this GcObject.
    fn trace(&self, context: &GcContext) {}
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

    // All potentially live garbage collected objects are kept in the live list, known dead objects
    // are kept in the dead list until their last Gc is dropped.
    live: Cell<Option<NonNull<GcBox<GcObject>>>>,
    dead: Cell<Option<NonNull<GcBox<GcObject>>>>,

    // All light-gray or dark-gray objects must be held in the gray queue, and the gray queue will
    // hold no white or black objects.  The gray queue is split into two parts, the primary part
    // primarly holds objects that enter the queue by becoming dark-gray and are processed with
    // first priority.  The secondary part holds objects that enter the queue by becoming rooted,
    // and are processed secondarily, to give rooted objects the time to become un-rooted before
    // processing.  Also, if an object is taken from the primary queue but is mutably locked, it is
    // moved to the secondary queue to be processed later.  If the primary queue is empty, the
    // primary and secondary queue are swapped so that the secondary queue can be re-scanned.
    gray_queue: NonNull<GrayQueue>,

    // During the sweep phase, the `all` list is scanned.  Black objects are turned white and white
    // objects are freed.
    sweep_position: Cell<Option<NonNull<GcBox<GcObject>>>>,
}

impl Default for GcContext {
    fn default() -> GcContext {
        const PAUSE_MULTIPLIER: f64 = 1.5;
        const TIMING_MULTIPLIER: f64 = 1.0;

        GcContext::new(PAUSE_MULTIPLIER, TIMING_MULTIPLIER)
    }
}

impl Drop for GcContext {
    fn drop(&mut self) {
        // Unimplemented, does nothing right now.  Once garbage collection is enabled, this should
        // run a full gc cycle and then check for any live objects.  If there are live objects, this
        // is an error, and this should panic and leak memory to maintain safety.
    }
}

impl GcContext {
    pub fn new(pause_multiplier: f64, timing_multiplier: f64) -> GcContext {
        assert!(pause_multiplier >= 1.0);
        assert!(timing_multiplier >= 0.0);

        let gray_queue = GrayQueue {
            uphold_invariant: false,
            primary: Cell::new(None),
            secondary: Cell::new(None),
        };
        let gray_queue = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(gray_queue))) };

        GcContext {
            pause_multiplier,
            timing_multiplier,
            total_allocated: Cell::new(0),
            phase: Cell::new(GcPhase::Sleeping),
            current_white: Cell::new(GcColor::White1),
            live: Cell::new(None),
            dead: Cell::new(None),
            gray_queue,
            sweep_position: Cell::new(None),
        }
    }

    /// Move a value of type T into a Gc<T>.  Any further allocation may trigger a collection, so in
    /// general you should either root this Gc or place it into a managed GcObject before allocating
    /// again so that it does not become invalid.
    pub fn allocate<T: GcObject>(&self, value: T) -> Gc<T> {
        self.do_work();

        let gc_box = GcBox {
            header: GcBoxHeader {
                flags: GcFlags::new(self.current_white.get()),
                ptr_count: RefCount::new(),
                root_count: RefCount::new(),
                borrow_state: BorrowState::new(),
                next: Cell::new(None),
                gray_link: Cell::new(GrayLink {
                    queue: self.gray_queue,
                }),
            },
            data: UnsafeCell::new(value),
        };
        gc_box.header.next.set(self.live.get());
        gc_box.header.ptr_count.increment();

        let gc_box = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(gc_box))) };
        self.live.set(Some(gc_box));

        Gc {
            gc_box,
            marker: PhantomData,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Gc<T: GcObject> {
    gc_box: NonNull<GcBox<T>>,
    marker: PhantomData<Rc<T>>,
}

impl<T: GcObject> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        unsafe {
            self.gc_box.as_ref().header.ptr_count.increment();
        }
        Gc {
            gc_box: self.gc_box,
            marker: PhantomData,
        }
    }
}

impl<T: GcObject> Drop for Gc<T> {
    fn drop(&mut self) {
        unsafe {
            self.gc_box.as_ref().header.ptr_count.decrement();
        }
    }
}

impl<T: GcObject> Gc<T> {
    /// Root this Gc pointer, panics if this Gc has been collected.
    pub fn root(&self) -> Rgc<T> {
        unsafe {
            let header = &self.gc_box.as_ref().header;
            assert!(header.flags.is_live(), "Gc has been collected");
            if header.flags.color().is_white() {
                // If our object is white, rooting it should turn it light-gray and place it into the
                // secondary gray queue.
                header.flags.set_color(GcColor::LightGray);
                let gray_queue = header.gray_link.get().queue;
                header.gray_link.set(GrayLink {
                    next: gray_queue.as_ref().secondary.get(),
                });
                gray_queue.as_ref().secondary.set(Some(self.gc_box));
            }
            header.root_count.increment();
        }

        Rgc {
            gc_box: self.gc_box,
            marker: PhantomData,
        }
    }

    /// Read the contents of this Gc.  Panics if the Gc has been collected or if this Gc is
    /// currently being written to.
    pub fn read(&self) -> GcRead<T> {
        unsafe {
            assert!(
                self.gc_box.as_ref().header.flags.is_live(),
                "Gc has been collected"
            );
            read_gc(&self.gc_box)
        }
    }

    /// Write to the contents of this Gc / Rgc.  Panics if the Gc has been collected, or the
    /// contents are already read or written on this or another cloned Gc.  Writing to a Gc will
    /// potentially execute a write barrier on the object, and will block tracing of the object for
    /// the lifetime of the `GcWrite`.
    pub fn write(&self) -> GcWrite<T> {
        unsafe {
            assert!(
                self.gc_box.as_ref().header.flags.is_live(),
                "Gc has been collected"
            );
            write_gc(&self.gc_box)
        }
    }

    /// Only call from `GcObject::trace`.
    pub fn trace(&self) {
        unsafe {
            let gc_box = self.gc_box.as_ref();
            assert!(gc_box.header.flags.is_live(), "Gc has been collected");
            match gc_box.header.flags.color() {
                GcColor::Black | GcColor::DarkGray => {}
                GcColor::LightGray => {
                    // A light-gray object is already in the gray queue, just turn it dark gray.
                    gc_box.header.flags.set_color(GcColor::DarkGray);
                }
                GcColor::White1 | GcColor::White2 => {
                    // A white object is not in the gray queue, becomes dark-gray and enters in the
                    // primary queue.
                    gc_box.header.flags.set_color(GcColor::DarkGray);
                    let gray_queue = gc_box.header.gray_link.get().queue;
                    gc_box.header.gray_link.set(GrayLink {
                        next: gray_queue.as_ref().primary.get(),
                    });
                    gray_queue.as_ref().primary.set(Some(self.gc_box));
                }
            }
        }
    }

    pub fn collect_garbage(&self) {
        unimplemented!()
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Rgc<T: GcObject> {
    gc_box: NonNull<GcBox<T>>,
    marker: PhantomData<Rc<T>>,
}

impl<T: GcObject> Clone for Rgc<T> {
    fn clone(&self) -> Rgc<T> {
        unsafe {
            let header = &self.gc_box.as_ref().header;
            header.root_count.increment();
            Rgc {
                gc_box: self.gc_box,
                marker: PhantomData,
            }
        }
    }
}

impl<T: GcObject> Drop for Rgc<T> {
    fn drop(&mut self) {
        unsafe {
            self.gc_box.as_ref().header.root_count.decrement();
        }
    }
}

impl<T: GcObject> Rgc<T> {
    pub fn unroot(&self) -> Gc<T> {
        unsafe {
            self.gc_box.as_ref().header.ptr_count.increment();
        }
        Gc {
            gc_box: self.gc_box,
            marker: PhantomData,
        }
    }

    /// Safe version of `Gc::read`, as we know that the pointer cannot be collected.
    pub fn read(&self) -> GcRead<T> {
        unsafe {
            debug_assert!(self.gc_box.as_ref().header.flags.is_live());
            read_gc(&self.gc_box)
        }
    }

    /// Safe version of `Gc::write`, as we know that the pointer cannot be collected.
    pub fn write(&self) -> GcWrite<T> {
        unsafe {
            debug_assert!(self.gc_box.as_ref().header.flags.is_live());
            write_gc(&self.gc_box)
        }
    }
}

pub struct GcRead<'a, T: GcObject> {
    borrow_state: &'a BorrowState,
    value: &'a T,
}

impl<'a, T: GcObject> Deref for GcRead<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'a, T: GcObject> Drop for GcRead<'a, T> {
    fn drop(&mut self) {
        self.borrow_state.dec_reading();
    }
}

pub struct GcWrite<'a, T: GcObject> {
    borrow_state: &'a BorrowState,
    value: &'a mut T,
}

impl<'a, T: GcObject> Deref for GcWrite<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'a, T: GcObject> DerefMut for GcWrite<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.value
    }
}

impl<'a, T: GcObject> Drop for GcWrite<'a, T> {
    fn drop(&mut self) {
        self.borrow_state.set_unused();
    }
}

impl GcContext {
    fn do_work(&self) {
        // unimplemented
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum GcPhase {
    Sleeping,
    Propagate,
    Atomic,
    Sweeping,
}

struct GrayQueue {
    uphold_invariant: bool,
    primary: Cell<Option<NonNull<GcBox<GcObject>>>>,
    secondary: Cell<Option<NonNull<GcBox<GcObject>>>>,
}

// If the color of a GcBox is white or black, holds a pointer to the GrayQueue.  If the color of the
// GcBox is light-gray or dark-gray, holds the next pointer for the gray list it is on.
#[derive(Copy, Clone)]
union GrayLink {
    queue: NonNull<GrayQueue>,
    next: Option<NonNull<GcBox<GcObject>>>,
}

struct GcBoxHeader {
    flags: GcFlags,
    ptr_count: RefCount,
    root_count: RefCount,
    borrow_state: BorrowState,
    next: Cell<Option<NonNull<GcBox<GcObject>>>>,
    gray_link: Cell<GrayLink>,
}

struct GcBox<T: GcObject + ?Sized> {
    header: GcBoxHeader,
    data: UnsafeCell<T>,
}

unsafe fn read_gc<T: GcObject>(gc_box: &NonNull<GcBox<T>>) -> GcRead<T> {
    let gc_box = gc_box.as_ref();
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

unsafe fn write_gc<T: GcObject>(gc_box_ptr: &NonNull<GcBox<T>>) -> GcWrite<T> {
    let gc_box = gc_box_ptr.as_ref();
    // During the propagate phase, if we are writing to a black object, we need it to become
    // dark gray to uphold the black invariant.
    if gc_box.header.flags.color() == GcColor::Black {
        let gray_queue = gc_box.header.gray_link.get().queue;
        if gray_queue.as_ref().uphold_invariant {
            gc_box.header.flags.set_color(GcColor::DarkGray);
            gc_box.header.gray_link.set(GrayLink {
                next: gray_queue.as_ref().primary.get(),
            });
            gray_queue.as_ref().primary.set(Some(*gc_box_ptr));
        }
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

impl GcColor {
    fn is_white(self) -> bool {
        match self {
            GcColor::White1 | GcColor::White2 => true,
            _ => false,
        }
    }
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

    fn is_live(&self) -> bool {
        self.0.get() & 0x80 == 0
    }

    fn set_dead(&self) {
        self.0.set(self.0.get() & !0x80)
    }
}

struct RefCount(Cell<usize>);

impl RefCount {
    // Creates new zero RefCount
    fn new() -> RefCount {
        RefCount(Cell::new(0))
    }

    fn is_zero(&self) -> bool {
        self.0.get() == 0
    }

    fn is_nonzero(&self) -> bool {
        self.0.get() != 0
    }

    fn increment(&self) {
        if self.0.get() == usize::MAX {
            process::abort();
        }
        self.0.set(self.0.get() + 1);
    }

    fn decrement(&self) {
        debug_assert!(self.0.get() > 0, "underflow on root count");
        self.0.set(self.0.get() - 1);
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
        if self.0.get() == usize::MAX {
            process::abort();
        }
        self.0.set(self.0.get() + 1);
    }

    // Only call when the mode is `BorrowMode::Reading`
    fn dec_reading(&self) {
        debug_assert!(self.mode() == BorrowMode::Reading);
        self.0.set(self.0.get() - 1);
    }
}
