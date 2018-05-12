use std::cell::{Cell, UnsafeCell};
use std::marker::PhantomData;
use std::ptr::NonNull;

use collect::Collect;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) enum GcColor {
    White,
    Gray,
    Black,
}

pub(crate) struct GcBox<T: Collect + ?Sized> {
    pub(crate) flags: GcFlags,
    pub(crate) next: Cell<Option<NonNull<GcBox<Collect>>>>,
    pub(crate) value: UnsafeCell<T>,
}

pub(crate) struct GcFlags(Cell<u8>);

impl GcFlags {
    pub(crate) fn new() -> GcFlags {
        GcFlags(Cell::new(0))
    }

    pub(crate) fn color(&self) -> GcColor {
        match self.0.get() & 0x3 {
            0x0 => GcColor::White,
            0x1 => GcColor::Gray,
            0x2 => GcColor::Black,
            _ => unreachable!(),
        }
    }

    pub(crate) fn set_color(&self, color: GcColor) {
        self.0.set(
            (self.0.get() & !0x3) | match color {
                GcColor::White => 0x0,
                GcColor::Gray => 0x1,
                GcColor::Black => 0x2,
            },
        )
    }

    pub(crate) fn needs_trace(&self) -> bool {
        self.0.get() | 0x4 != 0x0
    }

    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        self.0
            .set((self.0.get() & !0x4) | if needs_trace { 0x4 } else { 0x0 });
    }
}

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'gc> = PhantomData<::std::cell::Cell<&'gc mut ()>>;
