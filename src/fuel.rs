use std::ops::{Deref, DerefMut};

use thiserror::Error;

pub const RECURSION_LIMIT: u8 = u8::MAX;

/// A counter for tracking the amount of time spent in `Thread::step` and in callbacks.
///
/// The fuel unit is *approximately* one VM instruction, but this is just a rough estimate
/// (especially since VM instructions are highly variable in cost, such as with the len operator).
///
/// All operations that take a variable amount of time should consume some amount of fuel, so that
/// it is always possible to bound the amount of time spent in `Thread::step`.
#[derive(Debug, Clone)]
pub struct Fuel {
    fuel: i32,
    interrupted: bool,
    recursion_level: u8,
}

impl Fuel {
    pub fn empty() -> Self {
        Self::with_fuel(0)
    }

    pub fn with_fuel(fuel: i32) -> Self {
        Self {
            fuel,
            interrupted: false,
            recursion_level: 0,
        }
    }

    /// Refills fuel up to a given maximum and also clears the fuel interrupt flag.
    ///
    /// This is a convenience method that is intended to be called outside the outermost call to
    /// `Thread::step` in some loop. It does the operations that all loops that would *re-use* a
    /// fuel container from one tick to another would need to do.
    ///
    /// It credits the running thread with fuel while also preventing available fuel from growing to
    /// an unbounded level, and also clears the interrupt flag since we are now on the "outside" of
    /// whatever lua code is being run and this is where we would want to interrupt to.
    pub fn refill(&mut self, fuel: i32, max_fuel: i32) {
        self.fuel = self.fuel.saturating_add(fuel).min(max_fuel);
        self.interrupted = false;
    }

    /// Add to or subtract from the current remaining fuel.
    pub fn adjust_fuel(&mut self, fuel: i32) {
        self.fuel = self.fuel.saturating_add(fuel);
    }

    /// Subtract from the current remaining fuel.
    ///
    /// This is a convenience method that is equivalent to `self.adjust_fuel(-fuel)`.
    pub fn consume_fuel(&mut self, fuel: i32) {
        self.adjust_fuel(fuel.saturating_neg());
    }

    pub fn remaining_fuel(&self) -> i32 {
        self.fuel
    }

    pub fn set_remaining_fuel(&mut self, fuel: i32) {
        self.fuel = fuel;
    }

    /// Marks that the calling `Thread` should immediately stop executing, without actually
    /// consuming any fuel.
    ///
    /// This is useful in situations where you need to interrupt an executing `Thread` because it
    /// may be waiting on an external event that has not yet occurred, but still want to record the
    /// correct fuel usage.
    pub fn interrupt(&mut self) {
        self.interrupted = true;
    }

    pub fn is_interrupted(&self) -> bool {
        self.interrupted
    }

    pub fn clear_interrupt(&mut self) {
        self.interrupted = false;
    }

    /// Returns true if we have positive fuel remaining *and* we have not been interrupted.
    pub fn should_continue(&self) -> bool {
        self.fuel > 0 && !self.interrupted
    }

    /// Mark that we are about to run a Rust callback that is potentially controlled by untrusted
    /// code.
    ///
    /// Increments the current recursion level if it is below `RECURSION_LIMIT`. If the recursion
    /// level would rise above the limit, this returns a recursion error, otherwise returns a
    /// guard that restores the previous recursion level on drop. This prevents untrusted code from
    /// consuming arbitrary Rust stack depth and execution time by tricking Rust code into recursing
    /// endlessly.
    ///
    /// By default, the recursion level is automatically incremented whenever `Thread::step` is
    /// about to trigger a callback, so this should almost never be necessary to call explicitly.
    ///
    /// With the normal stdlib, arbitrary recursion is only possible by (ab)using coroutines. Normal
    /// Lua recursion and Rust code "calling" Lua code via a `Sequence::poll` does not actually use
    /// the real Rust call stack, and cannot lead to using unbounded time or unbounded Rust stack
    /// space. Coroutines create their own inner `Thread`s and step them inside a `Sequence`, so
    /// they can eventually trigger a recursion limit in pathological cases.
    pub fn recurse(&mut self) -> Result<Recurse<'_>, RecursionLimit> {
        if self.recursion_level == RECURSION_LIMIT {
            return Err(RecursionLimit);
        }

        self.recursion_level += 1;
        Ok(Recurse(self))
    }

    /// Returns the current Rust callback recursion level.
    pub fn recursion_level(&self) -> u8 {
        self.recursion_level
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("callback recursion limit reached")]
pub struct RecursionLimit;

pub struct Recurse<'a>(&'a mut Fuel);

impl<'a> Drop for Recurse<'a> {
    fn drop(&mut self) {
        self.recursion_level -= 1;
    }
}

impl<'a> Deref for Recurse<'a> {
    type Target = Fuel;

    fn deref(&self) -> &Fuel {
        self.0
    }
}

impl<'a> DerefMut for Recurse<'a> {
    fn deref_mut(&mut self) -> &mut Fuel {
        self.0
    }
}
