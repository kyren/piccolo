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
}

impl Fuel {
    pub fn empty() -> Self {
        Self::with_fuel(0)
    }

    pub fn with_fuel(fuel: i32) -> Self {
        Self {
            fuel,
            interrupted: false,
        }
    }

    pub fn adjust_fuel(&mut self, fuel: i32) {
        self.fuel = self.fuel.saturating_add(fuel);
    }

    pub fn consume_fuel(&mut self, fuel: i32) {
        self.adjust_fuel(fuel.saturating_neg());
    }

    pub fn remaining_fuel(&self) -> i32 {
        self.fuel
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
}
