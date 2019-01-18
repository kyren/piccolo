use gc_arena::{Collect, MutationContext};

use crate::error::Error;
use crate::lua::LuaContext;

/// A trait that describes a sequence of VM actions to perform with an eventual result.
///
/// This trait is similar to the `Future` trait in that it is not designed to be used directly, but
/// rather chained together using combinators and returned to an outer handler that will drive it to
/// completion.  There are three reasons for this design:
///
/// 1. The gc-arena design requires that the `mutate` method is exited before any garbage collection
///    can take place, but since VM code should not be able to delay garbage collection arbitrarily
///    long, we still need a way to execute some code, then garbage collect, then continue where we
///    left off.  This means that we must have a chained series of operations where in between each
///    link in the chain, we can exit the `mutate` method and perform garbage collection if
///    necessary.  This is also useful for sequencing separate allocating API calls and callbacks,
///    not just VM code, in such a way that garbage collection can occur with as fine of a
///    granularity as necessary.
///
/// 2. Full interoperability with Lua coroutines is difficult in a language without guaranteed TCO,
///    but there still needs to be a way to continue to another Lua or Rust function without
///    arbitrarily growing the Rust stack.  Even disregarding coroutines, this mechanism also allows
///    there not to be arbitrary limits on the Rust callback depth.
///
/// 3. It allows for an asynchronous API.  Using this, Rust code can be treated similarly to Lua VM
///    code in that it can be paused, resumed, and incrementally executed.  With a normal chain of
///    Rust -> Lua -> Rust -> Lua calls, it would not be possible to stop executing a script without
///    returning through all of the real Rust frames in the call stack, but it *is* possible to stop
///    arbitrarily at any point in a Sequence and resume later.
pub trait Sequence<'gc>: Collect {
    type Item;

    /// Perform a single unit of work, returning `Some` on completion, whether succsessful or not.
    /// Calling `pump` again after completion is an API violation and may panic.
    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Error>>;
}

impl<'gc, T: ?Sized + Sequence<'gc>> Sequence<'gc> for Box<T> {
    type Item = T::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Error>> {
        T::pump(&mut (*self), mc, lc)
    }
}
