use std::{any::TypeId, hash::BuildHasherDefault};

use ahash::AHasher;
use gc_arena::{
    allocator_api::MetricsAlloc, arena::Root, lock::RefLock, Collect, DynamicRootSet, Gc, Mutation,
    Rootable,
};
use hashbrown::{hash_map, HashMap};

use crate::{
    any::Any,
    stash::{Fetchable, Stashable},
    Context,
};

/// A type which can have a single registered value per [`Lua`](crate::Lua) instance.
///
/// By implementing this trait, you can store things like metatables globally per `Lua` instance,
/// and avoid needlessly duplicating the same values.
pub trait Singleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self;
}

impl<'gc, T: Default> Singleton<'gc> for T {
    fn create(_: Context<'gc>) -> Self {
        Self::default()
    }
}

/// A collection of stashed values and [`Singleton`]s.
///
/// Generally, there is one globally accessible `Registry` per [`Lua`](crate::Lua) instance.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Registry<'gc> {
    roots: DynamicRootSet<'gc>,
    singletons:
        Gc<'gc, RefLock<HashMap<TypeId, Any<'gc>, BuildHasherDefault<AHasher>, MetricsAlloc<'gc>>>>,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        let singletons =
            HashMap::with_hasher_in(BuildHasherDefault::default(), MetricsAlloc::new(mc));

        Self {
            roots: DynamicRootSet::new(mc),
            singletons: Gc::new(mc, RefLock::new(singletons)),
        }
    }

    /// Create an instance of a type that exists at most once per [`Lua`](crate::Lua) instance.
    ///
    /// If the type has already been created, returns the already created instance, otherwise calls
    /// `S::create` to create a new instance and returns it.
    pub fn singleton<S>(&self, ctx: Context<'gc>) -> &'gc Root<'gc, S>
    where
        S: for<'a> Rootable<'a> + 'static,
        Root<'gc, S>: Sized + Singleton<'gc> + Collect,
    {
        let mut singletons = self.singletons.borrow_mut(&ctx);
        match singletons.entry(TypeId::of::<S>()) {
            hash_map::Entry::Occupied(occupied) => occupied.get().downcast::<S>().unwrap(),
            hash_map::Entry::Vacant(vacant) => {
                let v = Root::<'gc, S>::create(ctx);
                vacant
                    .insert(Any::new::<S>(&ctx, v))
                    .downcast::<S>()
                    .unwrap()
            }
        }
    }

    /// Returns the inner [`DynamicRootSet`] held inside the global registry.
    ///
    /// This can be used to create `'static` roots directly without having to deal with the
    /// [`Stashable`] trait.
    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    /// "Stash" a value with a `'gc` branding lifetime in the global registry, creating a `'static`
    /// handle to it.
    ///
    /// This works for any type that implements the [`Stashable`] trait, which all common `piccolo`
    /// types do.
    ///
    /// Values stashed in the global registry produce handles without the `'gc` lifetime branding,
    /// which makes them completely unrestricted. They are `'static` Rust types, which means that
    /// the borrow checker will not stop you from storing them *anywhere*, including within the Lua
    /// state itself. Do not do this!
    ///
    /// Registry stashed values are not meant to be held within the Lua state. Stashed handles are
    /// not traced like normal GC types and do not have full cycle collection, so any stashed value
    /// will only be freed when the returned handle is *dropped*. This means that if there is a
    /// cycle through a stashed handle (e.g. the stashed handle points to a Lua value which in turn
    /// points directly or indirectly to whatever Lua value owns the handle), the handle will never
    /// be dropped so the value (and anything it transitively points to) can never be freed.
    ///
    /// Values stashed in the registry are designed to be held *completely outside* of the Lua state
    /// by outer Rust code. If storing a value inside the Lua state, always use a proper garbage
    /// collected type, which in addition to allowing full cycle collection will also be cheaper.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> S::Stashed {
        s.stash(mc, self.roots)
    }

    /// "Fetch" the real value for a handle that has been returned from `Registry::stash`.
    ///
    /// It can be implemented for external types by implementing the `Fetchable` trait.
    ///
    /// # Panics
    ///
    /// If the given handle was not stashed using the global registry for *this* `Lua` instance,
    /// then this method will panic.
    pub fn fetch<F: Fetchable>(&self, f: &F) -> F::Fetched<'gc> {
        f.fetch(self.roots)
    }
}
