use std::{
    any::TypeId,
    cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut},
};

use gc_arena::{unsize, Collect, CollectionContext, Gc, MutationContext, Rootable};

// Garbage collected `Any` type that can be downcast.
//
// SAFETY:
//
// Non'-static downcasting is notoriously dangerous. Rather than allowing arbitrary non-'static
// data to be downcast, we rely on the fact that *only* a single 'gc lifetime is present in the held
// type. We use the `Rootable` type as a proxy rather than the stored type itself to know what type
// is actually being held.
//
// Safety here is dependent on two subtle points:
//
// 1) We use the proxy `Rootable` type as the source of the `TypeId` rather than the projected
//    `<R as Rootable<'_>>::Root`. If we were to instead use `<R as Rootable<'static>>:Root` for
//    the `TypeId`, then you could fool this into giving you a type with the wrong projection by
//    implementing `Rootable` for two separate types that project to the same type differently. For
//    example, if you had a `Dangerous<'a, 'b>` type, you could have a `BadRootable1` that projects
//    to `Dangerous<'ctx, 'static>` and a `BadRootable2` that projects to `Dangerous<'static,
//    'ctx>`. Since both of these contexts would project to `Dangerous<'static, 'static>` for the
//    purposes of getting a `TypeId`, there would be no way to distinguish them, and this could be
//    used to transmute any lifetime to or from 'ctx. By using the `TypeId` of the rootable type
//    itself, we know we always return the same projection that we were given.
//
// 2) The `Gc` type is *invariant* in the 'gc lifetime. If it was instead covariant or contravariant
//    in 'gc, then we could store a type with a mismatched variance and improperly lengthen or
//    shorten the 'gc lifetime for that type. Since `Gc` is invariant in 'gc (the entire garbage
//    collection system relies on this), the context can project to a type with any variance in 'gc
//    and nothing can go wrong.

pub trait AnyValue<'gc> {
    fn get(&self) -> (TypeId, *const ());
}

pub fn new<'gc, R>(
    mc: MutationContext<'gc, '_>,
    val: <R as Rootable<'gc>>::Root,
) -> Gc<'gc, dyn AnyValue<'gc>>
where
    R: for<'a> Rootable<'a> + ?Sized + 'static,
{
    let ptr = Gc::allocate(mc, Value::<R>(RefCell::new(val)));
    unsize!(ptr => dyn AnyValue)
}

pub fn read<'gc, 'a, R>(
    v: &'a Gc<'gc, dyn AnyValue<'gc>>,
) -> Option<Result<Ref<'a, <R as Rootable<'gc>>::Root>, BorrowError>>
where
    R: for<'b> Rootable<'b> + ?Sized + 'static,
{
    let cell = cast::<R>(v)?;
    Some(cell.try_borrow())
}

pub fn write<'gc, 'a, R>(
    mc: MutationContext<'gc, '_>,
    v: &'a Gc<'gc, dyn AnyValue<'gc>>,
) -> Option<Result<RefMut<'a, <R as Rootable<'gc>>::Root>, BorrowMutError>>
where
    R: for<'b> Rootable<'b> + ?Sized + 'static,
{
    let cell = cast::<R>(v)?;
    let res = cell.try_borrow_mut();
    if res.is_ok() {
        Gc::write_barrier(mc, *v);
    }
    Some(res)
}

fn cast<'gc, 'a, R>(
    v: &'a Gc<'gc, dyn AnyValue<'gc>>,
) -> Option<&'a RefCell<<R as Rootable<'gc>>::Root>>
where
    R: for<'b> Rootable<'b> + ?Sized + 'static,
{
    let (type_id, ptr) = v.get();
    if type_id == TypeId::of::<R>() {
        let ptr = ptr as *const RefCell<<R as Rootable<'gc>>::Root>;
        Some(unsafe { &*ptr })
    } else {
        None
    }
}

struct Value<'gc, R>(RefCell<<R as Rootable<'gc>>::Root>)
where
    R: for<'a> Rootable<'a> + ?Sized + 'static;

unsafe impl<'gc, R> Collect for Value<'gc, R>
where
    R: for<'a> Rootable<'a> + ?Sized + 'static,
    <R as Rootable<'gc>>::Root: Collect,
{
    fn trace(&self, cc: CollectionContext) {
        self.0.borrow().trace(cc);
    }
}

impl<'gc, R> AnyValue<'gc> for Value<'gc, R>
where
    R: for<'a> Rootable<'a> + ?Sized + 'static,
{
    fn get(&self) -> (TypeId, *const ()) {
        (
            TypeId::of::<R>(),
            &self.0 as *const RefCell<<R as Rootable<'gc>>::Root> as *const (),
        )
    }
}
