use std::{
    any::TypeId,
    cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut},
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{unsize, Collect, Gc, MutationContext, Rootable};

// Garbage collected `Any` type that can be downcast.

#[derive(Collect)]
#[collect(no_drop, bound = "")]
pub struct AnyCell<'gc, M>(Gc<'gc, dyn AnyValue<'gc, Rootable!['gc_ => UnsafeCell<'gc_, M>]>>)
where
    M: for<'a> Rootable<'a> + ?Sized + 'static;

impl<'gc, M> Copy for AnyCell<'gc, M> where M: for<'a> Rootable<'a> + ?Sized {}

impl<'gc, M> Clone for AnyCell<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, M> fmt::Debug for AnyCell<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
    <M as Rootable<'gc>>::Root: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("AnyGcCell")
            .field("metadata", &self.0.metadata().0)
            .field("data", &(self.0.data().1 as *const _))
            .finish()
    }
}

impl<'gc, M> PartialEq for AnyCell<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
{
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc, M> Eq for AnyCell<'gc, M> where M: for<'a> Rootable<'a> + ?Sized {}

impl<'gc, M> Hash for AnyCell<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc, M> AnyCell<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
{
    pub fn new<R>(
        mc: MutationContext<'gc, '_>,
        metadata: <M as Rootable<'gc>>::Root,
        data: <R as Rootable<'gc>>::Root,
    ) -> Self
    where
        R: for<'a> Rootable<'a> + ?Sized + 'static,
    {
        Self(new_any::<
            Rootable!['gc_ => UnsafeCell<'gc_, M>],
            Rootable!['gc_ => UnsafeCell<'gc_, R>],
        >(
            mc,
            UnsafeCell(RefCell::new(metadata)),
            UnsafeCell(RefCell::new(data)),
        ))
    }

    // Only used for display, internal pointer type is private.
    pub fn as_ptr(&self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn read_metadata<'a>(&'a self) -> Result<Ref<'a, <M as Rootable<'gc>>::Root>, BorrowError> {
        self.0.metadata().0.try_borrow()
    }

    pub fn write_metadata<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, <M as Rootable<'gc>>::Root>, BorrowMutError> {
        let res = self.0.metadata().0.try_borrow_mut();
        if res.is_ok() {
            Gc::write_barrier(mc, self.0);
        }
        res
    }

    pub fn read_data<'a, R>(
        &'a self,
    ) -> Option<Result<Ref<'a, <R as Rootable<'gc>>::Root>, BorrowError>>
    where
        R: for<'b> Rootable<'b> + ?Sized + 'static,
    {
        let cell = get_data::<_, Rootable!['gc_ => UnsafeCell<'gc_, R>]>(&self.0)?;
        Some(cell.0.try_borrow())
    }

    pub fn write_data<'a, R>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Option<Result<RefMut<'a, <R as Rootable<'gc>>::Root>, BorrowMutError>>
    where
        R: for<'b> Rootable<'b> + ?Sized + 'static,
    {
        let cell = get_data::<_, Rootable!['gc_ => UnsafeCell<'gc_, R>]>(&self.0)?;
        let res = cell.0.try_borrow_mut();
        if res.is_ok() {
            Gc::write_barrier(mc, self.0);
        }
        Some(res)
    }
}

// SAFETY: Must call write barrier before writing
struct UnsafeCell<'gc, T>(RefCell<<T as Rootable<'gc>>::Root>)
where
    T: for<'a> Rootable<'a> + ?Sized;

unsafe impl<'gc, T> Collect for UnsafeCell<'gc, T>
where
    T: for<'a> Rootable<'a> + ?Sized,
    <T as Rootable<'gc>>::Root: Collect,
{
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        <T as Rootable<'gc>>::Root::needs_trace()
    }

    fn trace(&self, cc: gc_arena::CollectionContext) {
        self.0.borrow().trace(cc)
    }
}

// SAFETY:
//
// Non-'static downcasting is notoriously dangerous. Rather than allowing arbitrary non-'static
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

unsafe trait AnyValue<'gc, M>
where
    M: for<'a> Rootable<'a> + ?Sized,
{
    fn metadata(&self) -> &<M as Rootable<'gc>>::Root;
    fn data(&self) -> (TypeId, *const ());
}

fn new_any<'gc, M, R>(
    mc: MutationContext<'gc, '_>,
    metadata: <M as Rootable<'gc>>::Root,
    data: <R as Rootable<'gc>>::Root,
) -> Gc<'gc, dyn AnyValue<'gc, M>>
where
    M: for<'a> Rootable<'a> + ?Sized,
    R: for<'a> Rootable<'a> + ?Sized + 'static,
{
    let ptr = Gc::allocate(mc, Value::<M, R> { metadata, data });
    unsize!(ptr => dyn AnyValue<'gc, M>)
}

fn get_data<'gc, 'a, M, R>(
    v: &'a Gc<'gc, dyn AnyValue<'gc, M>>,
) -> Option<&'a <R as Rootable<'gc>>::Root>
where
    M: for<'b> Rootable<'b> + ?Sized,
    R: for<'b> Rootable<'b> + ?Sized + 'static,
{
    let (type_id, ptr) = v.data();
    if type_id == TypeId::of::<R>() {
        let ptr = ptr as *const <R as Rootable<'gc>>::Root;
        Some(unsafe { &*ptr })
    } else {
        None
    }
}

#[derive(Collect)]
#[collect(no_drop, bound = "")]
struct Value<'gc, M, R>
where
    M: for<'a> Rootable<'a> + ?Sized,
    R: for<'a> Rootable<'a> + ?Sized + 'static,
    <R as Rootable<'gc>>::Root: Collect,
{
    metadata: <M as Rootable<'gc>>::Root,
    data: <R as Rootable<'gc>>::Root,
}

unsafe impl<'gc, M, R> AnyValue<'gc, M> for Value<'gc, M, R>
where
    M: for<'a> Rootable<'a> + ?Sized,
    R: for<'a> Rootable<'a> + ?Sized + 'static,
{
    fn metadata(&self) -> &<M as Rootable<'gc>>::Root {
        &self.metadata
    }

    fn data(&self) -> (TypeId, *const ()) {
        (
            TypeId::of::<R>(),
            &self.data as *const <R as Rootable<'gc>>::Root as *const (),
        )
    }
}
