use std::{
    any::TypeId,
    cell::{BorrowError, BorrowMutError, Ref, RefMut},
    fmt,
};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation, Root, Rootable};

/// Garbage collected `Any` type that can be downcast.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct AnyCell<'gc, M: 'gc>(AnyValue<'gc, RefLock<M>>);

impl<'gc, M> fmt::Debug for AnyCell<'gc, M>
where
    M: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("AnyGcCell")
            .field("metadata", self.0.metadata())
            .field("type_id", &(self.0.type_id()))
            .finish()
    }
}

impl<'gc, M> AnyCell<'gc, M> {
    pub fn new<R>(mc: &Mutation<'gc>, metadata: M, data: Root<'gc, R>) -> Self
    where
        M: Collect,
        R: for<'a> Rootable<'a>,
    {
        Self(AnyValue::new::<Rootable!['gc_ => RefLock<Root<'gc_, R>>]>(
            mc,
            RefLock::new(metadata),
            RefLock::new(data),
        ))
    }

    pub fn as_ptr(&self) -> *const () {
        self.0.as_ptr()
    }

    pub fn is<R>(&self) -> bool
    where
        R: for<'a> Rootable<'a>,
    {
        self.0.is::<Rootable!['a => RefLock<Root<'a, R>>]>()
    }

    pub fn read_metadata<'a>(&'a self) -> Result<Ref<'a, M>, BorrowError> {
        self.0.metadata().try_borrow()
    }

    pub fn write_metadata<'a>(
        &'a self,
        mc: &Mutation<'gc>,
    ) -> Result<RefMut<'a, M>, BorrowMutError> {
        // SAFETY: We make sure to call the write barrier on successful borrowing.
        let res = unsafe { self.0.metadata().as_ref_cell().try_borrow_mut() };
        if res.is_ok() {
            Gc::write(mc, self.0 .0);
        }
        res
    }

    pub fn read_data<'a, R>(&'a self) -> Option<Result<Ref<'a, Root<'gc, R>>, BorrowError>>
    where
        R: for<'b> Rootable<'b>,
    {
        let cell = self
            .0
            .downcast::<Rootable!['gc_ => RefLock<Root<'gc_, R>>]>()?;
        Some(cell.try_borrow())
    }

    pub fn write_data<'a, R>(
        &'a self,
        mc: &Mutation<'gc>,
    ) -> Option<Result<RefMut<'a, Root<'gc, R>>, BorrowMutError>>
    where
        R: for<'b> Rootable<'b>,
    {
        let cell = self
            .0
            .downcast::<Rootable!['gc_ => RefLock<Root<'gc_, R>>]>()?;
        // SAFETY: We make sure to call the write barrier on successful borrowing.
        let res = unsafe { cell.as_ref_cell().try_borrow_mut() };
        if res.is_ok() {
            Gc::write(mc, self.0 .0);
        }
        Some(res)
    }
}

// SAFETY:
//
// Non-'static downcasting is notoriously dangerous. Rather than allowing arbitrary non-'static data
// to be downcast, we rely on the fact that *only* a single non-'static 'gc lifetime is present in
// the held type. We use the `Rootable` type as a proxy rather than the stored type itself to know
// what type is actually being held.
//
// Safety here is dependent on three subtle points:
//
// 1) The Rootable trait only allows for the projection of a single lifetime. We know this because
//    `<R as Rootable<'static>>::Root` is 'static, so the only possible non-'static lifetime
//    that the projection can have is the 'gc lifetime we give it. We don't lose any lifetime
//    information, the only non-'static lifetime is 'gc and we can restore this lifetime upon
//    access.
//
// 2) The `Gc` type is *invariant* in the 'gc lifetime. If it was instead covariant or contravariant
//    in 'gc, then we could store a type with a mismatched variance and improperly lengthen or
//    shorten the 'gc lifetime for that type. Since `Gc` is invariant in 'gc (the entire garbage
//    collection system relies on this), `AnyValue` can project to a type with any variance in 'gc
//    and nothing can go wrong.
//
// 3) We use the proxy `Rootable` type as the source of the `TypeId` rather than the projected
//    `<R as Rootable<'_>>::Root`. If we were to instead use `<R as Rootable<'static>>:Root` for
//    the `TypeId`, then you could fool this into giving you a type with the wrong projection by
//    implementing `Rootable` for two separate types that project to the same type differently. For
//    example, if you had a `Dangerous<'a, 'b>` type, you could have a `BadRootable1` that projects
//    to `Dangerous<'gc, 'static>` and a `BadRootable2` that projects to `Dangerous<'static, 'gc>`.
//    Since `<BR as Rootable<'static>>::Root` for both of these types would project to
//    `Dangerous<'static, 'static>` for the purposes of getting a `TypeId`, there would be no way
//    to distinguish them, and this could be used to transmute any lifetime to or from 'gc. By using
//    the `TypeId` of the rootable type itself, we know we always return the same projection that we
//    were given.

#[derive(Collect)]
#[collect(no_drop)]
struct Header<M> {
    metadata: M,
    type_id: TypeId,
}

#[derive(Collect)]
#[collect(no_drop)]
#[repr(C)]
struct Value<M, V> {
    header: Header<M>,
    data: V,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct AnyValue<'gc, M: 'gc>(Gc<'gc, Header<M>>);

impl<'gc, M> Copy for AnyValue<'gc, M> {}

impl<'gc, M> Clone for AnyValue<'gc, M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, M> AnyValue<'gc, M> {
    fn new<R>(mc: &Mutation<'gc>, metadata: M, data: Root<'gc, R>) -> Self
    where
        M: Collect,
        R: for<'a> Rootable<'a>,
    {
        let val = Gc::new(
            mc,
            Value::<M, Root<'gc, R>> {
                header: Header {
                    metadata,
                    type_id: TypeId::of::<R>(),
                },
                data,
            },
        );

        // SAFETY: We know we can cast to a `Header<M>` because `Value<M, Root<'gc, R>>` is
        // `#[repr(C)]` and `Header<M>` is the first field
        Self(unsafe { Gc::cast::<Header<M>>(val) })
    }

    pub fn metadata(&self) -> &M {
        &self.0.metadata
    }

    pub fn as_ptr(&self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn type_id(&self) -> TypeId {
        self.0.type_id
    }

    pub fn is<R>(&self) -> bool
    where
        R: for<'b> Rootable<'b>,
    {
        TypeId::of::<R>() == self.0.type_id
    }

    fn downcast<R>(&self) -> Option<&Root<'gc, R>>
    where
        R: for<'b> Rootable<'b>,
    {
        if TypeId::of::<R>() == self.0.type_id {
            let ptr = Gc::as_ptr(self.0) as *const Value<M, Root<'gc, R>>;
            Some(unsafe { &(*ptr).data })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::rootless_arena;

    use super::*;

    #[test]
    fn test_any_value() {
        rootless_arena(|mc| {
            #[derive(Collect)]
            #[collect(no_drop)]
            struct A<'gc>(Gc<'gc, i32>);

            #[derive(Collect)]
            #[collect(no_drop)]
            struct B<'gc>(Gc<'gc, i32>);

            #[derive(Collect)]
            #[collect(no_drop)]
            struct C<'gc>(Gc<'gc, i32>);

            let any1 = AnyValue::new::<Rootable![A<'gc>]>(mc, 1i32, A(Gc::new(mc, 5)));
            let any2 = AnyValue::new::<Rootable![B<'gc>]>(mc, 2i32, B(Gc::new(mc, 6)));
            let any3 = AnyValue::new::<Rootable![C<'gc>]>(mc, 3i32, C(Gc::new(mc, 7)));

            assert!(any1.is::<Rootable![A<'gc>]>());
            assert!(!any1.is::<Rootable![B<'gc>]>());
            assert!(!any1.is::<Rootable![C<'gc>]>());

            assert_eq!(*any1.metadata(), 1);
            assert_eq!(*any1.downcast::<Rootable![A<'gc>]>().unwrap().0, 5);
            assert_eq!(*any2.metadata(), 2);
            assert_eq!(*any2.downcast::<Rootable![B<'gc>]>().unwrap().0, 6);
            assert_eq!(*any3.metadata(), 3);
            assert_eq!(*any3.downcast::<Rootable![C<'gc>]>().unwrap().0, 7);

            assert!(any1.downcast::<Rootable![B<'gc>]>().is_none());
            assert!(any1.downcast::<Rootable![C<'gc>]>().is_none());
            assert!(any2.downcast::<Rootable![A<'gc>]>().is_none());
            assert!(any2.downcast::<Rootable![C<'gc>]>().is_none());
            assert!(any3.downcast::<Rootable![A<'gc>]>().is_none());
            assert!(any3.downcast::<Rootable![B<'gc>]>().is_none());
        })
    }
}
