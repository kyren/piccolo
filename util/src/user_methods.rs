use std::marker::PhantomData;

use gc_arena::{arena::Root, barrier, Collect, Rootable, Static};
use piccolo::{
    Callback, CallbackReturn, Context, Error, Execution, FromMultiValue, IntoMultiValue,
    MetaMethod, Table, UserData,
};

/// An easy way to wrap a value in [`UserData`] and let Lua call methods on it.
///
/// Creates a metatable with the `__index` field set to another table. Adding methods via
/// [`UserMethods::add`] or [`UserMethods::add_write`] adds fields to this `__index` table which
/// are callbakcs that take a wrapped userdata as their first parameter. If a `UserData` has its
/// metatable set to the constructed metatable, then this allows calling methods on the userdata
/// from Lua like `ud:my_method()`.
///
/// You can "wrap" userdata by calling [`UserMethods::wrap`], which is a type-safe convenience
/// method that creates a new `UserData` of the expected type and sets its metatable.
///
/// This is a convenience utility meant for *simple cases only* and only allows for plain methods
/// that take the userdata as the first parameter and have simple arguments and returns. More
/// complex cases like handling operator metamethods (`__add`, `__sub`, etc), providing methods that
/// do not have simple returns (like `CallbackReturn::Yield`), or providing methods that need to
/// perform custom stack manipulation can be handled by doing the same thing that `UserMethods`
/// does internally: create a custom metatable with callbacks and downcast userdata inside those
/// callbacks manually.
#[derive(Collect)]
#[collect(no_drop, bound = "")]
pub struct UserMethods<'gc, U: for<'a> Rootable<'a>> {
    index: Table<'gc>,
    metatable: Table<'gc>,
    _marker: PhantomData<U>,
}

impl<'gc, U: for<'a> Rootable<'a>> Copy for UserMethods<'gc, U> {}

impl<'gc, U: for<'a> Rootable<'a>> Clone for UserMethods<'gc, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, U> UserMethods<'gc, U>
where
    U: for<'a> Rootable<'a>,
{
    /// Create a new `UserMethods` instance.
    pub fn new(ctx: Context<'gc>) -> Self {
        let index = Table::new(&ctx);
        let metatable = Table::new(&ctx);
        metatable.set(ctx, MetaMethod::Index, index).unwrap();
        Self {
            index,
            metatable,
            _marker: PhantomData,
        }
    }

    /// Returns the constructed metatable which will allow calling all added methods on any userdata
    /// that uses this table as its metatable.
    ///
    /// The `UserMethods` instance will always mutate this metatable *in-place*. This means that any
    /// method like [`UserMethods::add`] which changes this metatable will change it *everywhere*.
    ///
    /// So, if you create a [`UserData`] value with [`UserMethods::wrap`], then call something like
    /// `UserMethods::add` on the same `UserMethods` instance afterwards, this will add / change
    /// available methods for the *already created* `UserData`.
    pub fn metatable(self) -> Table<'gc> {
        self.metatable
    }
}

impl<'gc, U> UserMethods<'gc, U>
where
    U: for<'a> Rootable<'a> + 'static,
    for<'a> Root<'a, U>: Sized,
{
    /// Add a method to the inner metatable.
    ///
    /// This adds a field to an inner `mt.__index` table which is a constructed callback that calls
    /// the provided `method`.
    ///
    /// The inner callback accepts a [`UserData`] as its first parameter and downcasts it to the
    /// expected type, then produces the `A` args type by consuming the *rest* of the provided
    /// arguments. If the wrong type is given as the first userdata parameter (either it is not a
    /// `UserData` or the downcast fails) or mismatched types are given for the `A` args type, then
    /// the method call will error.
    pub fn add<F, A, R>(self, name: &'static str, ctx: Context<'gc>, method: F) -> bool
    where
        F: Fn(&Root<'gc, U>, Context<'gc>, Execution<'gc, '_>, A) -> Result<R, Error<'gc>>
            + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        let callback = Callback::from_fn(&ctx, move |ctx, exec, mut stack| {
            let userdata: UserData = stack.from_front(ctx)?;
            let args: A = stack.consume(ctx)?;
            let this = userdata.downcast::<U>()?;
            let ret = method(&this, ctx, exec, args)?;
            stack.replace(ctx, ret);
            Ok(CallbackReturn::Return)
        });

        !self.index.set_field(ctx, name, callback).is_nil()
    }

    /// A version of [`UserMethods::add`] that invokes a write barrier on the held userdata.
    ///
    /// This method does the same thing as `UserData::add`, except the constructed callback will
    /// downcast the userdata with [`UserData::downcast_write`] and will pass a `[barrier::Write]`
    /// wrapper to `method`.
    ///
    /// This is useful if the added method needs to safely use interior mutability in the wrapped
    /// userdata.
    pub fn add_write<F, A, R>(self, name: &'static str, ctx: Context<'gc>, method: F) -> bool
    where
        F: Fn(
                &barrier::Write<Root<'gc, U>>,
                Context<'gc>,
                Execution<'gc, '_>,
                A,
            ) -> Result<R, Error<'gc>>
            + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        let callback = Callback::from_fn(&ctx, move |ctx, exec, mut stack| {
            let userdata: UserData = stack.from_front(ctx)?;
            let args: A = stack.consume(ctx)?;
            let mut this = userdata.downcast_write::<U>(&ctx)?;
            let ret = method(&mut this, ctx, exec, args)?;
            stack.replace(ctx, ret);
            Ok(CallbackReturn::Return)
        });

        !self.index.set_field(ctx, name, callback).is_nil()
    }
}

impl<'gc, U> UserMethods<'gc, U>
where
    U: for<'a> Rootable<'a> + 'static,
    for<'a> Root<'a, U>: Sized + Collect,
{
    /// A type-safe convenience method that creates a new [`UserData`] value and sets its metatable
    /// to the one returned from [`UserMethods::metatable`].
    ///
    /// This is safer to use than creating the `UserData` yourself with [`UserData::new`] and
    /// setting the metatable manually, because this method only accepts values that match the
    /// expected userdata type.
    pub fn wrap(self, ctx: Context<'gc>, ud: Root<'gc, U>) -> UserData<'gc> {
        let ud = UserData::new::<U>(&ctx, ud);
        ud.set_metatable(&ctx, Some(self.metatable()));
        ud
    }
}

/// A simplified version of [`UserMethods`] that only works for `'static` userdata types.
///
/// In the same way that [`UserData`] provides a simplified `xxx_static` API, this allows you to
/// create simple userdata methods without needing to deal with the [`trait@Rootable`] trait. It
/// is identical to using `Static<U>` as the `U` projection type in [`UserMethods`] in the same way
/// that the `UserData` static API is equivalent to the normal API with the [`Static`] wrapper.
#[derive(Collect)]
#[collect(no_drop, bound = "")]
pub struct StaticUserMethods<'gc, U: 'static>(UserMethods<'gc, Static<U>>);

impl<'gc, U: 'static> Copy for StaticUserMethods<'gc, U> {}

impl<'gc, U: 'static> Clone for StaticUserMethods<'gc, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, U: 'static> StaticUserMethods<'gc, U> {
    /// Create a new `StaticUserMethods` instance.
    pub fn new(ctx: Context<'gc>) -> Self {
        Self(UserMethods::new(ctx))
    }

    /// Add a method to the inner metatable.
    pub fn add<F, A, R>(self, name: &'static str, ctx: Context<'gc>, method: F) -> bool
    where
        F: Fn(&U, Context<'gc>, Execution<'gc, '_>, A) -> Result<R, Error<'gc>> + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        self.0.add(name, ctx, move |root, ctx, exec, args| {
            method(&root.0, ctx, exec, args)
        })
    }

    /// Returns the constructed metatable which will allow calling all added methods on any userdata
    /// that uses this table as its metatable.
    pub fn metatable(self) -> Table<'gc> {
        self.0.metatable()
    }

    /// A type-safe convenience method that creates a new [`UserData`] value and sets its metatable
    /// to the one returned from [`StaticUserMethods::metatable`].
    pub fn wrap(self, ctx: Context<'gc>, ud: U) -> UserData<'gc> {
        self.0.wrap(ctx, Static(ud))
    }
}
