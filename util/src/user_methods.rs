use std::marker::PhantomData;

use gc_arena::{barrier, Collect, Mutation, Root, Rootable};
use piccolo::{
    Callback, CallbackReturn, Context, Error, Execution, FromMultiValue, IntoMultiValue, IntoValue,
    MetaMethod, Table, UserData, Value,
};

#[derive(Collect)]
#[collect(no_drop, bound = "")]
pub struct UserMethods<'gc, U: for<'a> Rootable<'a>> {
    table: Table<'gc>,
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
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            table: Table::new(mc),
            _marker: PhantomData,
        }
    }

    pub fn metatable(self, ctx: Context<'gc>) -> Table<'gc> {
        let metatable = Table::new(&ctx);
        metatable.set(ctx, MetaMethod::Index, self.table).unwrap();
        metatable
    }
}

impl<'gc, U> UserMethods<'gc, U>
where
    U: for<'a> Rootable<'a>,
    for<'a> Root<'a, U>: Sized,
{
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

        !self.table.set(ctx, name, callback).unwrap().is_nil()
    }

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

        !self.table.set(ctx, name, callback).unwrap().is_nil()
    }
}

impl<'gc, U> UserMethods<'gc, U>
where
    U: for<'a> Rootable<'a>,
    for<'a> Root<'a, U>: Sized + Collect,
{
    pub fn wrap(self, ctx: Context<'gc>, ud: Root<'gc, U>) -> UserData<'gc> {
        let ud = UserData::new::<U>(&ctx, ud);
        ud.set_metatable(&ctx, Some(self.metatable(ctx)));
        ud
    }
}

impl<'gc, U: for<'a> Rootable<'a>> IntoValue<'gc> for UserMethods<'gc, U> {
    fn into_value(self, _: Context<'gc>) -> Value<'gc> {
        self.table.into()
    }
}

#[derive(Collect)]
#[collect(no_drop, bound = "")]
pub struct StaticUserMethods<'gc, U: 'static> {
    table: Table<'gc>,
    _marker: PhantomData<U>,
}

impl<'gc, U: 'static> Copy for StaticUserMethods<'gc, U> {}

impl<'gc, U: 'static> Clone for StaticUserMethods<'gc, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, U: 'static> StaticUserMethods<'gc, U> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            table: Table::new(mc),
            _marker: PhantomData,
        }
    }

    pub fn add<F, A, R>(self, name: &'static str, ctx: Context<'gc>, method: F) -> bool
    where
        F: Fn(&U, Context<'gc>, Execution<'gc, '_>, A) -> Result<R, Error<'gc>> + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        let callback = Callback::from_fn(&ctx, move |ctx, exec, mut stack| {
            let userdata: UserData = stack.from_front(ctx)?;
            let args: A = stack.consume(ctx)?;
            let this = userdata.downcast_static::<U>()?;
            let ret = method(&this, ctx, exec, args)?;
            stack.replace(ctx, ret);
            Ok(CallbackReturn::Return)
        });

        !self.table.set(ctx, name, callback).unwrap().is_nil()
    }

    pub fn metatable(self, ctx: Context<'gc>) -> Table<'gc> {
        let metatable = Table::new(&ctx);
        metatable.set(ctx, MetaMethod::Index, self.table).unwrap();
        metatable
    }

    pub fn wrap(self, ctx: Context<'gc>, ud: U) -> UserData<'gc> {
        let ud = UserData::new_static(&ctx, ud);
        ud.set_metatable(&ctx, Some(self.metatable(ctx)));
        ud
    }
}

impl<'gc, U: 'static> IntoValue<'gc> for StaticUserMethods<'gc, U> {
    fn into_value(self, _: Context<'gc>) -> Value<'gc> {
        self.table.into()
    }
}
