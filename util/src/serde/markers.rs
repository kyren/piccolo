use gc_arena::{Collect, Rootable};
use piccolo::{Callback, CallbackReturn, Context, Singleton, Table, UserData};

#[derive(Collect)]
#[collect(no_drop)]
struct UnitSingleton<'gc>(UserData<'gc>);

impl<'gc> Singleton<'gc> for UnitSingleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self {
        let ud = UserData::new_static(&ctx, ());
        let mt = Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "unit");
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        UnitSingleton(ud)
    }
}

pub fn unit<'gc>(ctx: Context<'gc>) -> UserData<'gc> {
    ctx.singleton::<Rootable![UnitSingleton<'_>]>().0
}

pub fn is_unit<'gc>(ud: UserData<'gc>) -> bool {
    ud.is_static::<()>()
}

pub struct None;

#[derive(Collect)]
#[collect(no_drop)]
struct NoneSingleton<'gc>(UserData<'gc>);

impl<'gc> Singleton<'gc> for NoneSingleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self {
        let ud = UserData::new_static(&ctx, None);
        let mt = Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "none");
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        NoneSingleton(ud)
    }
}

pub fn none<'gc>(ctx: Context<'gc>) -> UserData<'gc> {
    ctx.singleton::<Rootable![NoneSingleton<'_>]>().0
}

pub fn is_none<'gc>(ud: UserData<'gc>) -> bool {
    ud.is_static::<None>()
}

pub fn set_globals<'gc>(ctx: Context<'gc>) {
    ctx.set_global("unit", unit(ctx)).unwrap();
    ctx.set_global("none", none(ctx)).unwrap();
}
