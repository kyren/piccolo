use gc_arena::{Collect, Rootable};
use piccolo::{AnyCallback, AnyUserData, CallbackReturn, Context, Singleton, Table};

#[derive(Collect)]
#[collect(no_drop)]
struct UnitSingleton<'gc>(AnyUserData<'gc>);

impl<'gc> Singleton<'gc> for UnitSingleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self {
        let ud = AnyUserData::new_static(&ctx, ());
        let mt = Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "unit");
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        UnitSingleton(ud)
    }
}

pub fn unit<'gc>(ctx: Context<'gc>) -> AnyUserData<'gc> {
    ctx.singleton::<Rootable![UnitSingleton<'_>]>().0
}

pub fn is_unit<'gc>(ud: AnyUserData<'gc>) -> bool {
    ud.is_static::<()>()
}

pub struct None;

#[derive(Collect)]
#[collect(no_drop)]
struct NoneSingleton<'gc>(AnyUserData<'gc>);

impl<'gc> Singleton<'gc> for NoneSingleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self {
        let ud = AnyUserData::new_static(&ctx, None);
        let mt = Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "none");
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        NoneSingleton(ud)
    }
}

pub fn none<'gc>(ctx: Context<'gc>) -> AnyUserData<'gc> {
    ctx.singleton::<Rootable![NoneSingleton<'_>]>().0
}

pub fn is_none<'gc>(ud: AnyUserData<'gc>) -> bool {
    ud.is_static::<None>()
}

pub fn set_globals<'gc>(ctx: Context<'gc>) {
    ctx.set_global("unit", unit(ctx)).unwrap();
    ctx.set_global("none", none(ctx)).unwrap();
}
