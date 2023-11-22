use gc_arena::{Collect, Rootable};
use piccolo as lua;

#[derive(Collect)]
#[collect(no_drop)]
struct UnitSingleton<'gc>(lua::AnyUserData<'gc>);

impl<'gc> lua::Singleton<'gc> for UnitSingleton<'gc> {
    fn create(ctx: lua::Context<'gc>) -> Self {
        let ud = lua::AnyUserData::new_static(&ctx, ());
        let mt = lua::Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            lua::AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "unit");
                Ok(lua::CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        UnitSingleton(ud)
    }
}

pub fn unit<'gc>(ctx: lua::Context<'gc>) -> lua::AnyUserData<'gc> {
    ctx.state
        .registry
        .singleton::<Rootable![UnitSingleton<'_>]>(ctx)
        .0
}

pub fn is_unit<'gc>(ud: lua::AnyUserData<'gc>) -> bool {
    ud.is_static::<()>()
}

pub struct None;

#[derive(Collect)]
#[collect(no_drop)]
struct NoneSingleton<'gc>(lua::AnyUserData<'gc>);

impl<'gc> lua::Singleton<'gc> for NoneSingleton<'gc> {
    fn create(ctx: lua::Context<'gc>) -> Self {
        let ud = lua::AnyUserData::new_static(&ctx, None);
        let mt = lua::Table::new(&ctx);
        mt.set(
            ctx,
            "__tostring",
            lua::AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                stack.replace(ctx, "none");
                Ok(lua::CallbackReturn::Return)
            }),
        )
        .unwrap();
        ud.set_metatable(&ctx, Option::Some(mt));
        NoneSingleton(ud)
    }
}

pub fn none<'gc>(ctx: lua::Context<'gc>) -> lua::AnyUserData<'gc> {
    ctx.state
        .registry
        .singleton::<Rootable![NoneSingleton<'_>]>(ctx)
        .0
}

pub fn is_none<'gc>(ud: lua::AnyUserData<'gc>) -> bool {
    ud.is_static::<None>()
}

pub fn set_globals<'gc>(ctx: lua::Context<'gc>) {
    ctx.state.globals.set(ctx, "unit", unit(ctx)).unwrap();
    ctx.state.globals.set(ctx, "none", none(ctx)).unwrap();
}
