use piccolo::{
    AnyCallback, CallbackReturn, Closure, Executor, ExecutorMode, Fuel, Lua, StaticError,
};

#[test]
fn test_interrupt() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    lua.try_enter(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |_, mut exec, _| {
            exec.fuel().interrupt();
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(ctx, None, &b"callback(); abort()"[..])?;
        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.enter(|ctx| {
        let executor = ctx.fetch(&executor);
        let mut fuel = Fuel::with(i32::MAX);
        assert!(!executor.step(ctx, &mut fuel));
        assert!(fuel.is_interrupted());
        assert!(executor.mode() == ExecutorMode::Normal)
    });

    Ok(())
}
