use piccolo::{
    AnyCallback, CallbackReturn, Closure, Executor, ExecutorMode, Fuel, Lua, StaticError,
};

#[test]
fn test_interrupt() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |_, exec, _| {
            exec.fuel.interrupt();
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let executor = lua.try_run(|ctx| {
        let closure = Closure::load(ctx, &b"callback(); abort()"[..])?;
        Ok(ctx
            .state
            .registry
            .stash(&ctx, Executor::start(ctx, closure.into(), ())))
    })?;

    lua.run(|ctx| {
        let executor = ctx.state.registry.fetch(&executor);
        let mut fuel = Fuel::with(i32::MAX);
        assert!(!executor.step(ctx, &mut fuel));
        assert!(fuel.is_interrupted());
        assert!(executor.mode() == ExecutorMode::Normal)
    });

    Ok(())
}
