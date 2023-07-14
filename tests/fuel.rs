use piccolo::{AnyCallback, CallbackReturn, Closure, Lua, StaticError, Thread, ThreadMode};

#[test]
fn test_interrupt() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |_, fuel, _| {
            fuel.interrupt();
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::load(ctx, &b"callback(); abort()"[..])?;
        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);

    lua.run(|ctx| {
        assert!(ctx.state.registry.fetch(&thread).mode() == ThreadMode::Normal);
    });

    Ok(())
}
