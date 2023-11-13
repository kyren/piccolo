use piccolo::{AnyCallback, CallbackReturn, Executor, Function, Lua, StaticError, Variadic};

#[test]
fn function_compose_bind() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    let executor = lua.try_run(|ctx| {
        let composed_functions = Function::compose(
            &ctx,
            [
                AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                    let i: Variadic<Vec<i64>> = stack.consume(ctx)?;
                    stack.replace(ctx, i.into_iter().sum::<i64>());
                    Ok(CallbackReturn::Return)
                })
                .into(),
                AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                    let i: i64 = stack.consume(ctx)?;
                    stack.replace(ctx, i * 2);
                    Ok(CallbackReturn::Return)
                })
                .into(),
                AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                    let i: i64 = stack.consume(ctx)?;
                    stack.replace(ctx, i + 1);
                    Ok(CallbackReturn::Return)
                })
                .into(),
                AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                    let i: i64 = stack.consume(ctx)?;
                    stack.replace(ctx, i * 3);
                    Ok(CallbackReturn::Return)
                })
                .into(),
            ],
        )
        .bind(&ctx, 1)
        .bind(&ctx, (2, 1));
        Ok(ctx
            .state
            .registry
            .stash(&ctx, Executor::start(ctx, composed_functions, 1)))
    })?;

    assert_eq!(lua.execute::<i64>(&executor)?, 33);
    Ok(())
}
