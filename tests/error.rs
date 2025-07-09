use piccolo::{Callback, Closure, Error, Executor, ExternError, Lua, Value};
use thiserror::Error;

#[test]
fn error_unwind() -> Result<(), ExternError> {
    let mut lua = Lua::core();

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(
            ctx,
            None,
            &br#"
                function do_error()
                    error('test error')
                end

                do_error()
            "#[..],
        )?;
        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.finish(&executor).unwrap();
    lua.try_enter(|ctx| {
        match ctx.fetch(&executor).take_result::<()>(ctx)? {
            Err(Error::Lua(lua_error)) => {
                assert!(matches!(lua_error.value, Value::String(s) if s == "test error"))
            }
            _ => panic!("wrong error returned"),
        }
        Ok(())
    })
}

#[test]
fn error_tostring() -> Result<(), ExternError> {
    let mut lua = Lua::core();

    #[derive(Debug, Error)]
    #[error("test error")]
    struct TestError;

    let executor = lua.try_enter(|ctx| {
        let callback = Callback::from_fn(&ctx, |_, _, _| Err(TestError.into()));
        ctx.set_global("callback", callback);

        let closure = Closure::load(
            ctx,
            None,
            &br#"
                local r, e = pcall(callback)
                assert(not r)
                assert(tostring(e) == "test error")
            "#[..],
        )?;

        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.execute(&executor)
}
