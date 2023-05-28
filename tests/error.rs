mod sizes;

use piccolo::{compile, error::LuaError, Closure, Error, Lua, StaticError, Thread, Value};

#[test]
fn error_unwind() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
                &br#"
                    function do_error()
                        error('test error')
                    end

                    do_error()
                "#[..],
            )?,
            Some(ctx.state.globals),
        )?;
        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);
    lua.try_run(|ctx| {
        match ctx.state.registry.fetch(&thread).take_return::<()>(ctx)? {
            Err(Error::Lua(LuaError(Value::String(s)))) => assert!(s == "test error"),
            _ => panic!(),
        }
        Ok(())
    })
}
