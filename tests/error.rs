mod sizes;

use piccolo::{compile, Closure, Error, Lua, StaticError, Value};

#[test]
fn error_unwind() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, state| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                state.strings,
                &br#"
                    function do_error()
                        error('test error')
                    end

                    do_error()
                "#[..],
            )?,
            Some(state.globals),
        )?;
        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.finish_main_thread();
    lua.try_run(|mc, state| {
        match state.main_thread.take_return::<()>(mc)? {
            Err(Error::RuntimeError(Value::String(s))) => assert!(s == "test error"),
            _ => panic!(),
        }
        Ok(())
    })
}
