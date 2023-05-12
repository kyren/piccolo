mod sizes;

use piccolo::{compile, Closure, Error, Lua, RuntimeError, StaticError, Value};

#[test]
fn error_unwind() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                root.strings,
                &br#"
                    function do_error()
                        error('test error')
                    end

                    do_error()
                "#[..],
            )?,
            Some(root.globals),
        )?;
        root.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.finish_main_thread();
    lua.try_run(|mc, root| {
        match root.main_thread.take_return::<()>(mc)? {
            Err(Error::RuntimeError(RuntimeError(Value::String(s)))) => assert!(s == "test error"),
            _ => panic!(),
        }
        Ok(())
    })
}
