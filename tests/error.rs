use deimos::{compile, Closure, Function, Lua, StaticError};

#[test]
fn error_unwind() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    let function = lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                &br#"
                    function do_error()
                        error('test error')
                    end

                    do_error()
                "#[..],
            )?,
            Some(root.globals),
        )?;
        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    assert!(matches!(
        lua.run_function(&function, &[]),
        Err(StaticError::RuntimeError(_))
    ));

    assert!(matches!(
        lua.run_function(&function, &[]),
        Err(StaticError::RuntimeError(_))
    ));

    Ok(())
}
