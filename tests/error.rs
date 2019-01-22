use luster::{compile, sequence_fn, Closure, Error, Lua, SequenceExt};

#[test]
fn error_unwind() -> Result<(), Box<Error>> {
    let mut lua = Lua::new();
    lua.sequence(|_| {
        Box::new(
            sequence_fn(|mc, lc| -> Result<_, Error> {
                Ok(Closure::new(
                    mc,
                    compile(
                        mc,
                        lc.interned_strings,
                        &br#"
                            function do_error()
                                error('test error')
                            end

                            do_error()
                        "#[..],
                    )?,
                    Some(lc.globals),
                )?)
            })
            .and_then(|mc, lc, closure| {
                lc.main_thread
                    .run_closure(mc, closure, &[], 64)
                    .then(|_, _, res| match res {
                        Err(Error::RuntimeError(_)) => Ok(()),
                        _ => panic!(),
                    })
                    .and_then_with(closure, |mc, lc, closure, _| {
                        lc.main_thread.run_closure(mc, closure, &[], 64)
                    })
                    .then(|_, _, res| match res {
                        Err(Error::RuntimeError(_)) => Ok(()),
                        _ => panic!(),
                    })
            }),
        )
    })?;

    Ok(())
}
