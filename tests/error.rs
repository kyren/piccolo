use luster::{
    compile, sequence_fn, Closure, Error, Function, Lua, SequenceExt, StaticError, ThreadSequence,
};

#[test]
fn error_unwind() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|_| {
        sequence_fn(|mc, lc| {
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
            Ok(
                ThreadSequence::call_function(mc, lc.main_thread, Function::Closure(closure), &[])?
                    .then(|_, _, res| match res {
                        Err(Error::RuntimeError(_)) => Ok(()),
                        _ => panic!(),
                    })
                    .and_then_with(closure, |mc, lc, closure, _| {
                        Ok(ThreadSequence::call_function(
                            mc,
                            lc.main_thread,
                            Function::Closure(closure),
                            &[],
                        )?)
                    })
                    .flatten()
                    .then(|_, _, res| match res {
                        Err(Error::RuntimeError(_)) => Ok(()),
                        _ => panic!(),
                    }),
            )
        })
        .flatten()
        .map_ok(|_| ())
        .map_err(Error::to_static)
        .boxed()
    })?;

    Ok(())
}
