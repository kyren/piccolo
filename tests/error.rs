use luster::{
    compile, sequence, Closure, Error, Function, Lua, SequenceExt, SequenceResultExt, StaticError,
    ThreadSequence,
};

#[test]
fn error_unwind() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|root| {
        sequence::from_fn_with(root, |mc, root| {
            Ok(Closure::new(
                mc,
                compile(
                    mc,
                    root.interned_strings,
                    &br#"
                        function do_error()
                            error('test error')
                        end

                        do_error()
                    "#[..],
                )?,
                Some(root.globals),
            )?)
        })
        .and_chain_with(root, |mc, root, closure| {
            Ok(ThreadSequence::call_function(
                mc,
                root.main_thread,
                Function::Closure(closure),
                &[],
            )?
            .map(|res| match res {
                Err(Error::RuntimeError(_)) => Ok(()),
                _ => panic!(),
            })
            .and_chain_with((root, closure), |mc, (root, closure), _| {
                Ok(ThreadSequence::call_function(
                    mc,
                    root.main_thread,
                    Function::Closure(closure),
                    &[],
                )?)
            })
            .map(|res| match res {
                Err(Error::RuntimeError(_)) => Ok(()),
                _ => panic!(),
            }))
        })
        .map_err(Error::to_static)
        .boxed()
    })?;

    Ok(())
}
