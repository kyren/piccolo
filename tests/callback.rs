use luster::{
    compile, sequence_fn, Callback, CallbackResult, Closure, Error, Function, Lua, SequenceExt,
    StaticError, String, Value,
};

#[test]
fn callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|_| {
        Box::new(
            sequence_fn(|mc, lc| -> Result<(), Error> {
                let callback = Callback::new_immediate(mc, |_, args| {
                    let mut ret = args.to_vec();
                    ret.push(Value::Integer(42));
                    Ok(CallbackResult::Return(ret))
                });
                lc.globals
                    .set(mc, String::new_static(b"callback"), callback)?;
                Ok(())
            })
            .and_then(|mc, lc, _| {
                Ok(Closure::new(
                    mc,
                    compile(
                        mc,
                        lc.interned_strings,
                        &br#"
                            local a, b, c = callback(1, 2)
                            return a == 1 and b == 2 and c == 42
                        "#[..],
                    )?,
                    Some(lc.globals),
                )?)
            })
            .and_then(|mc, lc, closure| {
                lc.main_thread
                    .call(mc, Function::Closure(closure), &[])
                    .unwrap()
            })
            .map(|b| assert_eq!(b, vec![Value::Boolean(true)]))
            .map_err(Error::to_static),
        )
    })?;

    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|_| {
        Box::new(
            sequence_fn(|mc, lc| -> Result<(), Error> {
                let callback = Callback::new_immediate(mc, |_, args| {
                    let mut ret = args.to_vec();
                    ret.push(Value::Integer(3));
                    Ok(CallbackResult::Return(ret))
                });
                lc.globals
                    .set(mc, String::new_static(b"callback"), callback)?;
                Ok(())
            })
            .and_then(|mc, lc, _| {
                Ok(Closure::new(
                    mc,
                    compile(
                        mc,
                        lc.interned_strings,
                        &br#"
                            return callback(1, 2)
                        "#[..],
                    )?,
                    Some(lc.globals),
                )?)
            })
            .and_then(|mc, lc, closure| {
                lc.main_thread
                    .call(mc, Function::Closure(closure), &[])
                    .unwrap()
            })
            .map(|b| {
                assert_eq!(
                    b,
                    vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
                )
            })
            .map_err(Error::to_static),
        )
    })?;

    Ok(())
}
