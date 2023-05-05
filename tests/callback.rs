use piccolo::{
    compile, sequence, Callback, CallbackReturn, Closure, Error, Function, Lua, SequenceExt,
    StaticError, String, ThreadSequence, TrySequenceExt, Value,
};

#[test]
fn callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|root| {
        sequence::from_fn_with(root, |root, mc| {
            let callback = Callback::new_immediate(mc, |_, _, args| {
                let mut ret = args;
                ret.push(Value::Integer(42));
                Ok(CallbackReturn::Return(ret))
            });
            root.globals
                .set(mc, String::from_static(b"callback"), callback)?;
            Ok(())
        })
        .and_then_with(root, |root, mc, _| {
            Ok(Closure::new(
                mc,
                compile(
                    mc,
                    &br#"
                        local a, b, c = callback(1, 2)
                        return a == 1 and b == 2 and c == 42
                    "#[..],
                )?,
                Some(root.globals),
            )?)
        })
        .and_chain_with(root, |root, mc, closure| {
            Ok(ThreadSequence::call_function(
                mc,
                root.main_thread,
                Function::Closure(closure),
                &[],
            )?)
        })
        .map_ok(|b| assert_eq!(b, vec![Value::Boolean(true)]))
        .map_err(Error::to_static)
        .boxed()
    })?;

    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|root| {
        sequence::from_fn_with(root, |root, mc| {
            let callback = Callback::new_immediate(mc, |_, _, args| {
                let mut ret = args.to_vec();
                ret.push(Value::Integer(3));
                Ok(CallbackReturn::Return(ret))
            });
            root.globals
                .set(mc, String::from_static(b"callback"), callback)?;
            Ok(())
        })
        .and_then_with(root, |root, mc, _| {
            Ok(Closure::new(
                mc,
                compile(
                    mc,
                    &br#"
                        return callback(1, 2)
                    "#[..],
                )?,
                Some(root.globals),
            )?)
        })
        .and_chain_with(root, |root, mc, closure| {
            Ok(ThreadSequence::call_function(
                mc,
                root.main_thread,
                Function::Closure(closure),
                &[],
            )?)
        })
        .map_ok(|b| {
            assert_eq!(
                b,
                vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
            )
        })
        .map_err(Error::to_static)
        .boxed()
    })?;

    Ok(())
}
