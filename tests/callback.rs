use deimos::{
    compile, Callback, CallbackReturn, Closure, Continuation, Function, Lua, StaticError,
    StaticValue, Value,
};

#[test]
fn callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = Callback::new_immediate(mc, |_, args| {
            let mut ret = args;
            ret.push(Value::Integer(42));
            Ok(CallbackReturn::Return(ret))
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    let function = lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                &br#"
                    local a, b, c = callback(1, 2)
                    return a == 1 and b == 2 and c == 42
                "#[..],
            )?,
            Some(root.globals),
        )?;

        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    assert_eq!(
        lua.run_function(&function, &[])?,
        vec![StaticValue::Boolean(true)],
    );

    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = Callback::new_immediate(mc, |_, args| {
            let mut ret = args.to_vec();
            ret.push(Value::Integer(3));
            Ok(CallbackReturn::Return(ret))
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    let function = lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                &br#"
                    return callback(1, 2)
                "#[..],
            )?,
            Some(root.globals),
        )?;

        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    assert_eq!(
        lua.run_function(&function, &[])?,
        vec![
            StaticValue::Integer(1),
            StaticValue::Integer(2),
            StaticValue::Integer(3)
        ]
    );

    Ok(())
}

#[test]
fn loopy_callback() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = Callback::new_immediate(mc, |mc, args| {
            Ok(CallbackReturn::TailCall {
                function: Callback::new_immediate(mc, |_, mut args| {
                    args.push(3.into());
                    Ok(CallbackReturn::Yield(args))
                })
                .into(),
                args,
                continuation: Some(Continuation::new_immediate(|mc, args| {
                    let mut args = args?;
                    args.push(4.into());
                    Ok(CallbackReturn::TailCall {
                        function: Callback::new_immediate(mc, |_, args| {
                            Ok(CallbackReturn::Return(args))
                        })
                        .into(),
                        args,
                        continuation: Some(Continuation::new_immediate(|mc, args| {
                            let mut args = args?;
                            args.push(5.into());
                            Ok(CallbackReturn::TailCall {
                                function: Callback::new_immediate(mc, |_, mut args| {
                                    args.push(6.into());
                                    Ok(CallbackReturn::Return(args))
                                })
                                .into(),
                                args,
                                continuation: None,
                            })
                        })),
                    })
                })),
            })
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    let function = lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                &br#"
                    local function cotest()
                        return callback(1, 2)
                    end

                    local co = coroutine.create(cotest)

                    local e1, r1, r2, r3 = coroutine.resume(co)
                    local s1 = coroutine.status(co)
                    local e2, r4, r5, r6, r7, r8, r9 = coroutine.resume(co, r1, r2, r3)
                    local s2 = coroutine.status(co)

                    return
                        e1 == true and
                        r1 == 1 and r2 == 2 and r3 == 3 and
                        s1 == "suspended" and
                        e2 == true and
                        r4 == 1 and r5 == 2 and r6 == 3 and r7 == 4 and r8 == 5 and r9 == 6 and
                        s2 == "dead"
                "#[..],
            )?,
            Some(root.globals),
        )?;
        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    assert_eq!(
        lua.run_function(&function, &[])?,
        vec![StaticValue::Boolean(true)],
    );

    Ok(())
}
