use piccolo::{
    compile, AnyCallback, AnyContinuation, CallbackReturn, Closure, Error, Function, IntoValue,
    Lua, RuntimeError, StaticError, String, Thread, Value,
};

#[test]
fn callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = AnyCallback::from_fn(mc, |_, stack| {
            stack.push(Value::Integer(42));
            Ok(CallbackReturn::Return.into())
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                root.strings,
                &br#"
                    local a, b, c = callback(1, 2)
                    return a == 1 and b == 2 and c == 42
                "#[..],
            )?,
            Some(root.globals),
        )?;

        root.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    assert!(lua.run_main_thread::<bool>()?);
    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = AnyCallback::from_fn(mc, |_, stack| {
            stack.push(Value::Integer(3));
            Ok(CallbackReturn::Return.into())
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                root.strings,
                &br#"
                    return callback(1, 2)
                "#[..],
            )?,
            Some(root.globals),
        )?;

        root.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    assert_eq!(lua.run_main_thread::<(i64, i64, i64)>()?, (1, 2, 3));
    Ok(())
}

#[test]
fn loopy_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = AnyCallback::from_fn(mc, |mc, _| {
            Ok(CallbackReturn::TailCall(
                AnyCallback::from_fn(mc, |_, stack| {
                    stack.push(3.into());
                    Ok(CallbackReturn::Yield(None).into())
                })
                .into(),
                Some(AnyContinuation::from_ok_fn(mc, |mc, stack| {
                    stack.push(4.into());
                    Ok(CallbackReturn::TailCall(
                        AnyCallback::from_fn(mc, |_, _| Ok(CallbackReturn::Return.into())).into(),
                        Some(AnyContinuation::from_ok_fn(mc, |mc, stack| {
                            stack.push(5.into());
                            Ok(CallbackReturn::TailCall(
                                AnyCallback::from_fn(mc, |_, stack| {
                                    stack.push(6.into());
                                    Ok(CallbackReturn::Return.into())
                                })
                                .into(),
                                None,
                            )
                            .into())
                        })),
                    )
                    .into())
                })),
            )
            .into())
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                root.strings,
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

        root.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.finish_main_thread();

    lua.try_run(|mc, root| {
        assert!(root.main_thread.take_return::<bool>(mc)??);
        Ok(())
    })
}

#[test]
fn yield_continuation() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let callback = AnyCallback::from_fn(mc, |mc, stack| {
            assert!(matches!(
                stack.as_slice(),
                &[Value::Integer(1), Value::Integer(2)]
            ));
            stack.clear();
            stack.extend([Value::Integer(3), Value::Integer(4)]);
            Ok(
                CallbackReturn::Yield(Some(AnyContinuation::from_ok_fn(mc, |mc, stack| {
                    assert!(matches!(
                        stack.as_slice(),
                        &[Value::Integer(5), Value::Integer(6)]
                    ));
                    stack.clear();
                    stack.extend([Value::Integer(7), Value::Integer(8)]);
                    Ok(
                        CallbackReturn::Yield(Some(AnyContinuation::from_ok_fn(mc, |_, stack| {
                            assert!(matches!(
                                stack.as_slice(),
                                &[Value::Integer(9), Value::Integer(10)]
                            ));
                            stack.clear();
                            stack.extend([Value::Integer(11), Value::Integer(12)]);
                            Ok(CallbackReturn::Return.into())
                        })))
                        .into(),
                    )
                })))
                .into(),
            )
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                root.strings,
                &br#"
                    local co = coroutine.create(callback)

                    local e, r1, r2 = coroutine.resume(co, 1, 2)
                    assert(e == true and r1 == 3 and r2 == 4)
                    assert(coroutine.status(co) == "suspended")

                    local e, r1, r2 = coroutine.resume(co, 5, 6)
                    assert(e == true and r1 == 7 and r2 == 8)
                    assert(coroutine.status(co) == "suspended")

                    local e, r1, r2 = coroutine.resume(co, 9, 10)
                    assert(e == true and r1 == 11 and r2 == 12)
                    assert(coroutine.status(co) == "dead")
                "#[..],
            )?,
            Some(root.globals),
        )?;
        root.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.run_main_thread()
}

#[test]
fn resume_with_err() {
    let mut lua = Lua::new();

    let thread = lua.run(|mc, root| {
        let callback = AnyCallback::from_fn(mc, |mc, stack| {
            assert!(stack.len() == 1);
            assert!(matches!(stack.as_slice(), [Value::String(s)] if s == "resume"));
            stack.clear();
            stack.push("return".into_value(mc));
            Ok(CallbackReturn::Yield(Some(AnyContinuation::from_fns(
                mc,
                |_, _| panic!("did not error"),
                |mc, _, _| Err(RuntimeError("a different error".into_value(mc)).into()),
            )))
            .into())
        });

        let thread = Thread::new(mc);
        thread
            .start_suspended(mc, Function::Callback(callback))
            .unwrap();

        thread.resume(mc, "resume").unwrap();

        root.registry.stash(mc, thread)
    });

    lua.finish_thread(&thread);

    lua.run(|mc, root| {
        let thread = root.registry.fetch(&thread);
        assert!(thread.take_return::<String>(mc).unwrap().unwrap() == "return");
        thread
            .resume_err(mc, RuntimeError("an error".into_value(mc)).into())
            .unwrap();
    });

    lua.finish_thread(&thread);

    lua.run(|mc, root| {
        let thread = root.registry.fetch(&thread);
        match thread.take_return::<()>(mc).unwrap() {
            Err(Error::RuntimeError(RuntimeError(val))) => {
                assert!(matches!(val, Value::String(s) if s == "a different error"))
            }
            _ => panic!(),
        }
    });
}
