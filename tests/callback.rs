use piccolo::{
    compile, AnyCallback, AnyContinuation, CallbackReturn, Closure, Error, Function, IntoValue,
    Lua, StaticError, String, Thread, Value,
};

#[test]
fn callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, state| {
        let callback = AnyCallback::from_fn(mc, |_, stack| {
            stack.push_back(Value::Integer(42));
            Ok(CallbackReturn::Return)
        });
        state.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, state| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                state.strings,
                &br#"
                    local a, b, c = callback(1, 2)
                    return a == 1 and b == 2 and c == 42
                "#[..],
            )?,
            Some(state.globals),
        )?;

        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    assert!(lua.run_main_thread::<bool>()?);
    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, state| {
        let callback = AnyCallback::from_fn(mc, |_, stack| {
            stack.push_back(Value::Integer(3));
            Ok(CallbackReturn::Return)
        });
        state.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, state| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                state.strings,
                &br#"
                    return callback(1, 2)
                "#[..],
            )?,
            Some(state.globals),
        )?;

        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    assert_eq!(lua.run_main_thread::<(i64, i64, i64)>()?, (1, 2, 3));
    Ok(())
}

#[test]
fn loopy_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, state| {
        let callback = AnyCallback::from_fn(mc, |mc, _| {
            Ok(CallbackReturn::TailCall(
                AnyCallback::from_fn(mc, |_, stack| {
                    stack.push_back(3.into());
                    Ok(CallbackReturn::Yield(None))
                })
                .into(),
                Some(AnyContinuation::from_ok_fn(mc, |mc, stack| {
                    stack.push_back(4.into());
                    Ok(CallbackReturn::TailCall(
                        AnyCallback::from_fn(mc, |_, _| Ok(CallbackReturn::Return)).into(),
                        Some(AnyContinuation::from_ok_fn(mc, |mc, stack| {
                            stack.push_back(5.into());
                            Ok(CallbackReturn::TailCall(
                                AnyCallback::from_fn(mc, |_, stack| {
                                    stack.push_back(6.into());
                                    Ok(CallbackReturn::Return)
                                })
                                .into(),
                                None,
                            ))
                        })),
                    ))
                })),
            ))
        });
        state.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, state| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                state.strings,
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
            Some(state.globals),
        )?;

        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.finish_main_thread();

    lua.try_run(|mc, state| {
        assert!(state.main_thread.take_return::<bool>(mc)??);
        Ok(())
    })
}

#[test]
fn yield_continuation() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|mc, state| {
        let callback = AnyCallback::from_fn(mc, |mc, stack| {
            let (a, b): (i32, i32) = stack.consume(mc)?;
            assert_eq!((a, b), (1, 2));
            stack.extend([Value::Integer(3), Value::Integer(4)]);
            Ok(CallbackReturn::Yield(Some(AnyContinuation::from_ok_fn(
                mc,
                |mc, stack| {
                    let (a, b): (i32, i32) = stack.consume(mc)?;
                    assert_eq!((a, b), (5, 6));
                    stack.extend([Value::Integer(7), Value::Integer(8)]);
                    Ok(CallbackReturn::Yield(Some(AnyContinuation::from_ok_fn(
                        mc,
                        |mc, stack| {
                            let (a, b): (i32, i32) = stack.consume(mc)?;
                            assert_eq!((a, b), (9, 10));
                            stack.extend([Value::Integer(11), Value::Integer(12)]);
                            Ok(CallbackReturn::Return)
                        },
                    ))))
                },
            ))))
        });
        state.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    lua.try_run(|mc, state| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                state.strings,
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
            Some(state.globals),
        )?;
        state.main_thread.start(mc, closure.into(), ())?;
        Ok(())
    })?;

    lua.run_main_thread()
}

#[test]
fn resume_with_err() {
    let mut lua = Lua::new();

    let thread = lua.run(|mc, state| {
        let callback = AnyCallback::from_fn(mc, |mc, stack| {
            assert!(stack.len() == 1);
            assert_eq!(stack.consume::<String>(mc)?, "resume");
            stack.replace(mc, "return");
            Ok(CallbackReturn::Yield(Some(AnyContinuation::from_fns(
                mc,
                |_, _| panic!("did not error"),
                |mc, _, _| Err("a different error".into_value(mc).into()),
            ))))
        });

        let thread = Thread::new(mc);
        thread
            .start_suspended(mc, Function::Callback(callback))
            .unwrap();

        thread.resume(mc, "resume").unwrap();

        state.registry.stash(mc, thread)
    });

    lua.finish_thread(&thread);

    lua.run(|mc, state| {
        let thread = state.registry.fetch(&thread);
        assert!(thread.take_return::<String>(mc).unwrap().unwrap() == "return");
        thread
            .resume_err(mc, "an error".into_value(mc).into())
            .unwrap();
    });

    lua.finish_thread(&thread);

    lua.run(|mc, state| {
        let thread = state.registry.fetch(&thread);
        match thread.take_return::<()>(mc).unwrap() {
            Err(Error::Lua(val)) => {
                assert!(matches!(val.0, Value::String(s) if s == "a different error"))
            }
            _ => panic!(),
        }
    });
}
