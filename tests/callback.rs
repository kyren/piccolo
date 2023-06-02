use gc_arena::Collect;
use piccolo::{
    compile, AnyCallback, AnySequence, CallbackReturn, Closure, Error, Function, IntoValue, Lua,
    Sequence, SequencePoll, StaticError, String, Thread, Value,
};

#[test]
fn callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |_, stack| {
            stack.push_back(Value::Integer(42));
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
                &br#"
                    local a, b, c = callback(1, 2)
                    return a == 1 and b == 2 and c == 42
                "#[..],
            )?,
            Some(ctx.state.globals),
        )?;

        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    assert!(lua.run_thread::<bool>(&thread)?);
    Ok(())
}

#[test]
fn tail_call_trivial_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |_, stack| {
            stack.push_back(Value::Integer(3));
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
                &br#"
                    return callback(1, 2)
                "#[..],
            )?,
            Some(ctx.state.globals),
        )?;

        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    assert_eq!(lua.run_thread::<(i64, i64, i64)>(&thread)?, (1, 2, 3));
    Ok(())
}

#[test]
fn loopy_callback() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |ctx, _| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct Cont(i64);

            impl<'gc> Sequence<'gc> for Cont {
                fn poll(
                    &mut self,
                    _ctx: piccolo::Context<'gc>,
                    stack: &mut piccolo::Stack<'gc>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    stack.push_back(self.0.into());
                    self.0 += 1;
                    if self.0 > 6 {
                        Ok(SequencePoll::Return)
                    } else {
                        Ok(SequencePoll::Pending)
                    }
                }
            }

            Ok(CallbackReturn::TailCall(
                AnyCallback::from_fn(&ctx, |_, stack| {
                    stack.push_back(3.into());
                    Ok(CallbackReturn::Yield(None))
                })
                .into(),
                Some(AnySequence::new(Cont(4))),
            ))
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
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
            Some(ctx.state.globals),
        )?;

        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);
    lua.try_run(|ctx| {
        assert!(ctx
            .state
            .registry
            .fetch(&thread)
            .take_return::<bool>(ctx)??);
        Ok(())
    })
}

#[test]
fn yield_sequence() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |ctx, stack| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct Cont(i8);

            impl<'gc> Sequence<'gc> for Cont {
                fn poll(
                    &mut self,
                    ctx: piccolo::Context<'gc>,
                    stack: &mut piccolo::Stack<'gc>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    match self.0 {
                        0 => {
                            let (a, b): (i32, i32) = stack.consume(ctx)?;
                            assert_eq!((a, b), (5, 6));
                            stack.extend([Value::Integer(7), Value::Integer(8)]);
                            self.0 = 1;
                            Ok(SequencePoll::Yield { is_tail: false })
                        }
                        1 => {
                            let (a, b): (i32, i32) = stack.consume(ctx)?;
                            assert_eq!((a, b), (9, 10));
                            stack.extend([Value::Integer(11), Value::Integer(12)]);
                            self.0 = 2;
                            Ok(SequencePoll::Return)
                        }
                        _ => unreachable!(),
                    }
                }
            }

            let (a, b): (i32, i32) = stack.consume(ctx)?;
            assert_eq!((a, b), (1, 2));
            stack.extend([Value::Integer(3), Value::Integer(4)]);
            Ok(CallbackReturn::Yield(Some(Cont(0).into())))
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
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
            Some(ctx.state.globals),
        )?;

        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.run_thread(&thread)
}

#[test]
fn resume_with_err() {
    let mut lua = Lua::new();

    let thread = lua.run(|ctx| {
        let callback = AnyCallback::from_fn(&ctx, |ctx, stack| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct Cont;

            impl<'gc> Sequence<'gc> for Cont {
                fn poll(
                    &mut self,
                    _ctx: piccolo::Context<'gc>,
                    _stack: &mut piccolo::Stack<'gc>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    panic!("did not error");
                }

                fn error(
                    &mut self,
                    ctx: piccolo::Context<'gc>,
                    _error: Error<'gc>,
                    _stack: &mut piccolo::Stack<'gc>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    Err("a different error".into_value(ctx).into())
                }
            }

            assert!(stack.len() == 1);
            assert_eq!(stack.consume::<String>(ctx)?, "resume");
            stack.replace(ctx, "return");
            Ok(CallbackReturn::Yield(Some(Cont.into())))
        });

        let thread = Thread::new(&ctx);
        thread
            .start_suspended(&ctx, Function::Callback(callback))
            .unwrap();

        thread.resume(ctx, "resume").unwrap();

        ctx.state.registry.stash(&ctx, thread)
    });

    lua.finish_thread(&thread);

    lua.run(|ctx| {
        let thread = ctx.state.registry.fetch(&thread);
        assert!(thread.take_return::<String>(ctx).unwrap().unwrap() == "return");
        thread
            .resume_err(&ctx, "an error".into_value(ctx).into())
            .unwrap();
    });

    lua.finish_thread(&thread);

    lua.run(|ctx| {
        let thread = ctx.state.registry.fetch(&thread);
        match thread.take_return::<()>(ctx).unwrap() {
            Err(Error::Lua(val)) => {
                assert!(matches!(val.0, Value::String(s) if s == "a different error"))
            }
            _ => panic!(),
        }
    });
}
