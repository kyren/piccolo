use gc_arena::{Collect, MutationContext};

use crate::{
    AnyCallback, BadThreadMode, CallbackMode, CallbackReturn, IntoValue, Root, RuntimeError,
    Sequence, Table, Thread, ThreadMode, TypeError, Value, Variadic,
};

pub fn load_coroutine<'gc>(mc: MutationContext<'gc, '_>, _root: Root<'gc>, env: Table<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            "create",
            AnyCallback::from_fn(mc, |mc, stack| {
                let function = match stack.get(0).copied().unwrap_or(Value::Nil) {
                    Value::Function(function) => function,
                    value => {
                        return Err(TypeError {
                            expected: "function",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                let thread = Thread::new(mc);
                thread.start_suspended(mc, function).unwrap();
                stack.clear();
                stack.push(thread.into());
                Ok(CallbackReturn::Return.into())
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "resume",
            AnyCallback::from_fn(mc, |mc, stack| {
                let thread = match stack.get(0).copied().unwrap_or(Value::Nil) {
                    Value::Thread(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "thread",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                thread
                    .resume(mc, Variadic::from_iter(stack.drain(1..)))
                    .map_err(|_| RuntimeError("cannot resume thread".into_value(mc)))?;

                #[derive(Collect)]
                #[collect(require_static)]
                struct ThreadSequence;

                impl<'gc> Sequence<'gc> for ThreadSequence {
                    fn step(
                        &mut self,
                        mc: MutationContext<'gc, '_>,
                        stack: &mut Vec<Value<'gc>>,
                    ) -> Result<Option<CallbackReturn<'gc>>, crate::Error<'gc>>
                    {
                        let thread = match stack.get(0) {
                            Some(&Value::Thread(thread)) => thread,
                            _ => panic!("thread lost from stack"),
                        };

                        match thread.mode() {
                            ThreadMode::Return => {
                                stack.clear();
                                match thread.take_return::<Variadic<Value<'gc>>>(mc).unwrap() {
                                    Ok(res) => {
                                        stack.push(Value::Boolean(true));
                                        stack.extend(res)
                                    }
                                    Err(err) => {
                                        stack.extend([Value::Boolean(false), err.to_value(mc)]);
                                    }
                                }
                                Ok(Some(CallbackReturn::Return))
                            }
                            ThreadMode::Normal => {
                                thread.step(mc).unwrap();
                                Ok(None)
                            }
                            mode => Err(BadThreadMode {
                                expected: ThreadMode::Normal,
                                found: mode,
                            }
                            .into()),
                        }
                    }
                }

                Ok(CallbackMode::Sequence(ThreadSequence.into()))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "status",
            AnyCallback::from_fn(mc, |mc, stack| {
                let thread = match stack.get(0).copied().unwrap_or(Value::Nil) {
                    Value::Thread(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "thread",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                stack.clear();
                stack.push(
                    match thread.mode() {
                        ThreadMode::Stopped | ThreadMode::Return => "dead",
                        ThreadMode::Running => "running",
                        ThreadMode::Normal => "normal",
                        ThreadMode::Suspended => "suspended",
                    }
                    .into_value(mc),
                );
                Ok(CallbackReturn::Return.into())
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "yield",
            AnyCallback::from_fn(mc, |_, _| Ok(CallbackReturn::Yield(None).into())),
        )
        .unwrap();

    env.set(mc, "coroutine", coroutine).unwrap();
}
