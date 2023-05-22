use gc_arena::{Collect, Mutation};

use crate::{
    conversion::Variadic, AnyCallback, BadThreadMode, CallbackReturn, IntoValue, Root, Sequence,
    Stack, Table, Thread, ThreadMode, Value,
};

pub fn load_coroutine<'gc>(mc: &Mutation<'gc>, root: Root<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            "create",
            AnyCallback::from_fn(mc, |mc, stack| {
                let function = stack.consume(mc)?;
                let thread = Thread::new(mc);
                thread.start_suspended(mc, function).unwrap();
                stack.replace(mc, thread);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "resume",
            AnyCallback::from_fn(mc, |mc, stack| {
                let (thread, args): (Thread, Variadic<Value>) = stack.consume(mc)?;

                thread
                    .resume(mc, args)
                    .map_err(|_| "cannot resume thread".into_value(mc))?;

                #[derive(Collect)]
                #[collect(require_static)]
                struct ThreadSequence;

                impl<'gc> Sequence<'gc> for ThreadSequence {
                    fn step(
                        &mut self,
                        mc: &Mutation<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<Option<CallbackReturn<'gc>>, crate::Error<'gc>>
                    {
                        let thread = match stack.get(0) {
                            Value::Thread(thread) => thread,
                            _ => panic!("thread lost from stack"),
                        };

                        match thread.mode() {
                            ThreadMode::Return => {
                                match thread.take_return::<Variadic<Value<'gc>>>(mc).unwrap() {
                                    Ok(res) => {
                                        stack.replace(mc, (true, res));
                                    }
                                    Err(err) => {
                                        stack.replace(mc, (false, err.to_value(mc)));
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

                stack.push_front(thread.into());
                Ok(CallbackReturn::Sequence(ThreadSequence.into()))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "status",
            AnyCallback::from_fn(mc, |mc, stack| {
                let thread: Thread = stack.consume(mc)?;
                stack.replace(
                    mc,
                    match thread.mode() {
                        ThreadMode::Stopped | ThreadMode::Return => "dead",
                        ThreadMode::Running => "running",
                        ThreadMode::Normal => "normal",
                        ThreadMode::Suspended => "suspended",
                    },
                );
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            "yield",
            AnyCallback::from_fn(mc, |_, _| Ok(CallbackReturn::Yield(None))),
        )
        .unwrap();

    root.globals.set(mc, "coroutine", coroutine).unwrap();
}
