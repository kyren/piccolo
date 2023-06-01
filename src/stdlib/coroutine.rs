use gc_arena::Collect;

use crate::{
    AnyCallback, BadThreadMode, CallbackReturn, Context, Continuation, ContinuationPoll, IntoValue,
    Stack, Table, Thread, ThreadMode, Value, Variadic,
};

pub fn load_coroutine<'gc>(ctx: Context<'gc>) {
    let coroutine = Table::new(&ctx);

    coroutine
        .set(
            ctx,
            "create",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let function = stack.consume(ctx)?;
                let thread = Thread::new(&ctx);
                thread.start_suspended(&ctx, function).unwrap();
                stack.replace(ctx, thread);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "resume",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let (thread, args): (Thread, Variadic<Value>) = stack.consume(ctx)?;

                thread
                    .resume(ctx, args)
                    .map_err(|_| "cannot resume thread".into_value(ctx))?;

                #[derive(Collect)]
                #[collect(require_static)]
                struct ThreadContinuation;

                impl<'gc> Continuation<'gc> for ThreadContinuation {
                    fn poll(
                        &mut self,
                        ctx: Context<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<ContinuationPoll<'gc>, crate::Error<'gc>> {
                        let thread = match stack.get(0) {
                            Value::Thread(thread) => thread,
                            _ => panic!("thread lost from stack"),
                        };

                        match thread.mode() {
                            ThreadMode::Result => {
                                match thread.take_return::<Variadic<Value<'gc>>>(ctx).unwrap() {
                                    Ok(res) => {
                                        stack.replace(ctx, (true, res));
                                    }
                                    Err(err) => {
                                        stack.replace(ctx, (false, err.to_value(&ctx)));
                                    }
                                }
                                Ok(ContinuationPoll::Return)
                            }
                            ThreadMode::Normal => {
                                thread.step(ctx).unwrap();
                                Ok(ContinuationPoll::Pending)
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
                Ok(CallbackReturn::Continuation(ThreadContinuation.into()))
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "status",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let thread: Thread = stack.consume(ctx)?;
                stack.replace(
                    ctx,
                    match thread.mode() {
                        ThreadMode::Stopped | ThreadMode::Result => "dead",
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
            ctx,
            "yield",
            AnyCallback::from_fn(&ctx, |_, _| Ok(CallbackReturn::Yield(None))),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "coroutine", coroutine).unwrap();
}
