use gc_arena::Collect;

use crate::{
    meta_ops, AnyCallback, AnySequence, CallbackReturn, Context, Fuel, Sequence, SequencePoll,
    Stack, Table, Thread, ThreadMode, Value, Variadic,
};

pub fn load_coroutine<'gc>(ctx: Context<'gc>) {
    let coroutine = Table::new(&ctx);

    coroutine
        .set(
            ctx,
            "create",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let thread = Thread::new(&ctx);
                thread
                    .start_suspended(&ctx, meta_ops::call(ctx, stack.get(0))?)
                    .unwrap();
                stack.replace(ctx, thread);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "resume",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (thread, args): (Thread, Variadic<Vec<Value>>) = stack.consume(ctx)?;

                thread.resume(ctx, args)?;

                #[derive(Collect)]
                #[collect(require_static)]
                struct ThreadSequence;

                impl<'gc> Sequence<'gc> for ThreadSequence {
                    fn poll(
                        &mut self,
                        ctx: Context<'gc>,
                        fuel: &mut Fuel,
                        stack: &mut Stack<'gc>,
                    ) -> Result<SequencePoll<'gc>, crate::Error<'gc>> {
                        let thread = match stack.get(0) {
                            Value::Thread(thread) => thread,
                            _ => panic!("thread lost from stack"),
                        };

                        if thread.mode() == ThreadMode::Result {
                            match thread
                                .take_return::<Variadic<Vec<Value<'gc>>>>(ctx)
                                .unwrap()
                            {
                                Ok(res) => {
                                    stack.replace(ctx, (true, res));
                                }
                                Err(err) => {
                                    stack.replace(ctx, (false, err.to_value(ctx)));
                                }
                            }
                            Ok(SequencePoll::Return)
                        } else {
                            thread.step(ctx, fuel)?;
                            Ok(SequencePoll::Pending)
                        }
                    }
                }

                stack.push_front(thread.into());
                Ok(CallbackReturn::Sequence(AnySequence::new(
                    &ctx,
                    ThreadSequence,
                )))
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "status",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
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
            AnyCallback::from_fn(&ctx, |_, _, _| Ok(CallbackReturn::Yield(None))),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "coroutine", coroutine).unwrap();
}
