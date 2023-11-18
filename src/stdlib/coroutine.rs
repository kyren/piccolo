use gc_arena::Collect;

use crate::{
    meta_ops, AnyCallback, AnySequence, CallbackReturn, Context, Fuel, Sequence, SequencePoll,
    Stack, Table, Thread, ThreadMode,
};

pub fn load_coroutine<'gc>(ctx: Context<'gc>) {
    let coroutine = Table::new(&ctx);

    coroutine
        .set(
            ctx,
            "create",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let thread = Thread::new(ctx);
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
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let thread: Thread = stack.from_front(ctx)?;

                #[derive(Collect)]
                #[collect(require_static)]
                struct ResumeHandler;

                impl<'gc> Sequence<'gc> for ResumeHandler {
                    fn poll(
                        &mut self,
                        ctx: Context<'gc>,
                        _fuel: &mut Fuel,
                        mut stack: Stack<'gc, '_>,
                    ) -> Result<SequencePoll<'gc>, crate::Error<'gc>> {
                        stack.into_front(ctx, true);
                        Ok(SequencePoll::Return)
                    }

                    fn error(
                        &mut self,
                        ctx: Context<'gc>,
                        _fuel: &mut Fuel,
                        error: crate::Error<'gc>,
                        mut stack: Stack<'gc, '_>,
                    ) -> Result<SequencePoll<'gc>, crate::Error<'gc>> {
                        stack.replace(ctx, (false, error.to_value(ctx)));
                        Ok(SequencePoll::Return)
                    }
                }

                Ok(CallbackReturn::Resume {
                    thread,
                    then: Some(AnySequence::new(&ctx, ResumeHandler)),
                })
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "continue",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let thread: Thread = stack.from_front(ctx)?;
                Ok(CallbackReturn::Resume { thread, then: None })
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "status",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let thread: Thread = stack.consume(ctx)?;
                stack.replace(
                    ctx,
                    match thread.mode() {
                        ThreadMode::Stopped => "dead",
                        ThreadMode::Running | ThreadMode::Waiting => "running",
                        ThreadMode::Normal => "normal",
                        ThreadMode::Result | ThreadMode::Suspended => "suspended",
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
            AnyCallback::from_fn(&ctx, |_, _, _| {
                Ok(CallbackReturn::Yield {
                    to_thread: None,
                    then: None,
                })
            }),
        )
        .unwrap();

    coroutine
        .set(
            ctx,
            "yieldto",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let thread: Thread = stack.from_front(ctx)?;
                Ok(CallbackReturn::Yield {
                    to_thread: Some(thread),
                    then: None,
                })
            }),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "coroutine", coroutine).unwrap();
}
