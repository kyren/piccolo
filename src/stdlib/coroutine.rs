use gc_arena::MutationContext;

use crate::{
    Callback, CallbackReturn, Root, RuntimeError, SequenceExt, String, Table, Thread, ThreadMode,
    ThreadSequence, TypeError, Value,
};

pub fn load_coroutine<'gc>(mc: MutationContext<'gc, '_>, _root: Root<'gc>, env: Table<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            String::from_static(b"create"),
            Callback::new_immediate(mc, |mc, _, args| {
                let function = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Function(function) => function,
                    value => {
                        return Err(TypeError {
                            expected: "function",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                let thread = Thread::new(mc, true);
                thread.start_suspended(mc, function).unwrap();
                Ok(CallbackReturn::Return(vec![Value::Thread(thread)]))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::from_static(b"resume"),
            Callback::new_sequence(mc, |mc, _, mut args| {
                let thread = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Thread(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "thread",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                args.remove(0);

                thread.resume(mc, &args).map_err(|_| {
                    RuntimeError(Value::String(String::from_static(b"cannot resume thread")))
                })?;

                Ok(ThreadSequence(thread).then(|mc, res| {
                    Ok(CallbackReturn::Return(match res {
                        Ok(mut res) => {
                            res.insert(0, Value::Boolean(true));
                            res
                        }
                        Err(err) => {
                            vec![Value::Boolean(false), err.to_value(mc)]
                        }
                    }))
                }))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::from_static(b"status"),
            Callback::new_immediate(mc, |_, current_thread, args| {
                let thread = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Thread(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "thread",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                Ok(CallbackReturn::Return(vec![Value::String(
                    // TODO: When the current thread is available again for callbacks, whether or
                    // not the active thread matches will determine 'normal' from 'running'.
                    String::from_static(match thread.mode() {
                        ThreadMode::Stopped | ThreadMode::Results => b"dead",
                        ThreadMode::Running => {
                            if thread == current_thread {
                                b"running"
                            } else {
                                b"normal"
                            }
                        }
                        ThreadMode::Suspended => b"suspended",
                    }),
                )]))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::from_static(b"yield"),
            Callback::new_immediate(mc, |_, _, args| Ok(CallbackReturn::Yield(args))),
        )
        .unwrap();

    env.set(mc, String::from_static(b"coroutine"), coroutine)
        .unwrap();
}
