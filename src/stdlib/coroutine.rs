use gc_arena::MutationContext;

use crate::{
    sequence_fn_with, thread::ThreadMode, vm::ThreadSequence, Callback, CallbackResult,
    IntoSequence, LuaContext, RuntimeError, SequenceExt, String, Table, Thread, TypeError, Value,
};

pub fn load_coroutine<'gc>(mc: MutationContext<'gc, '_>, _: LuaContext<'gc>, env: Table<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            String::new_static(b"create"),
            Callback::new(mc, |args| {
                let function = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Function(function) => function,
                    value => {
                        return Err(TypeError {
                            expected: "function",
                            found: value.type_name(),
                        }
                        .into())
                        .into_sequence()
                        .boxed();
                    }
                };

                sequence_fn_with(function, |mc, _, function| {
                    let thread = Thread::new(mc, true);
                    thread.start_suspended(mc, function)?;
                    Ok(CallbackResult::Return(vec![Value::Thread(thread)]))
                })
                .boxed()
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"resume"),
            Callback::new(mc, |mut args| {
                let thread = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Thread(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "thread",
                            found: value.type_name(),
                        }
                        .into())
                        .into_sequence()
                        .boxed();
                    }
                };

                args.remove(0);
                sequence_fn_with((thread, args), |mc, _, (thread, args)| {
                    if let Ok(()) = thread.resume(mc, &args) {
                        Ok(ThreadSequence(thread))
                    } else {
                        Err(RuntimeError(Value::String(String::new_static(
                            b"cannot resume thread",
                        )))
                        .into())
                    }
                })
                .flatten()
                .then(|mc, lc, res| {
                    Ok(CallbackResult::Return(match res {
                        Ok(mut res) => {
                            res.insert(0, Value::Boolean(true));
                            res
                        }
                        Err(err) => {
                            vec![Value::Boolean(false), err.to_value(mc, lc.interned_strings)]
                        }
                    }))
                })
                .boxed()
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"status"),
            Callback::new_immediate(mc, |args| {
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

                Ok(CallbackResult::Return(vec![Value::String(
                    String::new_static(match thread.mode() {
                        ThreadMode::Stopped | ThreadMode::Results => b"dead",
                        ThreadMode::Lua => b"normal",
                        ThreadMode::Callback | ThreadMode::Running => b"running",
                        ThreadMode::Suspended => b"suspended",
                    }),
                )]))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"yield"),
            Callback::new_immediate(mc, |args| Ok(CallbackResult::Yield(args))),
        )
        .unwrap();

    env.set(mc, String::new_static(b"coroutine"), coroutine)
        .unwrap();
}
