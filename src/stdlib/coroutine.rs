use gc_arena::MutationContext;
use gc_sequence::{self as sequence, SequenceExt, SequenceResultExt};

use crate::{
    Callback, CallbackResult, CallbackReturn, LuaRoot, RuntimeError, String, Table, Thread,
    ThreadMode, ThreadSequence, TypeError, Value,
};

pub fn load_coroutine<'gc>(mc: MutationContext<'gc, '_>, root: LuaRoot<'gc>, env: Table<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            String::new_static(b"create"),
            Callback::new(mc, |args| {
                CallbackReturn::sequence(|| {
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

                    Ok(sequence::from_fn_with(function, |mc, function| {
                        let thread = Thread::new(mc, true);
                        thread.start_suspended(mc, function).unwrap();
                        Ok(CallbackResult::Return(vec![Value::Thread(thread)]))
                    }))
                })
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"resume"),
            Callback::new_with(mc, root.interned_strings, |interned_strings, mut args| {
                CallbackReturn::sequence(|| {
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
                    Ok(
                        sequence::from_fn_with((thread, args), |mc, (thread, args)| {
                            if let Ok(()) = thread.resume(mc, &args) {
                                Ok(ThreadSequence(thread))
                            } else {
                                Err(RuntimeError(Value::String(String::new_static(
                                    b"cannot resume thread",
                                )))
                                .into())
                            }
                        })
                        .flatten_ok()
                        .then_with(
                            *interned_strings,
                            |mc, interned_strings, res| {
                                Ok(CallbackResult::Return(match res {
                                    Ok(mut res) => {
                                        res.insert(0, Value::Boolean(true));
                                        res
                                    }
                                    Err(err) => vec![
                                        Value::Boolean(false),
                                        err.to_value(mc, interned_strings),
                                    ],
                                }))
                            },
                        ),
                    )
                })
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"status"),
            Callback::new(mc, |args| {
                CallbackReturn::immediate(|| {
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
                        // TODO: When the current thread is available again for callbacks, whether or
                        // not the active thread matches will determine 'normal' from 'running'.
                        String::new_static(match thread.mode() {
                            ThreadMode::Stopped | ThreadMode::Results => b"dead",
                            ThreadMode::Running => b"running",
                            ThreadMode::Suspended => b"suspended",
                        }),
                    )]))
                })
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"yield"),
            Callback::new(mc, |args| {
                CallbackReturn::Immediate(Ok(CallbackResult::Yield(args)))
            }),
        )
        .unwrap();

    env.set(mc, String::new_static(b"coroutine"), coroutine)
        .unwrap();
}
