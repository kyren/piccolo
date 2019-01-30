use gc_arena::MutationContext;

use crate::{
    sequence_fn_with, Callback, CallbackResult, LuaContext, RuntimeError, SequenceExt, String,
    Table, Thread, TypeError, Value,
};

pub fn load_coroutine<'gc>(mc: MutationContext<'gc, '_>, _: LuaContext<'gc>, env: Table<'gc>) {
    let coroutine = Table::new(mc);

    coroutine
        .set(
            mc,
            String::new_static(b"create"),
            Callback::new_sequence(mc, |_, args| {
                let closure = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::Closure(closure) => closure,
                    value => {
                        return Err(TypeError {
                            expected: "closure",
                            found: value.type_name(),
                        }
                        .into());
                    }
                };

                Ok(Box::new(sequence_fn_with(closure, |mc, _, closure| {
                    Ok(CallbackResult::Return(vec![Value::Thread(
                        Thread::new_coroutine(mc, closure),
                    )]))
                })))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"resume"),
            Callback::new_sequence(mc, |_, args| {
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

                if !thread.is_suspended() {
                    Err(RuntimeError(Some("cannot resume thread".to_owned())).into())
                } else {
                    let args = args[1..].to_vec();
                    // TODO: Errors in coroutines are not handled yet
                    Ok(Box::new(
                        sequence_fn_with((thread, args), |mc, _, (thread, args)| {
                            thread.resume(mc, args).unwrap()
                        })
                        .map(|mut res| {
                            res.insert(0, Value::Boolean(true));
                            CallbackResult::Return(res)
                        }),
                    ))
                }
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"status"),
            Callback::new_immediate(mc, |self_thread, args| {
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
                    String::new_static(if thread.is_suspended() {
                        b"suspended"
                    } else if thread == self_thread {
                        b"running"
                    } else if thread.is_finished() {
                        b"dead"
                    } else {
                        b"running"
                    }),
                )]))
            }),
        )
        .unwrap();

    coroutine
        .set(
            mc,
            String::new_static(b"yield"),
            Callback::new_immediate(mc, |_, args| Ok(CallbackResult::Yield(args.to_vec()))),
        )
        .unwrap();

    env.set(mc, String::new_static(b"coroutine"), coroutine)
        .unwrap();
}
