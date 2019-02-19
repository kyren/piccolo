use std::io::{self, Write};

use gc_arena::MutationContext;
use gc_sequence as sequence;

use crate::{
    Callback, CallbackResult, Continuation, LuaRoot, RuntimeError, String, Table, TypeError, Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, root: LuaRoot<'gc>, env: Table<'gc>) {
    env.set(
        mc,
        String::new_static(b"print"),
        Callback::new_immediate(mc, |args| {
            let mut stdout = io::stdout();
            for i in 0..args.len() {
                args[i].display(&mut stdout)?;
                if i != args.len() - 1 {
                    stdout.write_all(&b"\t"[..])?;
                }
            }
            stdout.write_all(&b"\n"[..])?;
            stdout.flush()?;
            Ok(CallbackResult::Return(vec![]))
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::new_static(b"error"),
        Callback::new_immediate(mc, |args| {
            let err = args.get(0).cloned().unwrap_or(Value::Nil);
            Err(RuntimeError(err).into())
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::new_static(b"pcall"),
        Callback::new_immediate_with(mc, root.interned_strings, |interned_strings, mut args| {
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

            args.remove(0);
            Ok(CallbackResult::TailCall {
                function,
                args,
                continuation: Continuation::new_sequence_with(
                    *interned_strings,
                    move |interned_strings, res| {
                        Ok(sequence::from_fn_with(
                            (res, interned_strings),
                            |mc, (res, interned_strings)| {
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
                        ))
                    },
                ),
            })
        }),
    )
    .unwrap();
}
