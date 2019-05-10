use std::io::{self, Write};

use gc_arena::MutationContext;
use gc_sequence as sequence;

use crate::{
    Callback, CallbackResult, Continuation, Root, RuntimeError, String, Table, TypeError, Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, root: Root<'gc>, env: Table<'gc>) {
    env.set(
        mc,
        "print",
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
        "error",
        Callback::new_immediate(mc, |args| {
            let err = args.get(0).cloned().unwrap_or(Value::Nil);
            Err(RuntimeError(err).into())
        }),
    )
    .unwrap();

    env.set(
        mc,
        "assert",
        Callback::new_immediate(mc, |args| {
            let v = args.get(0).cloned().unwrap_or(Value::Nil);
            let message = args
                .get(1)
                .cloned()
                .unwrap_or(Value::String(String::new_static(b"assertion failed!")));

            if v.to_bool() {
                Ok(CallbackResult::Return(args))
            } else {
                Err(RuntimeError(message).into())
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        "pcall",
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

    env.set(
        mc,
        "type",
        Callback::new_immediate(mc, |args| {
            if args.len() == 0 {
                return Err(RuntimeError(Value::String(String::new_static(
                    b"Missing argument to type",
                )))
                .into());
            }
            Ok(CallbackResult::Return(vec![Value::String(
                String::new_static(args.get(0).cloned().unwrap().type_name().as_bytes()),
            )]))
        }),
    )
    .unwrap();

    env.set(
        mc,
        "select",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_integer() {
                Some(n) if n >= 1 && (n as usize) <= args.len() => Ok(CallbackResult::Return(
                    args[n as usize..args.len()].to_vec(),
                )),
                // This is required because Rust will panic if the starting slice index is out of
                // range by more than one
                Some(n) if n as usize > args.len() => Ok(CallbackResult::Return(vec![])),
                _ => Err(RuntimeError(Value::String(String::new_static(
                    b"Bad argument to select",
                )))
                .into()),
            }
        }),
    )
    .unwrap();
}
