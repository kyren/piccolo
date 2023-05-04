use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{
    Callback, CallbackReturn, Continuation, Root, RuntimeError, String, Table, TypeError, Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, _root: Root<'gc>, env: Table<'gc>) {
    env.set(
        mc,
        String::from_static(b"print"),
        Callback::new_immediate(mc, |_, _, args| {
            let mut stdout = io::stdout();
            for i in 0..args.len() {
                args[i].display(&mut stdout)?;
                if i != args.len() - 1 {
                    stdout.write_all(&b"\t"[..])?;
                }
            }
            stdout.write_all(&b"\n"[..])?;
            stdout.flush()?;
            Ok(CallbackReturn::Return(vec![]))
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::from_static(b"error"),
        Callback::new_immediate(mc, |_, _, args| {
            let err = args.get(0).cloned().unwrap_or(Value::Nil);
            Err(RuntimeError(err).into())
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::from_static(b"assert"),
        Callback::new_immediate(mc, |_, _, args| {
            let v = args.get(0).cloned().unwrap_or(Value::Nil);
            let message = args
                .get(1)
                .cloned()
                .unwrap_or(Value::String(String::from_static(b"assertion failed!")));

            if v.to_bool() {
                Ok(CallbackReturn::Return(args))
            } else {
                Err(RuntimeError(message).into())
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::from_static(b"pcall"),
        Callback::new_immediate(mc, |_, _, mut args| {
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
            Ok(CallbackReturn::TailCall {
                function,
                args,
                continuation: Continuation::new_immediate(move |mc, res| {
                    Ok(CallbackReturn::Return(match res {
                        Ok(mut res) => {
                            res.insert(0, Value::Boolean(true));
                            res
                        }
                        Err(err) => {
                            vec![Value::Boolean(false), err.to_value(mc)]
                        }
                    }))
                }),
            })
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::from_static(b"type"),
        Callback::new_immediate(mc, |_, _, args| {
            if args.len() == 0 {
                return Err(RuntimeError(Value::String(String::from_static(
                    b"Missing argument to type",
                )))
                .into());
            }
            Ok(CallbackReturn::Return(vec![Value::String(
                String::from_static(args.get(0).cloned().unwrap().type_name().as_bytes()),
            )]))
        }),
    )
    .unwrap();

    env.set(
        mc,
        String::from_static(b"select"),
        Callback::new_immediate(mc, |_, _, args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_integer() {
                Some(n) if n >= 1 && (n as usize) <= args.len() => Ok(CallbackReturn::Return(
                    args[n as usize..args.len()].to_vec(),
                )),
                // This is required because Rust will panic if the starting slice index is out of
                // range by more than one
                Some(n) if n as usize > args.len() => Ok(CallbackReturn::Return(vec![])),
                _ => Err(RuntimeError(Value::String(String::from_static(
                    b"Bad argument to select",
                )))
                .into()),
            }
        }),
    )
    .unwrap();
}
