use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{
    raw_ops, Callback, CallbackReturn, Continuation, Root, RuntimeError, String, Table, TypeError,
    Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, _root: Root<'gc>, env: Table<'gc>) {
    env.set(
        mc,
        "print",
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
        "error",
        Callback::new_immediate(mc, |_, _, args| {
            let err = args.get(0).copied().unwrap_or(Value::Nil);
            Err(RuntimeError(err).into())
        }),
    )
    .unwrap();

    env.set(
        mc,
        "assert",
        Callback::new_immediate(mc, |_, _, args| {
            let v = args.get(0).copied().unwrap_or(Value::Nil);
            let message = args.get(1).copied().unwrap_or("assertion failed!".into());

            if raw_ops::to_bool(v) {
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
                continuation: Some(Continuation::new_immediate(move |mc, res| {
                    Ok(CallbackReturn::Return(match res {
                        Ok(mut res) => {
                            res.insert(0, Value::Boolean(true));
                            res
                        }
                        Err(err) => {
                            vec![Value::Boolean(false), err.to_value(mc)]
                        }
                    }))
                })),
            })
        }),
    )
    .unwrap();

    env.set(
        mc,
        "type",
        Callback::new_immediate(mc, |_, _, args| {
            if args.len() == 0 {
                return Err(RuntimeError("Missing argument to type".into()).into());
            }
            Ok(CallbackReturn::Return(vec![args
                .get(0)
                .copied()
                .unwrap()
                .type_name()
                .into()]))
        }),
    )
    .unwrap();

    env.set(
        mc,
        "select",
        Callback::new_immediate(mc, |_, _, args| {
            match raw_ops::to_integer(args.get(0).copied().unwrap_or(Value::Nil)) {
                Some(n) if n >= 1 && (n as usize) <= args.len() => Ok(CallbackReturn::Return(
                    args[n as usize..args.len()].to_vec(),
                )),
                Some(n) if n as usize > args.len() => Ok(CallbackReturn::Return(vec![])),
                _ => Err(RuntimeError("Bad argument to 'select'".into()).into()),
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        "rawget",
        Callback::new_immediate(mc, |_, _, args| match (args.get(0), args.get(1)) {
            (Some(&Value::Table(table)), Some(&key)) => {
                Ok(CallbackReturn::Return(vec![table.get(key)]))
            }
            _ => Err(RuntimeError("Bad argument to 'rawget'".into()).into()),
        }),
    )
    .unwrap();

    env.set(
        mc,
        "rawset",
        Callback::new_immediate(mc, |mc, _, args| {
            match (args.get(0), args.get(1), args.get(2)) {
                (Some(&Value::Table(table)), Some(&key), Some(&value)) => {
                    table.set(mc, key, value)?;
                    Ok(CallbackReturn::Return(vec![Value::Table(table)]))
                }
                _ => Err(RuntimeError("Bad argument to 'rawset'".into()).into()),
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        "getmetatable",
        Callback::new_immediate(mc, |_, _, args| match args.get(0) {
            Some(&Value::Table(table)) => Ok(CallbackReturn::Return(vec![table
                .metatable()
                .map(Value::Table)
                .unwrap_or(Value::Nil)])),
            _ => Err(RuntimeError("'getmetatable' can only be used on table types".into()).into()),
        }),
    )
    .unwrap();

    env.set(
        mc,
        "setmetatable",
        Callback::new_immediate(mc, |mc, _, args| match (args.get(0), args.get(1)) {
            (Some(&Value::Table(table)), Some(&Value::Table(metatable))) => {
                table.set_metatable(mc, Some(metatable));
                Ok(CallbackReturn::Return(vec![table.into()]))
            }
            (Some(&Value::Table(table)), Some(Value::Nil)) => {
                table.set_metatable(mc, None);
                Ok(CallbackReturn::Return(vec![table.into()]))
            }
            _ => Err(RuntimeError(
                "Bad argument to 'setmetatable', can only be used with table types".into(),
            )
            .into()),
        }),
    )
    .unwrap();
}
