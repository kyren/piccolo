use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{
    meta_ops, AnyCallback, AnyContinuation, CallbackReturn, Root, RuntimeError, String, Table,
    Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, _root: Root<'gc>, env: Table<'gc>) {
    env.set(
        mc,
        "print",
        AnyCallback::from_fn(mc, |_, stack| {
            let mut stdout = io::stdout();
            for i in 0..stack.len() {
                stack[i].display(&mut stdout)?;
                if i != stack.len() - 1 {
                    stdout.write_all(&b"\t"[..])?;
                }
            }
            stdout.write_all(&b"\n"[..])?;
            stdout.flush()?;
            stack.clear();
            Ok(CallbackReturn::Return.into())
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "error",
        AnyCallback::from_fn(mc, |_, stack| {
            let err = stack.get(0).copied().unwrap_or(Value::Nil);
            Err(RuntimeError(err).into())
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "assert",
        AnyCallback::from_fn(mc, |mc, stack| {
            let v = stack.get(0).copied().unwrap_or(Value::Nil);
            let message = stack
                .get(1)
                .copied()
                .unwrap_or(String::from_static(mc, "assertion failed!").into());
            stack.clear();
            if v.to_bool() {
                Ok(CallbackReturn::Return.into())
            } else {
                Err(RuntimeError(message).into())
            }
        })
        .into(),
    )
    .unwrap();

    let pcall_cont = AnyContinuation::from_fns(
        mc,
        move |_, stack| {
            stack.insert(0, Value::Boolean(true));
            Ok(CallbackReturn::Return.into())
        },
        move |mc, stack, error| {
            stack.clear();
            stack.extend([Value::Boolean(false), error.to_value(mc)]);
            Ok(CallbackReturn::Return.into())
        },
    );

    env.set(
        mc,
        "pcall",
        AnyCallback::from_fn_with(mc, pcall_cont, move |pcall_cont, mc, stack| {
            let function = meta_ops::call(mc, stack.get(0).copied().unwrap_or(Value::Nil))?;
            stack.remove(0);
            Ok(CallbackReturn::TailCall(function, Some(*pcall_cont)).into())
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "type",
        AnyCallback::from_fn(mc, |mc, stack| {
            if let Some(&v) = stack.get(0) {
                stack.clear();
                stack.push(String::from_static(mc, v.type_name().as_bytes()).into());
                Ok(CallbackReturn::Return.into())
            } else {
                Err(RuntimeError(String::from_static(mc, "Missing argument to type").into()).into())
            }
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "select",
        AnyCallback::from_fn(mc, |mc, stack| {
            match stack.get(0).copied().unwrap_or(Value::Nil).to_integer() {
                Some(n) if n >= 1 => {
                    stack.drain(0..(n as usize).min(stack.len()));
                    Ok(CallbackReturn::Return.into())
                }
                _ => Err(
                    RuntimeError(String::from_static(mc, "Bad argument to 'select'").into()).into(),
                ),
            }
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "rawget",
        AnyCallback::from_fn(mc, |mc, stack| match (stack.get(0), stack.get(1)) {
            (Some(&Value::Table(table)), Some(&key)) => {
                stack.clear();
                stack.push(table.get(key));
                Ok(CallbackReturn::Return.into())
            }
            _ => {
                Err(RuntimeError(String::from_static(mc, "Bad argument to 'rawget'").into()).into())
            }
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "rawset",
        AnyCallback::from_fn(mc, |mc, stack| {
            match (stack.get(0), stack.get(1), stack.get(2)) {
                (Some(&Value::Table(table)), Some(&key), Some(&value)) => {
                    table.set(mc, key, value)?;
                    stack.drain(1..);
                    Ok(CallbackReturn::Return.into())
                }
                _ => Err(
                    RuntimeError(String::from_static(mc, "Bad argument to 'rawset'").into()).into(),
                ),
            }
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "getmetatable",
        AnyCallback::from_fn(mc, |mc, stack| match stack.get(0) {
            Some(&Value::Table(table)) => {
                stack.clear();
                if let Some(metatable) = table.metatable() {
                    stack.push(metatable.into());
                }
                Ok(CallbackReturn::Return.into())
            }
            _ => Err(RuntimeError(
                String::from_static(mc, "'getmetatable' can only be used on table types").into(),
            )
            .into()),
        })
        .into(),
    )
    .unwrap();

    env.set(
        mc,
        "setmetatable",
        AnyCallback::from_fn(mc, |mc, stack| match (stack.get(0), stack.get(1)) {
            (Some(&Value::Table(table)), Some(&Value::Table(metatable))) => {
                stack.drain(1..);
                table.set_metatable(mc, Some(metatable));
                Ok(CallbackReturn::Return.into())
            }
            (Some(&Value::Table(table)), Some(Value::Nil)) => {
                stack.drain(1..);
                table.set_metatable(mc, None);
                Ok(CallbackReturn::Return.into())
            }
            _ => Err(RuntimeError(
                String::from_static(
                    mc,
                    "Bad argument to 'setmetatable', can only be used with table types",
                )
                .into(),
            )
            .into()),
        })
        .into(),
    )
    .unwrap();
}
