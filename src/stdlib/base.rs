use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{
    meta_ops::{self, MetaMethod, MetaResult},
    table::NextValue,
    AnyCallback, AnyContinuation, CallbackReturn, FromMultiValue, IntoValue, Root, Table, Value,
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
        }),
    )
    .unwrap();

    env.set(
        mc,
        "error",
        AnyCallback::from_immediate_fn::<_, ()>(mc, |_, e: Value| Err(e.into())),
    )
    .unwrap();

    env.set(
        mc,
        "assert",
        AnyCallback::from_immediate_fn(mc, |_, (r, message): (Value, Value)| {
            if r.to_bool() {
                Ok((CallbackReturn::Return, r))
            } else {
                Err(message.into())
            }
        }),
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
        }),
    )
    .unwrap();

    env.set(
        mc,
        "type",
        AnyCallback::from_fn(mc, |mc, stack| {
            if let Some(&v) = stack.get(0) {
                stack.clear();
                stack.push(v.type_name().into_value(mc));
                Ok(CallbackReturn::Return.into())
            } else {
                Err("Missing argument to type".into_value(mc).into())
            }
        }),
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
                _ => Err("Bad argument to 'select'".into_value(mc).into()),
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        "rawget",
        AnyCallback::from_immediate_fn(mc, |mc, (table, key): (Table, Value)| {
            Ok((CallbackReturn::Return, table.get(mc, key)))
        }),
    )
    .unwrap();

    env.set(
        mc,
        "rawset",
        AnyCallback::from_immediate_fn(mc, |mc, (table, key, value): (Table, Value, Value)| {
            table.set(mc, key, value)?;
            Ok((CallbackReturn::Return, table))
        }),
    )
    .unwrap();

    env.set(
        mc,
        "getmetatable",
        AnyCallback::from_immediate_fn(mc, |mc, v: Value| {
            if let Value::Table(t) = v {
                Ok((CallbackReturn::Return, t.metatable()))
            } else {
                Err("'getmetatable' can only be used on table types"
                    .into_value(mc)
                    .into())
            }
        }),
    )
    .unwrap();

    env.set(
        mc,
        "setmetatable",
        AnyCallback::from_immediate_fn(mc, |mc, (t, mt): (Table, Option<Table>)| {
            t.set_metatable(mc, mt);
            Ok((CallbackReturn::Return, t))
        }),
    )
    .unwrap();

    fn next<'gc>(
        mc: MutationContext<'gc, '_>,
        table: Table<'gc>,
        index: Value<'gc>,
    ) -> Result<(Value<'gc>, Value<'gc>), Value<'gc>> {
        match table.next(mc, index) {
            NextValue::Found { key, value } => Ok((key, value)),
            NextValue::Last => Ok((Value::Nil, Value::Nil)),
            NextValue::NotFound => Err("invalid table key".into_value(mc)),
        }
    }

    let next = AnyCallback::from_immediate_fn(mc, |mc, (table, index): (Table, Value)| {
        let (key, value) = next(mc, table, index)?;
        Ok((CallbackReturn::Return, (key, value)))
    });

    env.set(mc, "next", next).unwrap();

    env.set(
        mc,
        "pairs",
        AnyCallback::from_immediate_fn_with(mc, next, move |next, mc, table: Value| {
            if let Some(mt) = match table {
                Value::Table(t) => t.metatable(),
                Value::UserData(u) => u.metatable(),
                _ => None,
            } {
                let pairs = mt.get(mc, MetaMethod::Pairs);
                if !pairs.is_nil() {
                    let f = meta_ops::call(mc, pairs)?;
                    return Ok((CallbackReturn::TailCall(f, None), [table, Value::Nil]));
                }
            }

            Ok((CallbackReturn::Return, [Value::from(*next), table]))
        }),
    )
    .unwrap();

    let inext = AnyCallback::from_fn(mc, |mc, stack| {
        let (table, index): (Value, Option<i64>) =
            FromMultiValue::from_multi_value(mc, stack.drain(..))?;
        let next_index = index.unwrap_or(0) + 1;
        Ok(match meta_ops::index(mc, table, next_index.into())? {
            MetaResult::Value(v) => {
                if !v.is_nil() {
                    stack.extend([next_index.into(), v]);
                }
                CallbackReturn::Return.into()
            }
            MetaResult::Call(f, args) => {
                stack.extend(args);
                CallbackReturn::TailCall(
                    f,
                    Some(AnyContinuation::from_ok_fn(mc, move |_, stack| {
                        if !stack.get(0).copied().unwrap_or(Value::Nil).is_nil() {
                            stack.insert(0, next_index.into());
                        }
                        Ok(CallbackReturn::Return.into())
                    })),
                )
                .into()
            }
        })
    });

    env.set(
        mc,
        "ipairs",
        AnyCallback::from_immediate_fn_with(mc, inext, move |inext, _, table: Value| {
            Ok((CallbackReturn::Return, [Value::from(*inext), table]))
        }),
    )
    .unwrap();
}
