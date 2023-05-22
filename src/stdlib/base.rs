use std::io::{self, Write};

use gc_arena::Mutation;

use crate::{
    meta_ops::{self, MetaMethod, MetaResult},
    table::NextValue,
    AnyCallback, AnyContinuation, CallbackReturn, IntoValue, Root, Table, Value,
};

pub fn load_base<'gc>(mc: &Mutation<'gc>, root: Root<'gc>) {
    root.globals
        .set(
            mc,
            "print",
            AnyCallback::from_fn(mc, |_, stack| {
                let mut stdout = io::stdout();
                for i in 0..stack.len() {
                    stack.get(i).display(&mut stdout)?;
                    if i != stack.len() - 1 {
                        stdout.write_all(&b"\t"[..])?;
                    }
                }
                stdout.write_all(&b"\n"[..])?;
                stdout.flush()?;
                stack.clear();
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "error",
            AnyCallback::from_fn(mc, |_, stack| {
                Err(stack.pop_front().unwrap_or_default().into())
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "assert",
            AnyCallback::from_fn(mc, |mc, stack| {
                if stack.get(0).to_bool() {
                    Ok(CallbackReturn::Return)
                } else if stack.get(1).is_nil() {
                    Err("assertion failed!".into_value(mc).into())
                } else {
                    Err(stack.get(1).into())
                }
            }),
        )
        .unwrap();

    let pcall_cont = AnyContinuation::from_fns(
        mc,
        move |_, stack| {
            stack.push_front(Value::Boolean(true));
            Ok(CallbackReturn::Return)
        },
        move |mc, stack, error| {
            stack.clear();
            stack.extend([Value::Boolean(false), error.to_value(mc)]);
            Ok(CallbackReturn::Return)
        },
    );

    root.globals
        .set(
            mc,
            "pcall",
            AnyCallback::from_fn_with(mc, pcall_cont, move |pcall_cont, mc, stack| {
                let function = meta_ops::call(mc, stack.get(0))?;
                stack.pop_front();
                Ok(CallbackReturn::TailCall(function, Some(*pcall_cont)))
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "type",
            AnyCallback::from_fn(mc, |mc, stack| {
                if let Some(v) = stack.consume::<Option<Value>>(mc)? {
                    stack.replace(mc, v.type_name());
                    Ok(CallbackReturn::Return)
                } else {
                    Err("Missing argument to type".into_value(mc).into())
                }
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "select",
            AnyCallback::from_fn(mc, |mc, stack| match stack.get(0).to_integer() {
                Some(n) if n >= 1 => {
                    let last = (n as usize).min(stack.len());
                    stack.drain(0..last);
                    Ok(CallbackReturn::Return)
                }
                _ => Err("Bad argument to 'select'".into_value(mc).into()),
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "rawget",
            AnyCallback::from_fn(mc, |mc, stack| {
                let (table, key): (Table, Value) = stack.consume(mc)?;
                stack.replace(mc, table.get(mc, key));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "rawset",
            AnyCallback::from_fn(mc, |mc, stack| {
                let (table, key, value): (Table, Value, Value) = stack.consume(mc)?;
                table.set(mc, key, value)?;
                stack.replace(mc, table);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "getmetatable",
            AnyCallback::from_fn(mc, |mc, stack| {
                if let Value::Table(t) = stack.get(0) {
                    stack.replace(mc, t.metatable());
                    Ok(CallbackReturn::Return)
                } else {
                    Err("'getmetatable' can only be used on table types"
                        .into_value(mc)
                        .into())
                }
            }),
        )
        .unwrap();

    root.globals
        .set(
            mc,
            "setmetatable",
            AnyCallback::from_fn(mc, |mc, stack| {
                let (t, mt): (Table, Option<Table>) = stack.consume(mc)?;
                t.set_metatable(mc, mt);
                stack.replace(mc, t);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn next<'gc>(
        mc: &Mutation<'gc>,
        table: Table<'gc>,
        index: Value<'gc>,
    ) -> Result<(Value<'gc>, Value<'gc>), Value<'gc>> {
        match table.next(mc, index) {
            NextValue::Found { key, value } => Ok((key, value)),
            NextValue::Last => Ok((Value::Nil, Value::Nil)),
            NextValue::NotFound => Err("invalid table key".into_value(mc)),
        }
    }

    let next = AnyCallback::from_fn(mc, |mc, stack| {
        let (table, index): (Table, Value) = stack.consume(mc)?;
        stack.replace(mc, next(mc, table, index)?);
        Ok(CallbackReturn::Return)
    });

    root.globals.set(mc, "next", next).unwrap();

    root.globals
        .set(
            mc,
            "pairs",
            AnyCallback::from_fn_with(mc, next, move |next, mc, stack| {
                let table = stack.get(0);
                if let Some(mt) = match table {
                    Value::Table(t) => t.metatable(),
                    Value::UserData(u) => u.metatable(),
                    _ => None,
                } {
                    let pairs = mt.get(mc, MetaMethod::Pairs);
                    if !pairs.is_nil() {
                        let f = meta_ops::call(mc, pairs)?;
                        stack.replace(mc, (table, Value::Nil));
                        return Ok(CallbackReturn::TailCall(f, None));
                    }
                }

                stack.replace(mc, (*next, table));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    let inext = AnyCallback::from_fn(mc, |mc, stack| {
        let (table, index): (Value, Option<i64>) = stack.consume(mc)?;
        let next_index = index.unwrap_or(0) + 1;
        Ok(match meta_ops::index(mc, table, next_index.into())? {
            MetaResult::Value(v) => {
                if !v.is_nil() {
                    stack.extend([next_index.into(), v]);
                }
                CallbackReturn::Return
            }
            MetaResult::Call(f, args) => {
                stack.extend(args);
                CallbackReturn::TailCall(
                    f,
                    Some(AnyContinuation::from_ok_fn(mc, move |_, stack| {
                        if !stack.get(0).is_nil() {
                            stack.push_front(next_index.into());
                        }
                        Ok(CallbackReturn::Return)
                    })),
                )
            }
        })
    });

    root.globals
        .set(
            mc,
            "ipairs",
            AnyCallback::from_fn_with(mc, inext, move |inext, mc, stack| {
                stack.into_front(mc, *inext);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
}
