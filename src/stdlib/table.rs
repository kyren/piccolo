use crate::{AnyCallback, CallbackReturn, Context, IntoValue, String, Table, Value};
use std::string::String as StdString;

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table
        .set(
            ctx,
            "pack",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let t = Table::new(&ctx);
                for i in 0..stack.len() {
                    t.set(ctx, i as i64 + 1, stack[i]).unwrap();
                }
                t.set(ctx, "n", stack.len() as i64).unwrap();
                stack.replace(ctx, t);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "unpack",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (table, start, end): (Table<'gc>, Option<i64>, Option<i64>) =
                    stack.consume(ctx)?;
                let start = start.unwrap_or(1);
                let end = end.unwrap_or_else(|| table.length());

                if start <= end {
                    stack.resize((end - start + 1) as usize);
                    for i in start..=end {
                        stack[(i - start) as usize] = table.get_value(i.into());
                    }
                }

                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "insert",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (table, v1, v2): (Table, Value, Value) = stack.consume(ctx)?;

                let (insert_pos, new_value) = match v2 {
                    Value::Nil => ((table.length() + 1) as usize, v1),
                    _ => (
                        match v1 {
                            Value::Integer(i) => i as usize,
                            _ => {
                                return Err("Invalid index given to table.insert"
                                    .into_value(ctx)
                                    .into());
                            }
                        },
                        v2,
                    ),
                };

                table
                    .0
                    .borrow_mut(&ctx)
                    .entries
                    .insert(insert_pos, new_value.into_value(ctx))?;

                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "remove",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (table, index): (Table, Option<i64>) = stack.consume(ctx)?;
                let len = table.length();

                if len == 0 {
                    return Ok(CallbackReturn::Return);
                }

                let index = index.unwrap_or(len);

                if index < 1 || index > len {
                    return Ok(CallbackReturn::Return);
                }

                stack.replace(ctx, table.0.borrow_mut(&ctx).entries.remove(index as usize));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "concat",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (table, sep, start, end): (Table, Option<String>, Option<i64>, Option<i64>) =
                    stack.consume(ctx)?;

                let start = start.unwrap_or(1);
                let end = end.unwrap_or(table.length());
                let mut result = StdString::new();

                let sep = match sep {
                    Some(sep) => Some(sep.to_str()?),
                    None => None,
                };

                for i in start..=end {
                    let entry = table.get(ctx, Value::Integer(i));

                    match entry {
                        Value::Nil => {
                            return Err("Encountered nil within given range to table.concat"
                                .into_value(ctx)
                                .into());
                        }
                        Value::Integer(i) => result.push_str(&i.to_string()),
                        Value::Number(i) => result.push_str(&i.to_string()),
                        Value::String(s) => match s.to_str() {
                            Ok(s) => result.push_str(s),
                            Err(_) => {
                                return Err("Failed to convert given string".into_value(ctx).into())
                            }
                        },
                        _ => {
                            return Err("Invalid value within given range to table.concat"
                                .into_value(ctx)
                                .into());
                        }
                    };

                    if i != end {
                        if let Some(ref sep) = sep {
                            result.push_str(sep);
                        }
                    }
                }

                stack.replace(ctx, Value::String(String::from_slice(&ctx, result)));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "table", table).unwrap();
}
