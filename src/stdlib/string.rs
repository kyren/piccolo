use crate::{Callback, CallbackReturn, Context, IntoValue, Table, Value};

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string
        .set(
            ctx,
            "len",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let v: Option<Value> = stack.consume(ctx)?;
                if let Some(len) = v.and_then(|v| match v {
                    Value::Integer(i) => Some(i.to_string().as_bytes().len().try_into().unwrap()),
                    Value::Number(n) => Some(n.to_string().as_bytes().len().try_into().unwrap()),
                    Value::String(s) => Some(s.len()),
                    _ => None,
                }) {
                    stack.replace(ctx, len);
                    Ok(CallbackReturn::Return)
                } else {
                    Err("Bad argument to len".into_value(ctx).into())
                }
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "sub",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.pop_front();
                let string = match string {
                    Value::Integer(i) => i.to_string().as_bytes().to_vec(),
                    Value::String(s) => s.as_bytes().to_vec(),
                    v => {
                        return Err(format!(
                            "Bad argument to sub: expected string, got {}",
                            v.type_name()
                        )
                        .into_value(ctx)
                        .into())
                    }
                };

                let (i, j) = stack.consume::<(i64, Option<i64>)>(ctx)?;
                let i = if i >= 0 {
                    i.saturating_sub(1) as usize
                } else {
                    string.len().saturating_add_signed(i as isize)
                };
                let j = if let Some(j) = j {
                    if j >= 0 {
                        j as usize
                    } else {
                        string.len().saturating_add_signed(j as isize + 1)
                    }
                } else {
                    string.len()
                }
                .clamp(0, string.len());

                let result = if i >= j || i >= string.len() {
                    &[]
                } else {
                    &string[i..j]
                };
                stack.replace(ctx, crate::String::from_slice(&ctx, result).into_value(ctx));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string).unwrap();
}
