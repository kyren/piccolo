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
                fn operate_sub<'a>(
                    string: &'a [u8],
                    i: i64,
                    j: Option<i64>,
                ) -> Result<&'a [u8], std::num::TryFromIntError> {
                    let i = if i > 0 {
                        i.saturating_sub(1).try_into()?
                    } else if i == 0 {
                        0
                    } else {
                        string.len().saturating_add_signed(i.try_into()?)
                    };
                    let j = if let Some(j) = j {
                        if j >= 0 {
                            j.try_into()?
                        } else {
                            let j: isize = j.try_into()?;
                            string.len().saturating_add_signed(j + 1)
                        }
                    } else {
                        string.len()
                    }
                    .clamp(0, string.len());

                    return Ok(if i >= j || i >= string.len() {
                        &[]
                    } else {
                        &string[i..j]
                    });
                }

                let string = stack.pop_front();
                let (i, j) = stack.consume::<(i64, Option<i64>)>(ctx)?;
                let string = match string {
                    Value::Integer(i) => ctx.intern(operate_sub(i.to_string().as_bytes(), i, j)?),
                    Value::Number(n) => ctx.intern(operate_sub(n.to_string().as_bytes(), i, j)?),
                    Value::String(s) => ctx.intern(operate_sub(s.as_bytes(), i, j)?),
                    v => {
                        return Err(format!(
                            "Bad argument to sub: expected string, got {}",
                            v.type_name()
                        )
                        .into_value(ctx)
                        .into())
                    }
                };

                stack.replace(ctx, string);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string).unwrap();
}
