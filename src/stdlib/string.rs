use crate::{AnyCallback, CallbackReturn, Context, IntoValue, String, Table, Value};

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string
        .set(
            ctx,
            "len",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
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
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (str_value, v2, v3): (String, Value, Value) = stack.consume(ctx)?;

                let start_index = match v2 {
                    Value::Integer(i) => i as usize - 1,
                    Value::Number(i) => i as usize - 1,
                    _ => return Err("Bad argument to string.sub".into_value(ctx).into()),
                };

                let std_str_val = str_value.to_str()?;

                let end_index = match v3 {
                    Value::Nil => std_str_val.len(),
                    Value::Integer(i) => i as usize,
                    Value::Number(i) => i as usize,
                    _ => return Err("Bad argument to string.sub".into_value(ctx).into()),
                };

                let sub_str = if start_index == 0 && end_index == std_str_val.len() {
                    std_str_val
                } else {
                    std_str_val
                        .get(start_index..end_index)
                        .ok_or_else(|| "String index out of bounds".into_value(ctx))?
                };
                stack.replace(ctx, String::from_slice(&ctx, sub_str));
                return Ok(CallbackReturn::Return);
            }),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "string", string).unwrap();
}
