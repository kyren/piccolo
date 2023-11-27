use lua_patterns::LuaPattern;

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
            "match",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
                let (v1, v2): (Value, Value) = stack.consume(ctx)?;

                if let Value::String(s1) = v1 {
                    if let Value::String(s2) = v2 {
                        let mut m = LuaPattern::new(s2.to_str()?);

                        if let Some(str_match) = m.match_maybe(s1.to_str()?) {
                            stack.replace(ctx, String::from_slice(&ctx, str_match));
                            return Ok(CallbackReturn::Return);
                        }
                    }
                }

                Err("Bad argument to string.match".into_value(ctx).into())
            }),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "string", string).unwrap();
}
