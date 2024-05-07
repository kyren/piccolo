use crate::{Callback, CallbackReturn, Context, IntoValue, Table, TypeError, Value};

pub fn load_utf8<'gc>(ctx: Context<'gc>) {
    let utf8 = Table::new(&ctx);

    utf8.set(
        ctx,
        "char",
        Callback::from_fn(&ctx, |ctx, mut exec, mut stack| {
            let mut string = String::new();
            for argn in 0..stack.len() {
                exec.fuel().consume(1);
                let codepoint = match stack.pop_front() {
                    Value::Integer(i) if i >= 0 && i <= char::MAX as i64 => Ok(i as u32),
                    Value::Number(n)
                        if n >= 0.0 && n.fract() == 0.0 && n <= char::MAX as u32 as f64 =>
                    {
                        Ok(n as u32)
                    }
                    Value::String(s) => String::from_utf8(s.to_vec())
                        .map_err(|_| format!("failed to decode argument #{argn} as UTF-8"))
                        .and_then(|s| {
                            s.parse::<f64>()
                                .map_err(|_| {
                                    format!("failed to parse argument #{argn} as a number")
                                })
                                .and_then(|f| {
                                    (f >= 0.0 && f.fract() == 0.0 && f <= char::MAX as u32 as f64)
                                        .then_some(f as u32)
                                        .ok_or(format!(
                                            "argument #{argn} has no integer representation"
                                        ))
                                })
                        }),
                    v => Err(TypeError {
                        expected: "valid UTF-8 codepoint (string, number, or integer)",
                        found: v.type_name(),
                    }
                    .to_string()),
                }
                .map_err(|s| s.into_value(ctx))?;

                if let Some(c) = char::from_u32(codepoint) {
                    string.push(c);
                } else {
                    return Err(format!(
                        "argument #{argn}: {codepoint:x} is not a valid codepoint"
                    )
                    .into_value(ctx)
                    .into());
                }
            }
            stack.replace(ctx, string);
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    ctx.set_global("utf8", utf8).unwrap();
}
