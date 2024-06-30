use crate::{Callback, CallbackReturn, Context, IntoValue, String, Table, Value};

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string.set_field(
        ctx,
        "len",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let len = string.len();
            stack.replace(ctx, len);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "sub",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            fn operate_sub(
                string: &[u8],
                i: i64,
                j: Option<i64>,
            ) -> Result<&[u8], std::num::TryFromIntError> {
                let i = match i {
                    i if i > 0 => i.saturating_sub(1).try_into()?,
                    0 => 0,
                    i => string.len().saturating_sub(i.unsigned_abs().try_into()?),
                };
                let j = if let Some(j) = j {
                    if j >= 0 {
                        j.try_into()?
                    } else {
                        let j: usize = j.unsigned_abs().try_into()?;
                        string.len().saturating_sub(j.saturating_sub(1))
                    }
                } else {
                    string.len()
                }
                .clamp(0, string.len());

                Ok(if i >= j || i >= string.len() {
                    &[]
                } else {
                    &string[i..j]
                })
            }

            let (string, i, j) = stack.consume::<(String, i64, Option<i64>)>(ctx)?;
            let substr = ctx.intern(operate_sub(string.as_bytes(), i, j)?);
            stack.replace(ctx, substr);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "lower",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let lowered = ctx.intern(
                &string
                    .as_bytes()
                    .iter()
                    .map(u8::to_ascii_lowercase)
                    .collect::<Vec<_>>(),
            );
            stack.replace(ctx, lowered);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "reverse",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let reversed = ctx.intern(&string.as_bytes().iter().copied().rev().collect::<Vec<_>>());
            stack.replace(ctx, reversed);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "upper",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let uppered = ctx.intern(
                &string
                    .as_bytes()
                    .iter()
                    .map(u8::to_ascii_uppercase)
                    .collect::<Vec<_>>(),
            );
            stack.replace(ctx, uppered);
            Ok(CallbackReturn::Return)
        }),
    );

    string
        .set(
            ctx,
            "char",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let bytes = stack
                    .drain(..)
                    .map(|v| try_to_byte(v))
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(|e| e.into_value(ctx))?;
                stack.replace(ctx, ctx.intern(&bytes));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn try_to_byte<'gc>(v: Value<'gc>) -> Result<u8, &'static str> {
        match v.to_integer() {
            Some(n @ 0..=255) => Ok(n as u8),
            Some(_) => Err("value out of range"),
            None => Err("expected integer"),
        }
    }

    string
        .set(
            ctx,
            "rep",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let (string, count) = stack.consume::<(String, i64)>(ctx)?;
                let repeated = string.repeat(count as usize);
                stack.replace(ctx, ctx.intern(&repeated));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string);
}
