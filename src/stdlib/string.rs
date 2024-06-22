use crate::{Callback, CallbackReturn, Context, String, Table};

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string
        .set(
            ctx,
            "len",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let len = string.len();
                stack.replace(ctx, len);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
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
        )
        .unwrap();

    string
        .set(
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
        )
        .unwrap();

    string
        .set(
            ctx,
            "reverse",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let reversed =
                    ctx.intern(&string.as_bytes().iter().copied().rev().collect::<Vec<_>>());
                stack.replace(ctx, reversed);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
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
        )
        .unwrap();

    ctx.set_global("string", string).unwrap();
}
