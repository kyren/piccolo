use crate::{Callback, CallbackReturn, Context, IntoValue, String, Table, Value};

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
                        string.len().saturating_sub(i.unsigned_abs().try_into()?)
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

                    return Ok(if i >= j || i >= string.len() {
                        &[]
                    } else {
                        &string[i..j]
                    });
                }

                let (string, i, j) = stack.consume::<(Value, i64, Option<i64>)>(ctx)?;
                let string = match string {
                    Value::Integer(int) => {
                        ctx.intern(operate_sub(int.to_string().as_bytes(), i, j)?)
                    }
                    Value::Number(num) => {
                        ctx.intern(operate_sub(num.to_string().as_bytes(), i, j)?)
                    }
                    Value::String(string) => ctx.intern(operate_sub(string.as_bytes(), i, j)?),
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

    string
        .set(
            ctx,
            "lower",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let s: String = stack.consume(ctx)?;
                // Use some fixed table to go from "uppercase" to "lowercase"
                #[rustfmt::skip]
                const MAPPING_TABLE: [u8; 128] = [
                      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
                     16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
                     32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
                     48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
                     64,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,  91,  92,  93,  94,  95,
                     96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
                ];
                let lowered = String::from_slice(
                    &ctx,
                    s.as_bytes()
                        .iter()
                        .map(|b| {
                            if *b < 128 {
                                MAPPING_TABLE[*b as usize]
                            } else {
                                *b
                            }
                        })
                        .collect::<Vec<_>>(),
                );
                stack.replace(ctx, lowered.into_value(ctx));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "rep",
            Callback::from_fn(&ctx, |ctx, mut exec, mut stack| {
                let (s, n, sep): (String, i64, Option<String>) = stack.consume(ctx)?;
                if n > 0 {
                    let mut ret = Vec::new();
                    let sep = sep.map(|s| s.as_bytes()).unwrap_or(b"");
                    for _ in 0..n {
                        exec.fuel().consume(1);
                        ret.extend(s.as_bytes());
                        ret.extend(sep);
                    }
                    stack.replace(ctx, String::from_slice(&ctx, ret).into_value(ctx));
                } else {
                    stack.replace(ctx, "".into_value(ctx));
                }
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "reverse",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let s: String = stack.consume(ctx)?;
                stack.replace(
                    ctx,
                    String::from_slice(
                        &ctx,
                        s.as_bytes().iter().copied().rev().collect::<Vec<_>>(),
                    )
                    .into_value(ctx),
                );
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "upper",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let s: String = stack.consume(ctx)?;
                // Use some fixed table to go from "lowercase" to "uppercase"
                #[rustfmt::skip]
                const MAPPING_TABLE: [u8; 128] = [
                      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
                     16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
                     32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
                     48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
                     64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
                     80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
                     96,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
                     80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 123, 124, 125, 126, 127,
                ];
                let uppered = String::from_slice(
                    &ctx,
                    s.as_bytes()
                        .iter()
                        .map(|b| {
                            if *b < 128 {
                                MAPPING_TABLE[*b as usize]
                            } else {
                                *b
                            }
                        })
                        .collect::<Vec<_>>(),
                );
                stack.replace(ctx, uppered.into_value(ctx));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string).unwrap();
}
