use gc_arena::Collect;

use crate::{
    BoxSequence, Callback, CallbackReturn, Context, Error, IntoValue, Sequence, SequencePoll,
    String, Table, Value,
};

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
                let lowered = String::from_slice(
                    &ctx,
                    s.as_bytes()
                        .iter()
                        .map(|b| {
                            if (65..=90).contains(b) {
                                97 + (*b - 65)
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

    #[derive(Collect)]
    #[collect(require_static)]
    struct StringRepSeq {
        string: Vec<u8>,
        sep: Vec<u8>,
        n: i64,
        i: i64,

        built: Vec<u8>,
    }

    impl<'gc> Sequence<'gc> for StringRepSeq {
        fn poll(
            &mut self,
            ctx: Context<'gc>,
            mut exec: crate::Execution<'gc, '_>,
            mut stack: crate::Stack<'gc, '_>,
        ) -> Result<SequencePoll<'gc>, Error<'gc>> {
            if self.i < self.n {
                self.i += 1;

                exec.fuel().consume(1);
                self.built.extend(&self.string);
                if self.i < self.n {
                    self.built.extend(&self.sep);
                }
                Ok(SequencePoll::Pending)
            } else {
                stack.replace(ctx, String::from_slice(&ctx, &self.built));
                Ok(SequencePoll::Return)
            }
        }
    }

    string
        .set(
            ctx,
            "rep",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let (string, n, sep): (String, i64, Option<String>) = stack.consume(ctx)?;
                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    StringRepSeq {
                        string: string.to_vec(),
                        sep: sep.map(|s| s.as_bytes()).unwrap_or(b"").to_vec(),
                        n,
                        i: 0,
                        built: vec![],
                    },
                )))
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
                let uppered = String::from_slice(
                    &ctx,
                    s.as_bytes()
                        .iter()
                        .map(|b| {
                            if (97..=122).contains(b) {
                                65 + (*b - 97)
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
