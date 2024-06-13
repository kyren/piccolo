use gc_arena::Collect;

use crate::{
    BoxSequence, Callback, CallbackReturn, Context, Error, IntoValue, Sequence, SequencePoll,
    String, Table, TypeError, Value,
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
                let s = match stack.consume::<Value>(ctx)? {
                    Value::String(s) => s,
                    Value::Integer(i) => String::from_slice(&ctx, i.to_string()),
                    Value::Number(f) => String::from_slice(&ctx, f.to_string()),
                    v => {
                        return Err(TypeError {
                            expected: "string, integer or number",
                            found: v.type_name(),
                        }
                        .into())
                    }
                };
                let lowered = ctx.intern(
                    &s.as_bytes()
                        .iter()
                        .map(u8::to_ascii_lowercase)
                        .collect::<Vec<_>>(),
                );
                stack.replace(ctx, lowered);
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

                // TODO How much fuel should each repetition cost?
                // Right now, it uses an amount of fuel equal to the number of characters,
                // which is most likely *way* too much fuel.
                exec.fuel().consume(if self.i < self.n {
                    self.string
                        .len()
                        .saturating_add(self.sep.len())
                        .try_into()?
                } else {
                    self.string.len() as i32
                });
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
                let (string, n, sep): (Value, i64, Option<Value>) = stack.consume(ctx)?;
                let string = match string {
                    Value::String(s) => s,
                    Value::Integer(i) => String::from_slice(&ctx, i.to_string()),
                    Value::Number(f) => String::from_slice(&ctx, f.to_string()),
                    v => {
                        return Err(TypeError {
                            expected: "string, integer or number",
                            found: v.type_name(),
                        }
                        .into())
                    }
                };
                let sep = sep
                    .map(|sep| match sep {
                        Value::String(s) => Ok(s),
                        Value::Integer(i) => Ok(String::from_slice(&ctx, i.to_string())),
                        Value::Number(f) => Ok(String::from_slice(&ctx, f.to_string())),
                        v => Err(TypeError {
                            expected: "string, integer or number",
                            found: v.type_name(),
                        }),
                    })
                    .transpose()?;
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
                let s = match stack.consume::<Value>(ctx)? {
                    Value::String(s) => s,
                    Value::Integer(i) => String::from_slice(&ctx, i.to_string()),
                    Value::Number(f) => String::from_slice(&ctx, f.to_string()),
                    v => {
                        return Err(TypeError {
                            expected: "string, integer or number",
                            found: v.type_name(),
                        }
                        .into())
                    }
                };
                stack.replace(
                    ctx,
                    ctx.intern(&s.as_bytes().iter().copied().rev().collect::<Vec<_>>()),
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
                let s = match stack.consume::<Value>(ctx)? {
                    Value::String(s) => s,
                    Value::Integer(i) => String::from_slice(&ctx, i.to_string()),
                    Value::Number(f) => String::from_slice(&ctx, f.to_string()),
                    v => {
                        return Err(TypeError {
                            expected: "string, integer or number",
                            found: v.type_name(),
                        }
                        .into())
                    }
                };
                let uppered = ctx.intern(
                    &s.as_bytes()
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
