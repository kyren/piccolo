use std::{cell::RefCell, f64, rc::Rc};

use gc_arena::Mutation;
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::{
    async_sequence, meta_ops, Callback, CallbackReturn, Context, FromMultiValue, IntoMultiValue,
    IntoValue, SequenceReturn, Table, Value,
};

pub fn load_math<'gc>(ctx: Context<'gc>) {
    fn callback<'gc, F, A, R>(name: &'static str, mc: &Mutation<'gc>, f: F) -> Callback<'gc>
    where
        F: Fn(Context<'gc>, A) -> Option<R> + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        Callback::from_fn(mc, move |ctx, _, mut stack| {
            if let Some(res) = f(ctx, stack.consume(ctx)?) {
                stack.replace(ctx, res);
                Ok(CallbackReturn::Return)
            } else {
                Err(format!("Bad argument to {name}").into_value(ctx).into())
            }
        })
    }

    fn to_int(v: Value) -> Value {
        if let Some(i) = v.to_integer() {
            Value::Integer(i)
        } else {
            v
        }
    }

    let math = Table::new(&ctx);
    let seeded_rng: Rc<RefCell<SmallRng>> = Rc::new(RefCell::new(SmallRng::from_entropy()));

    math.set_field(
        ctx,
        "abs",
        callback("abs", &ctx, |_, v: Value| {
            Some(if let Value::Integer(i) = v {
                Value::Integer(i.abs())
            } else {
                v.to_number()?.abs().into()
            })
        }),
    );

    math.set_field(
        ctx,
        "acos",
        callback("acos", &ctx, |_, v: f64| Some(v.acos())),
    );

    math.set_field(
        ctx,
        "asin",
        callback("asin", &ctx, |_, v: f64| Some(v.asin())),
    );

    math.set_field(
        ctx,
        "atan",
        callback("atan", &ctx, |_, (a, b): (f64, Option<f64>)| {
            Some(if let Some(b) = b {
                a.atan2(b)
            } else {
                a.atan()
            })
        }),
    );

    math.set_field(
        ctx,
        "ceil",
        callback("ceil", &ctx, |_, v: f64| Some(to_int(v.ceil().into()))),
    );

    math.set_field(ctx, "cos", callback("cos", &ctx, |_, v: f64| Some(v.cos())));

    math.set_field(
        ctx,
        "deg",
        callback("deg", &ctx, |_, v: f64| Some(v.to_degrees())),
    );

    math.set_field(
        ctx,
        "exp",
        callback("exp", &ctx, |_, v: f64| Some(f64::consts::E.powf(v))),
    );

    math.set_field(
        ctx,
        "floor",
        callback("floor", &ctx, |_, v: f64| Some(to_int(v.floor().into()))),
    );

    math.set_field(
        ctx,
        "fmod",
        callback("fmod", &ctx, |_, (f, g): (f64, f64)| {
            let result = (f % g).abs();
            Some(if f < 0.0 { -result } else { result })
        }),
    );

    math.set_field(ctx, "huge", Value::Number(f64::INFINITY));

    math.set_field(
        ctx,
        "log",
        callback("log", &ctx, |_, (v, base): (f64, Option<f64>)| match base {
            None => Some(v.ln()),
            Some(base) => Some(v.log(base)),
        }),
    );

    math.set_field(
        ctx,
        "max",
        Callback::from_fn(&ctx, |ctx, _, stack| {
            if stack.is_empty() {
                return Err("value expected".into_value(ctx).into());
            }
            let s = async_sequence(&ctx, |locals, mut seq| {
                let mut max = locals.stash(&ctx, stack.get(0));
                let args = stack.len();
                async move {
                    for i in 1..args {
                        let (call, bottom) = seq.try_enter(|ctx, locals, _, mut stack| {
                            let bottom = args;
                            match meta_ops::less_than(ctx, locals.fetch(&max), stack[i])? {
                                meta_ops::MetaResult::Value(v) => {
                                    stack.resize(bottom);
                                    stack.push_back(v);
                                    Ok((None, bottom))
                                }
                                meta_ops::MetaResult::Call(meta_ops::MetaCall {
                                    function,
                                    args,
                                }) => {
                                    stack.resize(bottom);
                                    stack.extend(args);
                                    Ok((Some(locals.stash(&ctx, function)), bottom))
                                }
                            }
                        })?;
                        if let Some(func) = call {
                            seq.call(&func, bottom).await?;
                        }
                        seq.enter(|ctx, locals, _, stack| {
                            if stack.get(bottom).to_bool() {
                                max = locals.stash(&ctx, stack.get(i))
                            }
                        });
                    }
                    seq.enter(|ctx, locals, _, mut stack| {
                        stack.replace(ctx, locals.fetch(&max));
                    });
                    Ok(SequenceReturn::Return)
                }
            });
            Ok(CallbackReturn::Sequence(s))
        }),
    );

    math.set_field(
        ctx,
        "min",
        Callback::from_fn(&ctx, |ctx, _, stack| {
            if stack.is_empty() {
                return Err("value expected".into_value(ctx).into());
            }
            let s = async_sequence(&ctx, |locals, mut seq| {
                let mut min = locals.stash(&ctx, stack.get(0));
                let args = stack.len();
                async move {
                    for i in 1..args {
                        let (call, bottom) = seq.try_enter(|ctx, locals, _, mut stack| {
                            let bottom = args;
                            match meta_ops::less_than(ctx, stack.get(i), locals.fetch(&min))? {
                                meta_ops::MetaResult::Value(v) => {
                                    stack.resize(bottom);
                                    stack.push_back(v);
                                    Ok((None, bottom))
                                }
                                meta_ops::MetaResult::Call(meta_ops::MetaCall {
                                    function,
                                    args,
                                }) => {
                                    stack.resize(bottom);
                                    stack.extend(args);
                                    Ok((Some(locals.stash(&ctx, function)), bottom))
                                }
                            }
                        })?;
                        if let Some(func) = call {
                            seq.call(&func, bottom).await?;
                        }
                        seq.enter(|ctx, locals, _, stack| {
                            if stack.get(bottom).to_bool() {
                                min = locals.stash(&ctx, stack.get(i))
                            }
                        });
                    }
                    seq.enter(|ctx, locals, _, mut stack| {
                        stack.replace(ctx, locals.fetch(&min));
                    });
                    Ok(SequenceReturn::Return)
                }
            });
            Ok(CallbackReturn::Sequence(s))
        }),
    );

    math.set_field(ctx, "maxinteger", Value::Integer(i64::MAX));

    math.set_field(ctx, "mininteger", Value::Integer(i64::MIN));

    math.set_field(
        ctx,
        "modf",
        callback("modf", &ctx, |_, f: f64| Some((f as i64, f % 1.0))),
    );

    math.set_field(ctx, "pi", Value::Number(f64::consts::PI));

    math.set_field(
        ctx,
        "rad",
        callback("rad", &ctx, |_, v: f64| Some(v.to_radians())),
    );

    let random_rng = seeded_rng.clone();
    math.set_field(
        ctx,
        "random",
        callback(
            "random",
            &ctx,
            move |_, (a, b): (Option<i64>, Option<i64>)| -> Option<Value> {
                let rng = &random_rng;
                match (a, b) {
                    (None, None) => Some(rng.borrow_mut().gen::<f64>().into()),
                    (Some(0), None) => Some(rng.borrow_mut().gen::<i64>().into()),
                    (Some(a), None) if a < 0 => None,
                    (Some(a), None) => Some(rng.borrow_mut().gen_range(1..=a).into()),
                    (Some(a), Some(b)) if a <= b => Some(rng.borrow_mut().gen_range(a..=b).into()),
                    _ => None,
                }
            },
        ),
    );

    let randomseed_rng = seeded_rng.clone();
    math.set_field(
        ctx,
        "randomseed",
        callback(
            "randomseed",
            &ctx,
            move |_, (u, l): (Option<u64>, Option<u64>)| {
                let rng = &randomseed_rng;
                match (u, l) {
                    (None, None) => {
                        *rng.borrow_mut() = SmallRng::from_entropy();
                        Some(())
                    }
                    (Some(seed), None) | (Some(seed), Some(0)) => {
                        *rng.borrow_mut() = SmallRng::seed_from_u64(seed);
                        Some(())
                    }
                    (Some(high), Some(low)) => {
                        let high_bytes = high.to_ne_bytes();
                        let low_bytes = low.to_ne_bytes();
                        let seed = std::array::from_fn(|idx| {
                            let idx_mod_16 = idx % 16;
                            if idx_mod_16 >= 8 {
                                high_bytes[idx_mod_16 - 8]
                            } else {
                                low_bytes[idx_mod_16]
                            }
                        });
                        *rng.borrow_mut() = SmallRng::from_seed(seed);
                        Some(())
                    }
                    _ => None,
                }
            },
        ),
    );

    math.set_field(ctx, "sin", callback("sin", &ctx, |_, v: f64| Some(v.sin())));

    math.set_field(
        ctx,
        "sqrt",
        callback("sqrt", &ctx, |_, v: f64| Some(v.sqrt())),
    );

    math.set_field(ctx, "tan", callback("tan", &ctx, |_, v: f64| Some(v.tan())));

    math.set_field(
        ctx,
        "tointeger",
        callback("tointeger", &ctx, |_, v: Value| {
            Some(if let Some(i) = v.to_integer() {
                i.into()
            } else {
                Value::Nil
            })
        }),
    );

    math.set_field(
        ctx,
        "type",
        callback("type", &ctx, |ctx, v: Value| {
            Some(match v {
                Value::Integer(_) => "integer".into_value(ctx),
                Value::Number(_) => "float".into_value(ctx),
                _ => Value::Nil,
            })
        }),
    );

    math.set_field(
        ctx,
        "ult",
        callback("ult", &ctx, |_, (a, b): (i64, i64)| {
            Some(Value::Boolean((a as u64) < (b as u64)))
        }),
    );

    ctx.set_global("math", math);
}
