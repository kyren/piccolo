use std::{cell::RefCell, f64, rc::Rc};

use gc_arena::Mutation;
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::{
    raw_ops, Callback, CallbackReturn, Context, FromMultiValue, IntoMultiValue, IntoValue, Table,
    Value, Variadic,
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

    math.set(
        ctx,
        "abs",
        callback("abs", &ctx, |_, v: Value| {
            Some(if let Value::Integer(i) = v {
                Value::Integer(i.abs())
            } else {
                v.to_number()?.abs().into()
            })
        }),
    )
    .unwrap();

    math.set(
        ctx,
        "acos",
        callback("acos", &ctx, |_, v: f64| Some(v.acos())),
    )
    .unwrap();

    math.set(
        ctx,
        "asin",
        callback("asin", &ctx, |_, v: f64| Some(v.asin())),
    )
    .unwrap();

    math.set(
        ctx,
        "atan",
        callback("atan", &ctx, |_, (a, b): (f64, Option<f64>)| {
            Some(if let Some(b) = b {
                a.atan2(b)
            } else {
                a.atan()
            })
        }),
    )
    .unwrap();

    math.set(
        ctx,
        "ceil",
        callback("ceil", &ctx, |_, v: f64| Some(to_int(v.ceil().into()))),
    )
    .unwrap();

    math.set(ctx, "cos", callback("cos", &ctx, |_, v: f64| Some(v.cos())))
        .unwrap();

    math.set(
        ctx,
        "deg",
        callback("deg", &ctx, |_, v: f64| Some(v.to_degrees())),
    )
    .unwrap();

    math.set(
        ctx,
        "exp",
        callback("exp", &ctx, |_, v: f64| Some(f64::consts::E.powf(v))),
    )
    .unwrap();

    math.set(
        ctx,
        "floor",
        callback("floor", &ctx, |_, v: f64| Some(to_int(v.floor().into()))),
    )
    .unwrap();

    math.set(
        ctx,
        "fmod",
        callback("fmod", &ctx, |_, (f, g): (f64, f64)| {
            let result = (f % g).abs();
            Some(if f < 0.0 { -result } else { result })
        }),
    )
    .unwrap();

    math.set(ctx, "huge", Value::Number(f64::INFINITY)).unwrap();

    math.set(
        ctx,
        "log",
        callback("log", &ctx, |_, (v, base): (f64, Option<f64>)| match base {
            None => Some(v.ln()),
            Some(base) => Some(v.log(base)),
        }),
    )
    .unwrap();

    math.set(
        ctx,
        "max",
        callback("max", &ctx, |_, v: Variadic<Vec<Value>>| {
            if v.is_empty() {
                None
            } else {
                v.into_iter()
                    .try_fold(Value::Number(-f64::INFINITY), |max, entry| {
                        Some(if raw_ops::less_than(max, entry)? {
                            entry
                        } else {
                            max
                        })
                    })
            }
        }),
    )
    .unwrap();

    math.set(ctx, "maxinteger", Value::Integer(i64::MAX))
        .unwrap();

    math.set(
        ctx,
        "min",
        callback("min", &ctx, |_, v: Variadic<Vec<Value>>| {
            if v.is_empty() {
                None
            } else {
                v.into_iter()
                    .try_fold(Value::Number(f64::INFINITY), |max, entry| {
                        Some(if raw_ops::less_than(entry, max)? {
                            entry
                        } else {
                            max
                        })
                    })
            }
        }),
    )
    .unwrap();

    math.set(ctx, "mininteger", Value::Integer(i64::MIN))
        .unwrap();

    math.set(
        ctx,
        "modf",
        callback("modf", &ctx, |_, f: f64| Some((f as i64, f % 1.0))),
    )
    .unwrap();

    math.set(ctx, "pi", Value::Number(f64::consts::PI)).unwrap();

    math.set(
        ctx,
        "rad",
        callback("rad", &ctx, |_, v: f64| Some(v.to_radians())),
    )
    .unwrap();

    let random_rng = seeded_rng.clone();
    math.set(
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
    )
    .unwrap();

    let randomseed_rng = seeded_rng.clone();
    math.set(
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
                        let seed = {
                            let mut seed = [0; 32];
                            let high_bytes = high.to_ne_bytes();
                            let low_bytes = low.to_ne_bytes();
                            seed[..8].copy_from_slice(&low_bytes);
                            seed[8..16].copy_from_slice(&high_bytes);
                            seed[16..24].copy_from_slice(&low_bytes);
                            seed[24..].copy_from_slice(&high_bytes);
                            seed
                        };
                        *rng.borrow_mut() = SmallRng::from_seed(seed);
                        Some(())
                    }
                    _ => None,
                }
            },
        ),
    )
    .unwrap();

    math.set(ctx, "sin", callback("sin", &ctx, |_, v: f64| Some(v.sin())))
        .unwrap();

    math.set(
        ctx,
        "sqrt",
        callback("sqrt", &ctx, |_, v: f64| Some(v.sqrt())),
    )
    .unwrap();

    math.set(ctx, "tan", callback("tan", &ctx, |_, v: f64| Some(v.tan())))
        .unwrap();

    math.set(
        ctx,
        "tointeger",
        callback("tointeger", &ctx, |_, v: Value| {
            Some(if let Some(i) = v.to_integer() {
                i.into()
            } else {
                Value::Nil
            })
        }),
    )
    .unwrap();

    math.set(
        ctx,
        "type",
        callback("type", &ctx, |ctx, v: Value| {
            Some(match v {
                Value::Integer(_) => "integer".into_value(ctx),
                Value::Number(_) => "float".into_value(ctx),
                _ => Value::Nil,
            })
        }),
    )
    .unwrap();

    math.set(
        ctx,
        "ult",
        callback("ult", &ctx, |_, (a, b): (i64, i64)| {
            Some(Value::Boolean((a as u64) < (b as u64)))
        }),
    )
    .unwrap();

    ctx.set_global("math", math).unwrap();
}
