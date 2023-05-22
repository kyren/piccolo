use std::{cell::RefCell, f64, ops::DerefMut, rc::Rc};

use gc_arena::Mutation;
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::{
    conversion::Variadic, raw_ops, AnyCallback, CallbackReturn, FromMultiValue, IntoMultiValue,
    IntoValue, Root, Table, Value,
};

pub fn load_math<'gc>(mc: &Mutation<'gc>, root: Root<'gc>) {
    fn callback<'gc, F, A, R>(name: &'static str, mc: &Mutation<'gc>, f: F) -> AnyCallback<'gc>
    where
        F: Fn(&Mutation<'gc>, A) -> Option<R> + 'static,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        AnyCallback::from_fn(mc, move |mc, stack| {
            if let Some(res) = f(mc, stack.consume(mc)?) {
                stack.replace(mc, res);
                Ok(CallbackReturn::Return)
            } else {
                Err(format!("Bad argument to {name}").into_value(mc).into())
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

    let math = Table::new(mc);
    let seeded_rng: Rc<RefCell<SmallRng>> = Rc::new(RefCell::new(SmallRng::from_entropy()));

    math.set(
        mc,
        "abs",
        callback("abs", mc, |_, v: Value| {
            Some(if let Value::Integer(i) = v {
                Value::Integer(i.abs())
            } else {
                v.to_number()?.abs().into()
            })
        }),
    )
    .unwrap();

    math.set(mc, "acos", callback("acos", mc, |_, v: f64| Some(v.acos())))
        .unwrap();

    math.set(mc, "asin", callback("asin", mc, |_, v: f64| Some(v.asin())))
        .unwrap();

    math.set(
        mc,
        "atan",
        callback("atan", mc, |_, (a, b): (f64, Option<f64>)| {
            Some(if let Some(b) = b {
                a.atan2(b)
            } else {
                a.atan()
            })
        }),
    )
    .unwrap();

    math.set(
        mc,
        "ceil",
        callback("ceil", mc, |_, v: f64| Some(to_int(v.ceil().into()))),
    )
    .unwrap();

    math.set(mc, "cos", callback("cos", mc, |_, v: f64| Some(v.cos())))
        .unwrap();

    math.set(
        mc,
        "deg",
        callback("deg", mc, |_, v: f64| Some(v.to_degrees())),
    )
    .unwrap();

    math.set(
        mc,
        "exp",
        callback("exp", mc, |_, v: f64| Some(f64::consts::E.powf(v))),
    )
    .unwrap();

    math.set(
        mc,
        "floor",
        callback("floor", mc, |_, v: f64| Some(to_int(v.floor().into()))),
    )
    .unwrap();

    math.set(
        mc,
        "fmod",
        callback("fmod", mc, |_, (f, g): (f64, f64)| {
            let result = (f % g).abs();
            Some(if f < 0.0 { -result } else { result })
        }),
    )
    .unwrap();

    math.set(mc, "huge", Value::Number(f64::INFINITY)).unwrap();

    math.set(mc, "log", callback("log", mc, |_, v: f64| Some(v.ln())))
        .unwrap();

    math.set(
        mc,
        "log10",
        callback("log10", mc, |_, v: f64| Some(v.log10())),
    )
    .unwrap();

    math.set(
        mc,
        "max",
        callback("max", mc, |_, v: Variadic<Value>| {
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

    math.set(mc, "maxinteger", Value::Integer(i64::MAX))
        .unwrap();

    math.set(
        mc,
        "min",
        callback("min", mc, |_, v: Variadic<Value>| {
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

    math.set(mc, "mininteger", Value::Integer(i64::MIN))
        .unwrap();

    math.set(
        mc,
        "modf",
        callback("modf", mc, |_, f: f64| Some((f as i64, f % 1.0))),
    )
    .unwrap();

    math.set(mc, "pi", Value::Number(f64::consts::PI)).unwrap();

    math.set(
        mc,
        "rad",
        callback("rad", mc, |_, v: f64| Some(v.to_radians())),
    )
    .unwrap();

    let random_rng = seeded_rng.clone();
    math.set(
        mc,
        "random",
        callback(
            "random",
            mc,
            move |_, (a, b): (Option<i64>, Option<i64>)| -> Option<Value> {
                let rng = &random_rng;
                match (a, b) {
                    (None, None) => Some(rng.borrow_mut().gen::<f64>().into()),
                    (Some(a), None) => Some(rng.borrow_mut().gen_range(1..a + 1).into()),
                    (Some(a), Some(b)) => Some(rng.borrow_mut().gen_range(a..b + 1).into()),
                    _ => None,
                }
            },
        ),
    )
    .unwrap();

    let randomseed_rng = seeded_rng.clone();
    math.set(
        mc,
        "randomseed",
        callback("randomseed", mc, move |_, f: i64| {
            let rng = &randomseed_rng;
            *(rng.borrow_mut().deref_mut()) = SmallRng::seed_from_u64(f as u64);
            Some(())
        }),
    )
    .unwrap();

    math.set(mc, "sin", callback("sin", mc, |_, v: f64| Some(v.sin())))
        .unwrap();

    math.set(mc, "sqrt", callback("sqrt", mc, |_, v: f64| Some(v.sqrt())))
        .unwrap();

    math.set(mc, "tan", callback("tan", mc, |_, v: f64| Some(v.tan())))
        .unwrap();

    math.set(
        mc,
        "tointeger",
        callback("tointeger", mc, |_, v: Value| {
            Some(if let Some(i) = v.to_integer() {
                i.into()
            } else {
                Value::Nil
            })
        }),
    )
    .unwrap();

    math.set(
        mc,
        "type",
        callback("type", mc, |mc, v: Value| {
            Some(match v {
                Value::Integer(_) => "integer".into_value(mc),
                Value::Number(_) => "float".into_value(mc),
                _ => Value::Nil,
            })
        }),
    )
    .unwrap();

    math.set(
        mc,
        "ult",
        callback("ult", mc, |_, (a, b): (i64, i64)| {
            Some(Value::Boolean((a as u64) < (b as u64)))
        }),
    )
    .unwrap();

    root.globals.set(mc, "math", math).unwrap();
}
