use std::{cell::RefCell, f64, ops::DerefMut, rc::Rc};

use gc_arena::MutationContext;
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::{
    raw_ops, value::IntoValue, AnyCallback, CallbackReturn, Root, RuntimeError, String, Table,
    Value,
};

pub fn load_math<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    fn callback1<'gc>(
        name: &'static str,
        mc: MutationContext<'gc, '_>,
        f: impl Fn(MutationContext<'gc, '_>, Value<'gc>) -> Option<Value<'gc>> + 'static,
    ) -> AnyCallback<'gc> {
        AnyCallback::from_fn(mc, move |mc, stack| {
            if let Some(res) = f(mc, stack.get(0).copied().unwrap_or(Value::Nil)) {
                stack[0] = res;
                stack.drain(1..);
                Ok(CallbackReturn::Return.into())
            } else {
                Err(RuntimeError(
                    String::from_slice(mc, &format!("Bad argument to {name}").into_bytes()).into(),
                )
                .into())
            }
        })
    }

    fn callback2<'gc>(
        err: &'static str,
        mc: MutationContext<'gc, '_>,
        f: impl Fn(MutationContext<'gc, '_>, Value<'gc>, Value<'gc>) -> Option<Value<'gc>> + 'static,
    ) -> AnyCallback<'gc> {
        AnyCallback::from_fn(mc, move |mc, stack| {
            let a = stack.get(0).copied().unwrap_or(Value::Nil);
            let b = stack.get(1).copied().unwrap_or(Value::Nil);
            if let Some(res) = f(mc, a, b) {
                stack.clear();
                stack.push(res);
                Ok(CallbackReturn::Return.into())
            } else {
                Err(RuntimeError(err.into_value(mc)).into())
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
        callback1("abs", mc, |_, v| {
            Some(if let Value::Integer(i) = v {
                Value::Integer(i.abs())
            } else {
                v.to_number()?.abs().into()
            })
        }),
    )
    .unwrap();

    math.set(
        mc,
        "acos",
        callback1("acos", mc, |_, v| Some(v.to_number()?.acos().into())),
    )
    .unwrap();

    math.set(
        mc,
        "asin",
        callback1("asin", mc, |_, v| Some(v.to_number()?.asin().into())),
    )
    .unwrap();

    math.set(
        mc,
        "atan",
        callback2("atan", mc, |_, a, b| {
            Some(if b.is_nil() {
                a.to_number()?.atan().into()
            } else {
                a.to_number()?.atan2(b.to_number()?).into()
            })
        }),
    )
    .unwrap();

    math.set(
        mc,
        "ceil",
        callback1("ceil", mc, |_, v| {
            Some(to_int(v.to_number()?.ceil().into()))
        }),
    )
    .unwrap();

    math.set(
        mc,
        "cos",
        callback1("cos", mc, |_, v| Some(v.to_number()?.cos().into())),
    )
    .unwrap();

    math.set(
        mc,
        "deg",
        callback1("deg", mc, |_, v| Some(v.to_number()?.to_degrees().into())),
    )
    .unwrap();

    math.set(
        mc,
        "exp",
        callback1("exp", mc, |_, v| {
            Some(f64::consts::E.powf(v.to_number()?).into())
        }),
    )
    .unwrap();

    math.set(
        mc,
        "floor",
        callback1("floor", mc, |_, v| {
            Some(to_int(v.to_number()?.floor().into()))
        }),
    )
    .unwrap();

    math.set(
        mc,
        "fmod",
        callback2("fmod", mc, |_, f, g| {
            let f = f.to_number()?;
            let g = g.to_number()?;
            let result = (f % g).abs();
            Some(if f < 0.0 { -result } else { result }.into())
        }),
    )
    .unwrap();

    math.set(mc, "huge", Value::Number(f64::INFINITY)).unwrap();

    math.set(
        mc,
        "log",
        callback1("log", mc, |_, v| Some(v.to_number()?.ln().into())),
    )
    .unwrap();

    math.set(
        mc,
        "log10",
        callback1("log10", mc, |_, v| Some(v.to_number()?.log10().into())),
    )
    .unwrap();

    math.set(
        mc,
        "max",
        AnyCallback::from_fn(mc, |mc, stack| {
            if stack.len() == 0 {
                return Err(RuntimeError("Bad argument to max".into_value(mc)).into());
            }

            let m =
                stack
                    .drain(..)
                    .try_fold(Value::Number(-std::f64::INFINITY), |max, entry| {
                        raw_ops::less_than(max, entry)
                            .ok_or(RuntimeError("Bad argument to max".into_value(mc)))
                            .and_then(|less| if less { Ok(entry) } else { Ok(max) })
                    })?;

            stack.push(m);
            Ok(CallbackReturn::Return.into())
        }),
    )
    .unwrap();

    math.set(mc, "maxinteger", Value::Integer(i64::MAX))
        .unwrap();

    math.set(
        mc,
        "min",
        AnyCallback::from_fn(mc, |mc, stack| {
            if stack.len() == 0 {
                return Err(RuntimeError("Bad argument to min".into_value(mc)).into());
            }

            let m = stack
                .drain(..)
                .try_fold(Value::Number(std::f64::INFINITY), |min, entry| {
                    raw_ops::less_than(entry, min)
                        .ok_or(RuntimeError("Bad argument to min".into_value(mc)))
                        .and_then(|less| if less { Ok(entry) } else { Ok(min) })
                })?;

            stack.push(m);
            Ok(CallbackReturn::Return.into())
        }),
    )
    .unwrap();

    math.set(mc, "mininteger", Value::Integer(i64::MIN))
        .unwrap();

    math.set(
        mc,
        "modf",
        AnyCallback::from_fn(mc, |mc, stack| {
            match stack.get(0).copied().unwrap_or(Value::Nil).to_number() {
                Some(f) => {
                    stack.clear();
                    stack.extend([Value::Integer(f as i64 / 1), Value::Number(f % 1.0)]);
                    Ok(CallbackReturn::Return.into())
                }
                _ => Err(RuntimeError("Bad argument to modf".into_value(mc)).into()),
            }
        }),
    )
    .unwrap();

    math.set(mc, "pi", Value::Number(f64::consts::PI)).unwrap();

    math.set(
        mc,
        "rad",
        callback1("rad", mc, |_, v| Some(v.to_number()?.to_radians().into())),
    )
    .unwrap();

    let random_rng = seeded_rng.clone();
    math.set(
        mc,
        "random",
        callback2("random", mc, move |_, a, b| {
            let rng = &random_rng;
            match (a, b) {
                (Value::Nil, Value::Nil) => Some(rng.borrow_mut().gen::<f64>().into()),
                (a, b) => {
                    if let (Some(first), Value::Nil) = (a.to_integer(), b) {
                        Some(rng.borrow_mut().gen_range(1..first + 1).into())
                    } else if let (Some(first), Some(second)) = (a.to_integer(), b.to_integer()) {
                        Some(rng.borrow_mut().gen_range(first..second + 1).into())
                    } else {
                        None
                    }
                }
            }
        }),
    )
    .unwrap();

    let randomseed_rng = seeded_rng.clone();
    math.set(
        mc,
        "randomseed",
        AnyCallback::from_fn(mc, move |mc, stack| {
            let rng = &randomseed_rng;
            match stack.get(0).copied().unwrap_or(Value::Nil).to_number() {
                Some(f) => {
                    *(rng.borrow_mut().deref_mut()) = SmallRng::seed_from_u64(f as u64);
                    stack.clear();
                    Ok(CallbackReturn::Return.into())
                }
                _ => Err(RuntimeError("Bad argument to randomseed".into_value(mc)).into()),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "sin",
        callback1("sin", mc, |_, v| Some(v.to_number()?.sin().into())),
    )
    .unwrap();

    math.set(
        mc,
        "sqrt",
        callback1("sqrt", mc, |_, v| Some(v.to_number()?.sqrt().into())),
    )
    .unwrap();

    math.set(
        mc,
        "tan",
        callback1("tan", mc, |_, v| Some(v.to_number()?.tan().into())),
    )
    .unwrap();

    math.set(
        mc,
        "tointeger",
        callback1("tointeger", mc, |_, v| {
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
        callback1("type", mc, |mc, v| {
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
        callback2("ult", mc, |_, a, b| {
            if let (Some(f), Some(g)) = (a.to_integer(), b.to_integer()) {
                Some(Value::Boolean((f as u64) < (g as u64)))
            } else {
                None
            }
        }),
    )
    .unwrap();

    env.set(mc, "math", math).unwrap();
}
