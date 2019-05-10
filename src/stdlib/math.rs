use gc_arena::MutationContext;

use crate::{Callback, CallbackResult, Root, RuntimeError, String, Table, Value};

use rand::{FromEntropy, Rng, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use std::{cell::RefCell, ops::DerefMut, rc::Rc};

pub fn load_math<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let math = Table::new(mc);
    let seeded_rng: Rc<RefCell<Xoshiro256StarStar>> =
        Rc::new(RefCell::new(Xoshiro256StarStar::from_entropy()));

    math.set(
        mc,
        "abs",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil) {
                Value::Integer(a) => Ok(CallbackResult::Return(vec![Value::Integer(a.abs())])),
                a => match a.to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.abs())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to abs",
                    )))
                    .into()),
                },
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "acos",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.acos())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to acos"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "asin",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.asin())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to asin"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "atan",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.atan())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to atan"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "atan2",
        Callback::new_immediate(mc, |args| {
            match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_number(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_number(),
            ) {
                (Some(f), Some(g)) => Ok(CallbackResult::Return(vec![Value::Number(f.atan2(g))])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to atan2")))
                        .into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "ceil",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![
                    Value::Integer(f.ceil() as i64),
                ])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to ceil"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "cos",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.cos())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to cos"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "cosh",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.cosh())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to cosh"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "deg",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.to_degrees())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to deg"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "exp",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(
                    std::f64::consts::E.powf(f),
                )])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to exp"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "floor",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Integer(
                    f.floor() as i64
                )])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to floor")))
                        .into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "fmod",
        Callback::new_immediate(mc, |args| {
            match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_number(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_number(),
            ) {
                (Some(f), Some(g)) => {
                    let result = (f % g).abs();
                    Ok(CallbackResult::Return(vec![Value::Number(if f < 0.0 {
                        -result
                    } else {
                        result
                    })]))
                }
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to fmod"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "frexp",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) if f.is_finite() => {
                    let bits = f.to_bits();
                    // Set the exponent to exactly 01111111111_b, then put into the range of
                    // the result
                    let m = f64::from_bits((bits | (0x3ff << 52)) & (!(1 << 62))) / 2.0;
                    // Extract the exponent, chop off the sign bit, and adjust the offset, then
                    // put into range of result
                    let e = ((bits >> 52) & 0x7ff) as i64 - 1023 + 1;

                    Ok(CallbackResult::Return(vec![
                        Value::Number(m),
                        Value::Integer(e),
                    ]))
                }
                Some(f) => Ok(CallbackResult::Return(vec![
                    Value::Number(f),
                    Value::Integer(0),
                ])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to frexp")))
                        .into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(mc, "huge", Value::Number(std::f64::INFINITY))
        .unwrap();

    math.set(
        mc,
        "ldexp",
        Callback::new_immediate(mc, |args| {
            match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_number(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_number(),
            ) {
                (Some(f), Some(g)) => Ok(CallbackResult::Return(vec![Value::Number(
                    f * 2.0_f64.powf(g),
                )])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to ldexp")))
                        .into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "log",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.ln())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to log"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "log10",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.log10())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to log10")))
                        .into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "max",
        Callback::new_immediate(mc, |args| {
            if args.len() == 0 {
                return Err(RuntimeError(Value::String(String::new_static(
                    b"Bad argument to max",
                )))
                .into());
            }

            args.iter()
                .try_fold(Value::Number(-std::f64::INFINITY), |max, &entry| {
                    max.less_than(entry)
                        .ok_or(
                            RuntimeError(Value::String(String::new_static(b"Bad argument to max")))
                                .into(),
                        )
                        .and_then(|less| if less { Ok(entry) } else { Ok(max) })
                })
                .map(|a| CallbackResult::Return(vec![a]))
        }),
    )
    .unwrap();

    math.set(mc, "maxinteger", Value::Integer(std::i64::MAX))
        .unwrap();

    math.set(
        mc,
        "min",
        Callback::new_immediate(mc, |args| {
            if args.len() == 0 {
                return Err(RuntimeError(Value::String(String::new_static(
                    b"Bad argument to min",
                )))
                .into());
            }

            args.iter()
                .try_fold(Value::Number(std::f64::INFINITY), |min, &entry| {
                    entry
                        .less_than(min)
                        .ok_or(
                            RuntimeError(Value::String(String::new_static(b"Bad argument to min")))
                                .into(),
                        )
                        .and_then(|less| if less { Ok(entry) } else { Ok(min) })
                })
                .map(|a| CallbackResult::Return(vec![a]))
        }),
    )
    .unwrap();

    math.set(mc, "mininteger", Value::Integer(std::i64::MIN))
        .unwrap();

    math.set(
        mc,
        "modf",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![
                    Value::Integer(f as i64 / 1),
                    Value::Number(f % 1.0),
                ])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to modf"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(mc, "pi", Value::Number(std::f64::consts::PI))
        .unwrap();

    math.set(
        mc,
        "rad",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.to_radians())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to rad"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    // TODO: Random and Randomseed
    let random_rng = seeded_rng.clone();
    math.set(
        mc,
        "random",
        Callback::new_immediate(mc, move |args| {
            let rng = &random_rng;
            match (
                args.get(0).cloned().unwrap_or(Value::Nil),
                args.get(1).cloned().unwrap_or(Value::Nil),
            ) {
                (Value::Nil, Value::Nil) => Ok(CallbackResult::Return(vec![Value::Number(
                    rng.borrow_mut().gen::<f64>(),
                )])),
                (a, b) => {
                    if let (Some(first), Value::Nil) = (a.to_integer(), b) {
                        Ok(CallbackResult::Return(vec![Value::Integer(
                            rng.borrow_mut().gen_range(1, first + 1),
                        )]))
                    } else if let (Some(first), Some(second)) = (a.to_integer(), b.to_integer()) {
                        Ok(CallbackResult::Return(vec![Value::Integer(
                            rng.borrow_mut().gen_range(first, second + 1),
                        )]))
                    } else {
                        Err(RuntimeError(Value::String(String::new_static(
                            b"Bad argument to random",
                        )))
                        .into())
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
        Callback::new_immediate(mc, move |args| {
            let rng = &randomseed_rng;
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => {
                    *(rng.borrow_mut().deref_mut()) = Xoshiro256StarStar::seed_from_u64(f as u64);
                    Ok(CallbackResult::Return(vec![]))
                }
                _ => Err(RuntimeError(Value::String(String::new_static(
                    b"Bad argument to randomseed",
                )))
                .into()),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "sin",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.sin())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to sin"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "sqrt",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.sqrt())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to sqrt"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "tan",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.tan())])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to tan"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "tointeger",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil).to_integer() {
                Some(f) => Ok(CallbackResult::Return(vec![Value::Integer(f)])),
                _ => Ok(CallbackResult::Return(vec![Value::Nil])),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "type",
        Callback::new_immediate(mc, |args| {
            match args.get(0).cloned().unwrap_or(Value::Nil) {
                Value::Integer(_) => Ok(CallbackResult::Return(vec![Value::String(
                    String::new_static(b"integer"),
                )])),
                Value::Number(_) => Ok(CallbackResult::Return(vec![Value::String(
                    String::new_static(b"float"),
                )])),
                _ => Ok(CallbackResult::Return(vec![Value::Nil])),
            }
        }),
    )
    .unwrap();

    math.set(
        mc,
        "ult",
        Callback::new_immediate(mc, |args| {
            match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_integer(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_integer(),
            ) {
                (Some(f), Some(g)) => Ok(CallbackResult::Return(vec![Value::Boolean(
                    (f as u64) < (g as u64),
                )])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to ult"))).into(),
                ),
            }
        }),
    )
    .unwrap();

    env.set(mc, "math", math).unwrap();
}
