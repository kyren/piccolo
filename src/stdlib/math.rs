use gc_arena::MutationContext;
use gc_sequence::{self as sequence, SequenceExt};

use crate::{Callback, CallbackResult, LuaRoot, RuntimeError, String, Table, Value};

pub fn load_math<'gc>(mc: MutationContext<'gc, '_>, _: LuaRoot<'gc>, env: Table<'gc>) {
    let math = Table::new(mc);

    math.set(
        mc,
        String::new_static(b"abs"),
        Callback::new(mc, |args| {
            sequence::done((move || match args.get(0).cloned().unwrap_or(Value::Nil) {
                Value::Integer(a) => Ok(CallbackResult::Return(vec![Value::Integer(a.abs())])),
                a => match a.to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.abs())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to abs",
                    )))
                    .into()),
                },
            })())
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"acos"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.acos())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to acos",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"asin"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.asin())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to asin",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"atan"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.atan())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to atan",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"atan2"),
        Callback::new(mc, |args| {
            sequence::done((move || match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_number(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_number(),
            ) {
                (Some(f), Some(g)) => Ok(CallbackResult::Return(vec![Value::Number(f.atan2(g))])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to atan2")))
                        .into(),
                ),
            })())
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"ceil"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.ceil())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to ceil",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"cos"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.cos())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to cos",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"cosh"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.cosh())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to cosh",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"deg"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.to_degrees())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to deg",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"exp"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(
                        std::f64::consts::E.powf(f),
                    )])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to exp",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"floor"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.floor())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to floor",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"fmod"),
        Callback::new(mc, |args| {
            sequence::done((move || match (
                args.get(0).cloned().unwrap_or(Value::Nil).to_number(),
                args.get(1).cloned().unwrap_or(Value::Nil).to_number(),
            ) {
                (Some(f), Some(g)) => Ok(CallbackResult::Return(vec![Value::Number(f % g)])),
                _ => Err(
                    RuntimeError(Value::String(String::new_static(b"Bad argument to fmod"))).into(),
                ),
            })())
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"frexp"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => {
                        let bits = f.to_bits();
                        // Set the exponent to exactly 01111111111_b, then put into the range of
                        // the result
                        let m = f64::from_bits(bits | (0x3ff << 52) & (!(1 << 62))) / 2.0;
                        // Extract the exponent, chop off the sign bit, and adjust the offset, then
                        // put into range of result
                        let e = ((bits >> 52) & 0x7ff) as i64 - 1023 + 1;

                        Ok(CallbackResult::Return(vec![
                            Value::Number(m),
                            Value::Integer(e),
                        ]))
                    }
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to frexp",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"huge"),
        Callback::new(mc, |_| {
            sequence::done((move || {
                Ok(CallbackResult::Return(vec![Value::Number(
                    std::f64::INFINITY,
                )]))
            })())
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"ldexp"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => {
                        let bits = f.to_bits();
                        // Set the exponent to exactly 01111111111_b, then put into the range of
                        // the result
                        let m = f64::from_bits(bits | (0x3ff << 52) & (!(1 << 62)));
                        // Extract the exponent, chop off the sign bit, and adjust the offset, then
                        // put into range of result
                        let e = ((bits >> 52) & 0x7ff) as i64 - 1023;

                        Ok(CallbackResult::Return(vec![
                            Value::Number(m),
                            Value::Integer(e),
                        ]))
                    }
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to ldexp",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"log"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.ln())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to log",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    math.set(
        mc,
        String::new_static(b"log10"),
        Callback::new(mc, |args| {
            sequence::done(
                (move || match args.get(0).cloned().unwrap_or(Value::Nil).to_number() {
                    Some(f) => Ok(CallbackResult::Return(vec![Value::Number(f.log10())])),
                    _ => Err(RuntimeError(Value::String(String::new_static(
                        b"Bad argument to log10",
                    )))
                    .into()),
                })(),
            )
            .boxed()
        }),
    )
    .unwrap();

    env.set(mc, String::new_static(b"math"), math).unwrap();
}
