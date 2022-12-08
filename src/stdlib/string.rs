use gc_arena::MutationContext;
use gc_sequence as sequence;

use crate::{Callback, CallbackResult, Root, RuntimeError, String, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            String::new_static(b"len"),
            Callback::new_sequence(mc, |args| {
                Ok(sequence::from_fn_with(args, |mc, args| {
                    match args.get(0).cloned().unwrap_or(Value::Nil).to_string(mc) {
                        Some(s) => Ok(CallbackResult::Return(vec![Value::Integer(s.len())])),
                        None => Err(RuntimeError(Value::String(String::new_static(
                            b"Bad argument to len",
                        )))
                        .into()),
                    }
                }))
            }),
        )
        .unwrap();

    string
        .set(
            mc,
            String::new_static(b"char"),
            Callback::new_sequence(mc, |args| {
                Ok(sequence::from_fn_with(args, |mc, args| {
                    args.iter()
                        .try_fold(Vec::with_capacity(args.len()), |mut bytes, arg| {
                            match arg.to_integer() {
                                Some(i) => {
                                    if i >= 0 && i <= 255 {
                                        bytes.push(i as u8);
                                        Ok(bytes)
                                    } else {
                                        Err(RuntimeError(Value::String(String::new_static(
                                            b"Bad argument to char (value out of range)",
                                        )))
                                        .into())
                                    }
                                }
                                None => Err(RuntimeError(Value::String(String::new_static(
                                    b"Bad argument to char",
                                )))
                                .into()),
                            }
                        })
                        .map(|bytes| {
                            CallbackResult::Return(vec![Value::String(String::new(mc, &bytes))])
                        })
                }))
            }),
        )
        .unwrap();

    string
        .set(
            mc,
            String::new_static(b"upper"),
            Callback::new_sequence(mc, |args| {
                Ok(sequence::from_fn_with(args, |mc, args| {
                    match args.get(0).cloned().unwrap_or(Value::Nil).to_string(mc) {
                        Some(s) => Ok(CallbackResult::Return(vec![Value::String(s.upper(mc))])),
                        None => Err(RuntimeError(Value::String(String::new_static(
                            b"Bad argument to upper",
                        )))
                        .into()),
                    }
                }))
            }),
        )
        .unwrap();

    string
        .set(
            mc,
            String::new_static(b"lower"),
            Callback::new_sequence(mc, |args| {
                Ok(sequence::from_fn_with(args, |mc, args| {
                    match args.get(0).cloned().unwrap_or(Value::Nil).to_string(mc) {
                        Some(s) => Ok(CallbackResult::Return(vec![Value::String(s.lower(mc))])),
                        None => Err(RuntimeError(Value::String(String::new_static(
                            b"Bad argument to lower",
                        )))
                        .into()),
                    }
                }))
            }),
        )
        .unwrap();

    string
        .set(
            mc,
            String::new_static(b"reverse"),
            Callback::new_sequence(mc, |args| {
                Ok(sequence::from_fn_with(args, |mc, args| {
                    match args.get(0).cloned().unwrap_or(Value::Nil).to_string(mc) {
                        Some(s) => Ok(CallbackResult::Return(vec![Value::String(s.reverse(mc))])),
                        None => Err(RuntimeError(Value::String(String::new_static(
                            b"Bad argument to reverse",
                        )))
                        .into()),
                    }
                }))
            }),
        )
        .unwrap();

    env.set(mc, String::new_static(b"string"), string).unwrap();
}
