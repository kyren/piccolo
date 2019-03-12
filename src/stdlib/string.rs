use gc_arena::MutationContext;

use crate::{Callback, CallbackResult, Root, RuntimeError, String, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            String::new_static(b"len"),
            Callback::new_immediate(mc, |args| {
                let s = args.get(0).cloned().unwrap_or(Value::Nil);

                if let Value::String(s) = s {
                    let len = Value::Integer(s.len() as i64);
                    Ok(CallbackResult::Return(vec![len]))
                } else {
                    Err(RuntimeError(Value::String(String::new_static(
                        b"Argument must be a string.",
                    )))
                    .into())
                }
            }),
        )
        .unwrap();

    string
        .set(
            mc,
            String::new_static(b"byte"),
            Callback::new_immediate(mc, |args| {
                let s = match args.get(0).cloned().unwrap_or(Value::Nil) {
                    Value::String(s) => s,
                    _ => {
                        return Err(RuntimeError(Value::String(String::new_static(
                            b"Target must be a string.",
                        )))
                        .into());
                    }
                };
                let i = match args
                    .get(1)
                    .cloned()
                    .unwrap_or(Value::Integer(1))
                    .to_integer()
                {
                    Some(i) => i,
                    None => {
                        return Err(RuntimeError(Value::String(String::new_static(
                            b"Start index must be a number.",
                        )))
                        .into());
                    }
                };
                let j = match args
                    .get(2)
                    .cloned()
                    .unwrap_or(Value::Integer(i))
                    .to_integer()
                {
                    Some(j) => j,
                    None => {
                        return Err(RuntimeError(Value::String(String::new_static(
                            b"End index must be a number.",
                        )))
                        .into());
                    }
                };

                Ok(CallbackResult::Return(
                    match s.as_bytes().get(((i - 1) as usize)..=((j - 1) as usize)) {
                        Some(bytes) => bytes
                            .into_iter()
                            .map(|b| Value::Integer(*b as i64))
                            .collect::<Vec<_>>(),
                        None => vec![Value::Nil],
                    },
                ))
            }),
        )
        .unwrap();

    env.set(mc, String::new_static(b"string"), string).unwrap();
}
