use gc_arena::MutationContext;

use crate::{Callback, CallbackReturn, Root, RuntimeError, String, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            String::from_static(b"len"),
            Callback::new_immediate(mc, |mc, _, args| {
                match args.get(0).cloned().unwrap_or(Value::Nil).to_string(mc) {
                    Some(s) => Ok(CallbackReturn::Return(vec![Value::Integer(s.len())])),
                    None => Err(RuntimeError(Value::String(String::from_static(
                        b"Bad argument to len",
                    )))
                    .into()),
                }
            }),
        )
        .unwrap();

    env.set(mc, String::from_static(b"string"), string).unwrap();
}
