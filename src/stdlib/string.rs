use gc_arena::MutationContext;

use crate::{Callback, CallbackReturn, Root, RuntimeError, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            "len",
            Callback::new_immediate(mc, |mc, args| {
                match args.get(0).copied().unwrap_or(Value::Nil).to_string(mc) {
                    Some(s) => Ok(CallbackReturn::Return(vec![Value::Integer(s.len())])),
                    None => Err(RuntimeError("Bad argument to len".into()).into()),
                }
            }),
        )
        .unwrap();

    env.set(mc, "string", string).unwrap();
}
