use gc_arena::MutationContext;

use crate::{raw_ops, Callback, CallbackReturn, Root, RuntimeError, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            "len",
            Callback::new_immediate(mc, |mc, _, args| {
                match raw_ops::to_string(mc, args.get(0).copied().unwrap_or(Value::Nil)) {
                    Some(s) => Ok(CallbackReturn::Return(vec![Value::Integer(s.len())])),
                    None => Err(RuntimeError("Bad argument to len".into()).into()),
                }
            }),
        )
        .unwrap();

    env.set(mc, "string", string).unwrap();
}
