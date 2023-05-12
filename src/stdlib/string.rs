use gc_arena::MutationContext;

use crate::{AnyCallback, CallbackReturn, Root, RuntimeError, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            "len",
            AnyCallback::from_immediate_fn(mc, |mc, v: Option<Value>| {
                if let Some(s) = v.and_then(|v| v.to_string(mc)) {
                    Ok((CallbackReturn::Return, s.len()))
                } else {
                    Err(RuntimeError::from_str(mc, "Bad argument to len").into())
                }
            }),
        )
        .unwrap();

    env.set(mc, "string", string).unwrap();
}
