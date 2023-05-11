use gc_arena::MutationContext;

use crate::{AnyCallback, CallbackReturn, Root, RuntimeError, String, Table, Value};

pub fn load_string<'gc>(mc: MutationContext<'gc, '_>, _: Root<'gc>, env: Table<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            "len",
            AnyCallback::from_fn(mc, |mc, stack| {
                match stack.get(0).copied().unwrap_or(Value::Nil).to_string(mc) {
                    Some(s) => {
                        stack.clear();
                        stack.push(s.len().into());
                        Ok(CallbackReturn::Return.into())
                    }
                    None => Err(RuntimeError(
                        String::from_static(mc, "Bad argument to len").into(),
                    )
                    .into()),
                }
            })
            .into(),
        )
        .unwrap();

    env.set(mc, "string", string.into()).unwrap();
}
