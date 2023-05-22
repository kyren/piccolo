use gc_arena::Mutation;

use crate::{AnyCallback, CallbackReturn, IntoValue, Root, Table, Value};

pub fn load_string<'gc>(mc: &Mutation<'gc>, root: Root<'gc>) {
    let string = Table::new(mc);

    string
        .set(
            mc,
            "len",
            AnyCallback::from_fn(mc, |mc, stack| {
                let v: Option<Value> = stack.consume(mc)?;
                if let Some(s) = v.and_then(|v| v.to_string(mc)) {
                    stack.replace(mc, s.len());
                    Ok(CallbackReturn::Return)
                } else {
                    Err("Bad argument to len".into_value(mc).into())
                }
            }),
        )
        .unwrap();

    root.globals.set(mc, "string", string).unwrap();
}
