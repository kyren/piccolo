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
                    _ => Err(RuntimeError(Value::String(String::new_static(b"Bad argument to abs"))).into()),
                },
            })())
            .boxed()
        }),
    ).unwrap();
    
    env.set(mc, String::new_static(b"math"), math)
        .unwrap();
}
