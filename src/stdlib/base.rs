use std::io::{self, Write};

use gc_arena::{Gc, MutationContext};

use crate::{
    sequence_fn_with, Callback, CallbackResult, LuaContext, RuntimeError, String, Table, Value,
};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, _: LuaContext<'gc>, env: Table<'gc>) {
    let print = Callback::new_immediate(mc, |_, args| {
        let mut stdout = io::stdout();
        for i in 0..args.len() {
            args[i].display(&mut stdout)?;
            if i != args.len() - 1 {
                stdout.write_all(&b"\t"[..])?;
            }
        }
        stdout.write_all(&b"\n"[..])?;
        stdout.flush()?;
        Ok(CallbackResult::Return(vec![]))
    });
    env.set(mc, String::new_static(b"print"), print).unwrap();

    let error = Callback::new_sequence(mc, |_, args| {
        let err = args.get(0).cloned().unwrap_or(Value::Nil);
        Ok(Box::new(sequence_fn_with(err, |mc, _, err| {
            Err(RuntimeError(Gc::allocate(mc, err)).into())
        })))
    });
    env.set(mc, String::new_static(b"error"), error).unwrap();
}
