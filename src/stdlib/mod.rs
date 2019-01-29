use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{Callback, CallbackResult, Error, LuaContext, Table};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, lc: LuaContext<'gc>, env: Table<'gc>) {
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

    let error = Callback::new_immediate(mc, |_, args| {
        if args.len() > 0 {
            let mut buf = Vec::new();
            args[0].display(&mut buf)?;
            Err(Error::RuntimeError(Some(
                String::from_utf8_lossy(&buf).into_owned(),
            )))
        } else {
            Err(Error::RuntimeError(None))
        }
    });

    env.set(mc, lc.interned_strings.new_string(mc, b"print"), print)
        .unwrap();
    env.set(mc, lc.interned_strings.new_string(mc, b"error"), error)
        .unwrap();
}
