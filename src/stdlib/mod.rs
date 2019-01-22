use std::io::{self, Write};

use gc_arena::MutationContext;

use crate::{Callback, ContinuationResult, LuaContext, Table};

pub fn load_base<'gc>(mc: MutationContext<'gc, '_>, lc: LuaContext<'gc>, env: Table<'gc>) {
    let print = Callback::new(mc, |args| {
        let mut stdout = io::stdout();
        for i in 0..args.len() {
            args[i].display(&mut stdout)?;
            if i != args.len() - 1 {
                stdout.write_all(&b"\t"[..])?;
            }
        }
        stdout.write_all(&b"\n"[..])?;
        stdout.flush()?;
        Ok(ContinuationResult::Finish(vec![]))
    });

    env.set(mc, lc.interned_strings.new_string(mc, b"print"), print)
        .unwrap();
}
