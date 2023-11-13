use crate::{AnyCallback, CallbackReturn, Context, Table};

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table
        .set(
            ctx,
            "pack",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let t = Table::new(&ctx);
                for i in 0..stack.len() {
                    t.set(ctx, i as i64 + 1, stack[i]).unwrap();
                }
                t.set(ctx, "n", stack.len() as i64).unwrap();
                stack.replace(ctx, t);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "unpack",
            AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let (table, start, end): (Table<'gc>, Option<i64>, Option<i64>) =
                    stack.consume(ctx)?;
                let start = start.unwrap_or(1);
                let end = end.unwrap_or_else(|| table.length());

                if start <= end {
                    stack.resize((end - start + 1) as usize);
                    for i in start..=end {
                        stack[(i - start) as usize] = table.get_value(i.into());
                    }
                }

                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.state.globals.set(ctx, "table", table).unwrap();
}
