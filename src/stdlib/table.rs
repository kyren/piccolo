use crate::{AnyCallback, CallbackReturn, Context, Table};

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table
        .set(
            ctx,
            "pack",
            AnyCallback::from_fn(&ctx, |ctx, _, stack| {
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

    ctx.state.globals.set(ctx, "table", table).unwrap();
}
