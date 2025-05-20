extern crate alloc;

use alloc::string::String as StdString;

use piccolo::{meta_ops::MetaCallError, Closure, Executor, Lua};

const SOURCE: &str = r#"
    -- Purposeful typo of 'tostring'
    return tosting("hello")
"#;

#[test]
fn tail_call_stack_panic() {
    let mut lua = Lua::core();

    let exec = lua.enter(|ctx| ctx.stash(Executor::new(ctx)));

    lua.try_enter(|ctx| {
        let closure = Closure::load(ctx, None, SOURCE.as_bytes())?;
        ctx.fetch(&exec).restart(ctx, closure.into(), ());
        Ok(())
    })
    .expect("load closure");

    assert!(matches!(
        lua.execute::<StdString>(&exec),
        Err(err) if err.root_cause().downcast_ref::<MetaCallError>().is_some()
    ));
}
