use std::string::String as StdString;

use piccolo::{thread::VMError, Closure, Executor, Lua, StaticError};

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
        Err(StaticError::Runtime(err)) if matches!(err.downcast::<VMError>(), Some(VMError::BadCall(_)))
    ));
}
