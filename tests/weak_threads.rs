use piccolo::{Closure, Executor, Lua, StaticError};

#[test]
fn weak_threads_close() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(
            ctx,
            None,
            &br#"
                closure = nil

                local co = coroutine.create(function()
                    local i = 0
                    coroutine.yield(function()
                        i = i + 1
                        return i
                    end)
                end)

                _, closure = coroutine.resume(co)
            "#[..],
        )?;

        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;
    lua.execute::<()>(&executor)?;

    for i in 1..4 {
        lua.gc_collect();
        let executor = lua.try_enter(|ctx| {
            let closure = Closure::load(ctx, None, format!("assert(closure() == {i})").as_bytes())?;
            Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
        })?;
        lua.execute::<()>(&executor)?;
    }

    Ok(())
}

#[test]
fn live_upvalues_not_dead() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(
            ctx,
            None,
            &br#"
                local co = coroutine.create(function()
                    local thread = coroutine.create(function()
                        local i = 1
                        while true do
                            coroutine.yield(i)
                            i = i + 1
                        end
                    end)

                    coroutine.yield(function()
                        return coroutine.continue(thread)
                    end)
                end)

                _, go = coroutine.resume(co)
            "#[..],
        )?;

        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;
    lua.execute::<()>(&executor)?;

    for i in 1..4 {
        lua.gc_collect();
        let executor = lua.try_enter(|ctx| {
            let closure = Closure::load(ctx, None, format!("assert(go() == {i})").as_bytes())?;
            Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
        })?;
        lua.execute::<()>(&executor)?;
    }

    Ok(())
}
