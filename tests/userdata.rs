use gc_arena::{lock::Lock, Collect, Gc, Rootable};
use piccolo::{
    compile, AnyCallback, AnyUserData, CallbackReturn, Closure, Lua, StaticError, Thread,
    UserDataError, Value,
};

#[derive(Collect)]
#[collect(no_drop)]
struct MyUserData<'gc>(Gc<'gc, Lock<i32>>);

#[test]
fn userdata() -> Result<(), StaticError> {
    let mut lua = Lua::new();

    lua.try_run(|ctx| {
        let userdata = AnyUserData::new::<Rootable![MyUserData<'gc>]>(
            &ctx,
            MyUserData(Gc::new(&ctx, Lock::new(17))),
        );
        ctx.state.globals.set(ctx, "userdata", userdata)?;
        let callback = AnyCallback::from_fn(&ctx, |ctx, stack| {
            match stack[0] {
                Value::UserData(ud) => {
                    let ud = ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
                    assert_eq!(ud.0.get(), 17);
                    ud.0.set(&ctx, 23);
                }
                _ => panic!(),
            };
            stack.clear();
            Ok(CallbackReturn::Return)
        });
        ctx.state.globals.set(ctx, "callback", callback)?;
        Ok(())
    })?;

    let thread = lua.try_run(|ctx| {
        let closure = Closure::new(
            &ctx,
            compile(
                ctx,
                &br#"
                    callback(userdata)
                    return userdata, type(userdata) == "userdata" and type(callback) == "function"
                "#[..],
            )?,
            Some(ctx.state.globals),
        )?;
        let thread = Thread::new(&ctx);
        thread.start(ctx, closure.into(), ())?;
        Ok(ctx.state.registry.stash(&ctx, thread))
    })?;

    lua.finish_thread(&thread);

    lua.try_run(|ctx| {
        let (ud, res) = ctx
            .state
            .registry
            .fetch(&thread)
            .take_return::<(AnyUserData, bool)>(ctx)??;
        assert!(res);
        let data = ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
        assert_eq!(data.0.get(), 23);
        #[derive(Collect)]
        #[collect(require_static)]
        struct MyUserData2;

        assert!(matches!(
            ud.read::<Rootable![MyUserData2]>(),
            Err(UserDataError::WrongType)
        ));
        Ok(())
    })
}
