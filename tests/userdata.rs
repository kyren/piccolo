use gc_arena::{lock::Lock, Collect, Gc, Rootable};
use piccolo::{Callback, CallbackReturn, Closure, Executor, Lua, StaticError, UserData, Value};

#[derive(Collect)]
#[collect(no_drop)]
struct MyUserData<'gc>(Gc<'gc, Lock<i32>>);

#[test]
fn userdata() -> Result<(), StaticError> {
    let mut lua = Lua::core();

    lua.try_enter(|ctx| {
        let userdata = UserData::new::<Rootable![MyUserData<'_>]>(
            &ctx,
            MyUserData(Gc::new(&ctx, Lock::new(17))),
        );
        ctx.set_global("userdata", userdata)?;
        let callback = Callback::from_fn(&ctx, |ctx, _, mut stack| {
            match stack[0] {
                Value::UserData(ud) => {
                    let ud = ud.downcast::<Rootable![MyUserData<'_>]>().unwrap();
                    assert_eq!(ud.0.get(), 17);
                    ud.0.set(&ctx, 23);
                }
                _ => panic!(),
            };
            stack.clear();
            Ok(CallbackReturn::Return)
        });
        ctx.set_global("callback", callback)?;
        Ok(())
    })?;

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(
            ctx,
            None,
            &br#"
                callback(userdata)
                return userdata, type(userdata) == "userdata" and type(callback) == "function"
            "#[..],
        )?;
        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.finish(&executor);

    lua.try_enter(|ctx| {
        let (ud, res) = ctx
            .fetch(&executor)
            .take_result::<(UserData, bool)>(ctx)??;
        assert!(res);
        let data = ud.downcast::<Rootable![MyUserData<'_>]>().unwrap();
        assert_eq!(data.0.get(), 23);
        #[derive(Collect)]
        #[collect(require_static)]
        struct MyUserData2;

        assert!(ud.downcast::<Rootable![MyUserData2]>().is_err());
        Ok(())
    })
}
