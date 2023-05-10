use gc_arena::{Collect, Gc, Lock, Rootable};
use piccolo::{
    compile, AnyCallback, CallbackReturn, Closure, Function, Lua, StaticError, StaticValue,
    UserData, UserDataError, Value,
};

#[derive(Collect)]
#[collect(no_drop)]
struct MyUserData<'gc>(Gc<'gc, Lock<i32>>);

#[test]
fn userdata() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let userdata =
            UserData::new::<Rootable![MyUserData<'gc>]>(mc, MyUserData(Gc::new(mc, Lock::new(17))));
        root.globals.set(mc, "userdata", userdata)?;
        let callback = AnyCallback::from_fn(mc, |mc, stack| {
            match stack[0] {
                Value::UserData(ud) => {
                    let ud = ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
                    assert_eq!(ud.0.get(), 17);
                    ud.0.set(mc, 23);
                }
                _ => panic!(),
            };
            stack.clear();
            Ok(CallbackReturn::Return.into())
        });
        root.globals.set(mc, "callback", callback)?;
        Ok(())
    })?;

    let function = lua.try_run(|mc, root| {
        let closure = Closure::new(
            mc,
            compile(
                mc,
                    &br#"
                        callback(userdata)
                        return userdata, type(userdata) == "userdata" and type(callback) == "function"
                    "#[..],
            )?,
            Some(root.globals),
        )?;
        Ok(root.registry.stash(mc, Function::Closure(closure)))
    })?;

    match &lua.run_function(&function, &[])?[..] {
        [StaticValue::UserData(ud), StaticValue::Boolean(true)] => lua.run(|_, root| {
            let ud = root.registry.fetch(ud);
            let data = ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
            assert_eq!(data.0.get(), 23);

            #[derive(Collect)]
            #[collect(require_static)]
            struct MyUserData2;

            assert!(matches!(
                ud.read::<Rootable![MyUserData2]>(),
                Err(UserDataError::WrongType)
            ));
        }),
        _ => panic!(),
    }

    Ok(())
}
