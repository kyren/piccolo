use deimos::{
    compile, Callback, CallbackReturn, Closure, Function, Lua, StaticError, StaticValue, UserData,
    UserDataError, Value,
};
use gc_arena::{Collect, GcCell, Rootable};

#[derive(Collect)]
#[collect(no_drop)]
struct MyUserData<'gc>(GcCell<'gc, i32>);

#[test]
fn userdata() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();

    lua.try_run(|mc, root| {
        let userdata =
            UserData::new::<Rootable![MyUserData<'gc>]>(mc, MyUserData(GcCell::allocate(mc, 17)));
        root.globals.set(mc, "userdata", userdata)?;
        let callback = Callback::new_immediate(mc, |mc, args| {
            match args[0] {
                Value::UserData(ud) => {
                    let ud = ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
                    assert_eq!(*ud.0.read(), 17);
                    *ud.0.write(mc) = 23;
                }
                _ => panic!(),
            };
            Ok(CallbackReturn::Return(vec![]))
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
            assert_eq!(*data.0.read(), 23);

            #[derive(Collect)]
            #[collect(no_drop)]
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
