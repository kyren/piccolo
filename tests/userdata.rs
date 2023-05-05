use gc_arena::{Collect, GcCell, Rootable};
use piccolo::{
    compile, sequence, Callback, CallbackReturn, Closure, Error, Function, Lua, SequenceExt,
    StaticError, String, ThreadSequence, TrySequenceExt, UserData, UserDataError, Value,
};

#[derive(Collect)]
#[collect(no_drop)]
struct MyUserData<'gc>(GcCell<'gc, i32>);

#[test]
fn userdata() -> Result<(), Box<StaticError>> {
    let mut lua = Lua::new();
    lua.sequence(|root| {
        sequence::from_fn_with(root, |root, mc| {
            let userdata = UserData::new::<Rootable![MyUserData<'gc>]>(
                mc,
                MyUserData(GcCell::allocate(mc, 17)),
            );
            root.globals
                .set(mc, String::from_static(b"userdata"), userdata)?;
            let callback = Callback::new_immediate(mc, |mc, _, args| {
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
            root.globals
                .set(mc, String::from_static(b"callback"), callback)?;
            Ok(())
        })
        .and_then_with(root, |root, mc, _| {
            Ok(Closure::new(
                mc,
                compile(
                    mc,
                    &br#"
                        callback(userdata)
                        return userdata, type(userdata) == "userdata" and type(callback) == "function"
                    "#[..],
                )?,
                Some(root.globals),
            )?)
        })
        .and_chain_with(root, |root, mc, closure| {
            Ok(ThreadSequence::call_function(
                mc,
                root.main_thread,
                Function::Closure(closure),
                &[],
            )?)
        })
        .map_ok(|b| {
            match b[0] {
                Value::UserData(ud) => {
                    let data= ud.read::<Rootable![MyUserData<'gc>]>().unwrap();
                    assert_eq!(*data.0.read(), 23);

                    #[derive(Collect)]
                    #[collect(no_drop)]
                    struct MyUserData2;

                    assert!(matches!(
                        ud.read::<Rootable![MyUserData2]>(),
                        Err(UserDataError::WrongType)
                    ));
                },
                _ => panic!(),
            }
            assert_eq!(b[1], Value::Boolean(true));
        })
        .map_err(Error::to_static)
        .boxed()
    })?;

    Ok(())
}
