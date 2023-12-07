use piccolo::{FromMultiValue, FromValue, IntoMultiValue, IntoValue, Lua, Table, Value};

#[test]
fn test_conversions() {
    let mut lua = Lua::core();
    lua.enter(|ctx| {
        let v = (1, true, "hello").into_multi_value(ctx).collect::<Vec<_>>();
        assert!(matches!(
            v.as_slice(),
            [
                Value::Integer(1),
                Value::Boolean(true),
                Value::String(s)
            ] if s == b"hello"
        ));

        let vals = Table::from_value(
            ctx,
            [
                1.into_value(ctx),
                true.into_value(ctx),
                "hello".into_value(ctx),
            ]
            .into_value(ctx),
        )
        .unwrap();

        assert!(matches!(vals.get(ctx, 1), Value::Integer(1)));
        assert!(matches!(vals.get(ctx, 2), Value::Boolean(true)));
        assert!(matches!(vals.get(ctx, 3), Value::String(s) if s == b"hello"));

        let array = <[Value; 3]>::from_value(ctx, vals.into()).unwrap();
        assert!(matches!(
            array.as_slice(),
            [
                Value::Integer(1),
                Value::Boolean(true),
                Value::String(s)
            ] if s == b"hello"
        ));

        let vec = Vec::<Value>::from_value(ctx, vals.into()).unwrap();
        assert!(matches!(
            vec.as_slice(),
            [
                Value::Integer(1),
                Value::Boolean(true),
                Value::String(s)
            ] if s == b"hello"
        ));

        let (a, b, c) = <(i32, bool, String)>::from_multi_value(
            ctx,
            (2, false, "goodbye").into_multi_value(ctx),
        )
        .unwrap();
        assert_eq!((a, b, c), (2, false, "goodbye".to_owned()));
    });
}
