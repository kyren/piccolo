use std::cmp::Ordering;

use piccolo::{Lua, Table, Value};

#[test]
fn test_table_iter() {
    let mut lua = Lua::core();

    lua.enter(|ctx| {
        let table = Table::new(&ctx);

        table.set(ctx, 1, "1").unwrap();
        table.set(ctx, 2, "2").unwrap();
        table.set(ctx, 3, "3").unwrap();
        table.set(ctx, "1", 1).unwrap();
        table.set(ctx, "2", 2).unwrap();
        table.set(ctx, "3", 3).unwrap();

        let mut pairs = table.iter().collect::<Vec<_>>();
        pairs.sort_by(|&(ak, _), &(bk, _)| match (ak, bk) {
            (piccolo::Value::Integer(a), piccolo::Value::Integer(b)) => a.cmp(&b),
            (piccolo::Value::Integer(_), piccolo::Value::String(_)) => Ordering::Less,
            (piccolo::Value::String(_), piccolo::Value::Integer(_)) => Ordering::Greater,
            (piccolo::Value::String(a), piccolo::Value::String(b)) => a.cmp(&b),
            _ => unreachable!(),
        });

        assert_eq!(pairs.len(), 6);
        assert!(matches!(pairs[0], (Value::Integer(1), Value::String(s)) if s == "1" ));
        assert!(matches!(pairs[1], (Value::Integer(2), Value::String(s)) if s == "2" ));
        assert!(matches!(pairs[2], (Value::Integer(3), Value::String(s)) if s == "3" ));
        assert!(matches!(pairs[3], (Value::String(s), Value::Integer(1)) if s == "1" ));
        assert!(matches!(pairs[4], (Value::String(s), Value::Integer(2)) if s == "2" ));
        assert!(matches!(pairs[5], (Value::String(s), Value::Integer(3)) if s == "3" ));

        for (k, _) in table.iter() {
            table.set(ctx, k, Value::Nil).unwrap();
        }

        assert!(table.get_value(ctx, 1).is_nil());
        assert!(table.get_value(ctx, 2).is_nil());
        assert!(table.get_value(ctx, 3).is_nil());
        assert!(table.get_value(ctx, "1").is_nil());
        assert!(table.get_value(ctx, "2").is_nil());
        assert!(table.get_value(ctx, "3").is_nil());
    });
}
