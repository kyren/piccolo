use luster::value::Value;

mod util;
use self::util::run_script;

#[test]
fn test_return() {
    run_script(
        r#"
            return 3
        "#,
        |r| assert_eq!(r, &[Value::Integer(3)]),
    );
}

#[test]
fn test_local() {
    run_script(
        r#"
            local i = 7
            local j = 30
            local i = 42
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(42)]),
    );
}

#[test]
fn test_assignment() {
    run_script(
        r#"
            local i = 7
            local i = 30
            i = 35
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(35)]),
    );
}

#[test]
fn test_function() {
    run_script(
        r#"
            local function test(a, b)
                return a + b
            end
            local i = test(1, 2)
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(3)]),
    );
}

#[test]
fn test_equality() {
    run_script(
        r#"
            return 1 == 1
        "#,
        |r| assert_eq!(r, &[Value::Boolean(true)]),
    );

    run_script(
        r#"
            return 1 == 2
        "#,
        |r| assert_eq!(r, &[Value::Boolean(false)]),
    );

    run_script(
        r#"
            local i = 1
            return i == 1
        "#,
        |r| assert_eq!(r, &[Value::Boolean(true)]),
    );

    run_script(
        r#"
            local i = 2
            local j = 3
            return i == j
        "#,
        |r| assert_eq!(r, &[Value::Boolean(false)]),
    );

    run_script(
        r#"
            local i = 2
            local j = 2
            return (i == j) == true
        "#,
        |r| assert_eq!(r, &[Value::Boolean(true)]),
    );
}

#[test]
fn test_short_circuit() {
    run_script(
        r#"
            return 1 == 1 and 2 == 2
        "#,
        |r| assert_eq!(r, &[Value::Boolean(true)]),
    );

    run_script(
        r#"
            return 1 == 1 or 1 == 2
        "#,
        |r| assert_eq!(r, &[Value::Boolean(true)]),
    );

    run_script(
        r#"
            return 1 == 1 and 2 == 3
        "#,
        |r| assert_eq!(r, &[Value::Boolean(false)]),
    );

    run_script(
        r#"
            return false or false
        "#,
        |r| assert_eq!(r, &[Value::Boolean(false)]),
    );

    run_script(
        r#"
            local i = 0
            local function t(p)
                i = i + p
            end
            local _ = (false and t(1))
            local _ = (true and t(2))
            local _ = (false or t(3))
            local _ = (true or t(4))
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(5)]),
    );
}
