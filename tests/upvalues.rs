use luster::value::Value;

mod util;
use self::util::run_script;

#[test]
fn test_upvalue() {
    run_script(
        r#"
            local i = 0
            local function inc(amt)
                i = i + amt
            end
            inc(1)
            inc(2)
            inc(3)
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(6)]),
    );
}

#[test]
fn test_upvalue_multi() {
    run_script(
        r#"
            local i = 0
            local j = 0
            local k = 0
            local function inc(amt)
                i = i + amt
                j = j + 1
                k = 2 + k
            end
            inc(1)
            inc(2)
            inc(3)
            return i + j + k
        "#,
        |r| assert_eq!(r, &[Value::Integer(15)]),
    );
}

#[test]
fn test_upvalue_outer() {
    run_script(
        r#"
            local i = 0
            local inc
            local function make()
                local k = 4
                local function f()
                    i = i + k
                end
                inc = f
            end

            make()
            inc()
            inc()
            inc()
            return i
        "#,
        |r| assert_eq!(r, &[Value::Integer(12)]),
    );
}
