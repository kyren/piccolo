use luster::value::Value;

mod util;
use self::util::run_script;

#[test]
fn test_mult_return() {
    run_script(
        r#"
            local function test()
                return 1, 2, 3
            end
            local i, j, k = test()
            return i + j + k
        "#,
        |r| assert_eq!(r, &[Value::Integer(6)]),
    );
}

#[test]
fn test_mult_arg() {
    run_script(
        r#"
            local function test1()
                return 2, 3
            end
            local function test2(a, b, c)
                return a + b + c
            end
            return test2(1, test1())
        "#,
        |r| assert_eq!(r, &[Value::Integer(6)]),
    );
}
