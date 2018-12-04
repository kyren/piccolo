mod util;
use self::util::test_script;

#[test]
fn test_mult_return() {
    test_script(
        r#"
            local function test()
                return 1, 2, 3
            end
            local i, j, k = test()
            return i + j + k
        "#,
        6,
    );
}

#[test]
fn test_mult_arg() {
    test_script(
        r#"
            local function test1()
                return 2, 3
            end
            local function test2(a, b, c)
                return a + b + c
            end
            return test2(1, test1())
        "#,
        6,
    );
}
