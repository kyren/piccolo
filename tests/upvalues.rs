mod util;
use self::util::test_script;

#[test]
fn test_upvalue() {
    test_script(
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
        6,
    );
}

#[test]
fn test_upvalue_multi() {
    test_script(
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
        15,
    );
}

#[test]
fn test_upvalue_outer() {
    test_script(
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
        12,
    );
}
