mod util;
use self::util::test_script;

#[test]
fn test_return() {
    test_script(
        r#"
            return 3
        "#,
        3,
    );
}

#[test]
fn test_local() {
    test_script(
        r#"
            local i = 7
            local j = 30
            local i = 42
            return i
        "#,
        42,
    );
}

#[test]
fn test_assignment() {
    test_script(
        r#"
            local i = 7
            local i = 30
            i = 35
            return i
        "#,
        35,
    );
}
