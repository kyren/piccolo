extern crate luster;

use std::fmt::Debug;

fn test_script<T>(script: &[u8], expected: T)
where
    T: Eq + Debug + luster::conversion::FromLua,
{
    let result: T = luster::state::run_lua(script).expect("error in script");
    assert_eq!(expected, result);
}

#[test]
fn test_return() {
    test_script(
        r#"
            return 3
        "#.as_bytes(),
        3,
    );
}
