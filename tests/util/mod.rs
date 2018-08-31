extern crate luster;

use std::fmt::Debug;

pub fn test_script<T>(script: &[u8], expected: T)
where
    T: Eq + Debug + luster::conversion::FromLua,
{
    let result: T = luster::state::run_lua(script).expect("error in script");
    assert_eq!(expected, result);
}
