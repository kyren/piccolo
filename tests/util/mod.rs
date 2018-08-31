extern crate luster;

use std::fmt::Debug;

pub fn test_script<A: AsRef<[u8]>, T>(script: A, expected: T)
where
    T: Eq + Debug + luster::conversion::FromLua,
{
    let result: T = luster::state::run_lua(script.as_ref()).expect("error in script");
    assert_eq!(expected, result);
}
