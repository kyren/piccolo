use std::{mem, thread::Thread};

use piccolo::{opcode::OpCode, AnyCallback, AnyUserData, Closure, String, Table, Value};

#[test]
fn test_sizes() {
    assert!(mem::size_of::<OpCode>() <= 4);

    let ptr_size = mem::size_of::<*const ()>();
    assert_eq!(mem::size_of::<String>(), ptr_size);
    assert_eq!(mem::size_of::<Table>(), ptr_size);
    assert_eq!(mem::size_of::<Closure>(), ptr_size);
    assert_eq!(mem::size_of::<AnyCallback>(), ptr_size);
    assert_eq!(mem::size_of::<Thread>(), ptr_size);
    assert_eq!(mem::size_of::<AnyUserData>(), ptr_size);
    assert!(mem::size_of::<Value>() <= ptr_size * 2);
}
