use gc_arena::{Collect, Gc};

use gc_sequence::make_sequencable_arena;

#[derive(Collect)]
#[collect(empty_drop)]
struct TestRoot<'gc> {
    test: Gc<'gc, i32>,
}

make_sequencable_arena!(test_sequencer, TestRoot);

#[test]
fn test_sequencer() {}
