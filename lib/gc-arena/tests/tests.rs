#[macro_use]
extern crate gc_arena;

use gc_arena::{ArenaParameters, Collect, CollectionContext, Gc};

#[test]
fn new_arena() {
    struct TestRoot<'gc> {
        test: Gc<'gc, i32>,
    }

    unsafe impl<'gc> Collect for TestRoot<'gc> {
        #[inline]
        fn trace(&self, cc: CollectionContext) {
            self.test.trace(cc);
        }
    }

    make_arena!(TestArena, TestRoot);

    let arena = TestArena::new(ArenaParameters::default(), |mc| TestRoot {
        test: Gc::allocate(mc, 42),
    });

    arena.mutate(|_mc, root| {
        assert_eq!(*((*root).test), 42);
    });
}
