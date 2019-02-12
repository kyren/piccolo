use gc_arena::{ArenaParameters, Collect, Gc};

use gc_sequence::{self as sequence, make_sequencable_arena, SequenceExt, SequenceResultExt};

#[derive(Collect)]
#[collect(empty_drop)]
struct TestRoot<'gc> {
    test: Gc<'gc, i32>,
}

make_sequencable_arena!(test_sequencer, TestRoot);
use test_sequencer::Arena as TestArena;

#[test]
fn test_sequencer() {
    let arena = TestArena::new(ArenaParameters::default(), |mc| TestRoot {
        test: Gc::allocate(mc, 42),
    });

    let mut sequence = arena.sequence(|root| {
        sequence::from_fn_with(root.test, |_, test| {
            if *test == 42 {
                Ok(*test + 10)
            } else {
                Err("will not be generated")
            }
        })
        .and_then(|_, r| Ok(r + 12))
        .and_chain(|_, r| Ok(sequence::ok(r - 10)))
        .then(|_, res| res.expect("should not be error"))
        .chain(|_, r| sequence::done(r + 10))
        .map(|r| sequence::done(r - 60))
        .flatten()
        .boxed()
    });

    loop {
        match sequence.step() {
            Ok((_, output)) => {
                assert_eq!(output, 4);
                return;
            }
            Err(s) => sequence = s,
        }
    }
}
