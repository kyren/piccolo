use gc_arena::{ArenaParameters, Collect, MutationContext};
use gc_sequence::{make_sequencable_arena, Sequence};

use crate::{
    stdlib::{load_base, load_coroutine, load_math, load_string},
    InternedStringSet, Table, Thread,
};

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct Root<'gc> {
    pub main_thread: Thread<'gc>,
    pub globals: Table<'gc>,
    pub interned_strings: InternedStringSet<'gc>,
}

impl<'gc> Root<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Root<'gc> {
        let root = Root {
            main_thread: Thread::new(mc, false),
            globals: Table::new(mc),
            interned_strings: InternedStringSet::new(mc),
        };

        load_base(mc, root, root.globals);
        load_coroutine(mc, root, root.globals);
        load_math(mc, root, root.globals);
        load_string(mc, root, root.globals);

        root
    }
}

make_sequencable_arena!(pub lua_arena, Root);

pub use lua_arena::Arena;
pub use lua_arena::Sequencer;

/// Simpler wrapper for `Arena` that automatically garbage collects at reasonable intervals.
pub struct Lua(Option<lua_arena::Arena>);

const COLLECTOR_GRANULARITY: f64 = 1024.0;

impl Lua {
    pub fn new() -> Lua {
        Lua(Some(Arena::new(ArenaParameters::default(), |mc| {
            Root::new(mc)
        })))
    }

    /// Runs a single action inside the Lua arena, during which no garbage collection may take place.
    pub fn mutate<F, R>(&mut self, f: F) -> R
    where
        R: 'static,
        F: for<'gc> FnOnce(MutationContext<'gc, '_>, Root<'gc>) -> R,
    {
        let arena = self.0.as_mut().unwrap();
        let r = arena.mutate(move |mc, root| f(mc, *root));
        if arena.allocation_debt() > COLLECTOR_GRANULARITY {
            arena.collect_debt();
        }
        r
    }

    /// Runs a sequence of actions inside the Lua arena and return the result.  Garbage collection
    /// may take place in-between sequence steps.
    pub fn sequence<F, R>(&mut self, f: F) -> R
    where
        R: 'static,
        F: for<'gc> FnOnce(Root<'gc>) -> Box<dyn Sequence<'gc, Output = R> + 'gc>,
    {
        let mut sequencer = self.0.take().unwrap().sequence(move |root| f(*root));
        loop {
            match sequencer.step() {
                Ok((arena, output)) => {
                    self.0 = Some(arena);
                    return output;
                }
                Err(s) => {
                    sequencer = s;
                    if sequencer.allocation_debt() > COLLECTOR_GRANULARITY {
                        sequencer.collect_debt();
                    }
                }
            }
        }
    }
}
