use gc_arena::{Arena, ArenaParameters, Collect, DynamicRootSet, MutationContext, Rootable};

use crate::{
    stdlib::{load_base, load_coroutine, load_math, load_string},
    Sequence, Table, Thread,
};

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct Root<'gc> {
    pub main_thread: Thread<'gc>,
    pub globals: Table<'gc>,
    pub registry: DynamicRootSet<'gc>,
}

impl<'gc> Root<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Root<'gc> {
        let root = Root {
            main_thread: Thread::new(mc, false),
            globals: Table::new(mc),
            registry: DynamicRootSet::new(mc),
        };

        load_base(mc, root, root.globals);
        load_coroutine(mc, root, root.globals);
        load_math(mc, root, root.globals);
        load_string(mc, root, root.globals);

        root
    }
}

pub struct Lua(Option<Arena<Rootable![Root<'gc>]>>);

const COLLECTOR_GRANULARITY: f64 = 1024.0;

impl Lua {
    pub fn new() -> Lua {
        Lua(Some(Arena::new(ArenaParameters::default(), |mc| {
            Root::new(mc)
        })))
    }

    /// Runs a single action inside the Lua arena, during which no garbage collection may take
    /// place.
    pub fn mutate<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(MutationContext<'gc, '_>, Root<'gc>) -> R,
    {
        let arena = self.0.as_mut().unwrap();
        let r = arena.mutate(move |mc, root| f(mc, *root));
        if arena.allocation_debt() > COLLECTOR_GRANULARITY {
            arena.collect_debt();
        }
        r
    }

    /// Runs a sequence of actions inside the Lua arena and return the result. Garbage collection
    /// may take place in-between sequence steps.
    pub fn sequence<F, R>(&mut self, f: F) -> R
    where
        R: 'static,
        F: for<'gc> FnOnce(Root<'gc>) -> Box<dyn Sequence<'gc, Output = R> + 'gc>,
    {
        #[derive(Collect)]
        #[collect(no_drop, bound = "")]
        struct RootSequence<'gc, R> {
            root: Root<'gc>,
            sequence: Box<dyn Sequence<'gc, Output = R> + 'gc>,
        }

        let mut sequence = self
            .0
            .take()
            .unwrap()
            .map_root::<Rootable![RootSequence<'gc, R>]>(|_, root| RootSequence {
                root,
                sequence: f(root),
            });

        let res = loop {
            if let Some(res) = sequence.mutate_root(|mc, arena| arena.sequence.step(mc)) {
                break res;
            }
        };

        self.0 = Some(sequence.map_root(|_, arena| arena.root));

        res
    }
}
