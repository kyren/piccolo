use std::ops;

use gc_arena::{metrics::Metrics, Arena, Collect, CollectionPhase, Mutation, Rootable};

use crate::{
    finalizers::Finalizers,
    stdlib::{load_base, load_coroutine, load_io, load_math, load_string, load_table},
    string::InternedStringSet,
    Error, FromMultiValue, Fuel, Registry, StashedExecutor, StaticError, Table,
};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct State<'gc> {
    pub globals: Table<'gc>,
    pub registry: Registry<'gc>,
    pub strings: InternedStringSet<'gc>,
    pub(crate) finalizers: Finalizers<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> State<'gc> {
        Self {
            globals: Table::new(mc),
            registry: Registry::new(mc),
            strings: InternedStringSet::new(mc),
            finalizers: Finalizers::new(mc),
        }
    }

    pub fn ctx(&'gc self, mutation: &'gc Mutation<'gc>) -> Context<'gc> {
        Context {
            mutation,
            state: self,
        }
    }
}

#[derive(Copy, Clone)]
pub struct Context<'gc> {
    pub mutation: &'gc Mutation<'gc>,
    pub state: &'gc State<'gc>,
}

impl<'gc> ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mutation
    }
}

pub struct Lua {
    arena: Arena<Rootable![State<'_>]>,
    finalized_this_cycle: bool,
}

impl Default for Lua {
    fn default() -> Self {
        Lua::core()
    }
}

impl Lua {
    /// Create a new `Lua` instance with no parts of the stdlib loaded.
    pub fn empty() -> Self {
        Lua {
            arena: Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc)),
            finalized_this_cycle: false,
        }
    }

    /// Create a new `Lua` instance with the core stdlib loaded.
    pub fn core() -> Self {
        let mut lua = Self::empty();
        lua.load_core();
        lua
    }

    /// Create a new `Lua` instance with all of the stdlib loaded.
    pub fn full() -> Self {
        let mut lua = Lua::core();
        lua.load_io();
        lua
    }

    /// Load the core parts of the stdlib that do not allow performing any I/O.
    ///
    /// Calls:
    ///   - `load_base`
    ///   - `load_coroutine`
    ///   - `load_math`
    ///   - `load_string`
    ///   - `load_table`
    pub fn load_core(&mut self) {
        self.run(|ctx| {
            load_base(ctx);
            load_coroutine(ctx);
            load_math(ctx);
            load_string(ctx);
            load_table(ctx);
        })
    }

    /// Load the parts of the stdlib that allow I/O.
    pub fn load_io(&mut self) {
        self.run(|ctx| {
            load_io(ctx);
        })
    }

    /// Size of all memory used by this Lua context.
    ///
    /// This is equivalent to `self.gc_metrics().total_allocation()`. This counts all `Gc`
    /// allocated memory and also all data Lua datastructures held inside `Gc`, as they are tracked
    /// as "external allocations" in gc-arena.
    pub fn total_memory(&self) -> usize {
        self.gc_metrics().total_allocation()
    }

    /// Finish the current collection cycle completely, calls `gc_arena::Arena::collect_all()`.
    pub fn gc_collect(&mut self) {
        if self.arena.collection_phase() == CollectionPhase::Sleeping {
            self.finalized_this_cycle = false;
        }

        self.arena.mark_all();
        if self.arena.collection_phase() == CollectionPhase::Marked {
            self.arena.mutate(|mc, root| {
                root.finalizers.finalize(mc);
            });
            self.finalized_this_cycle = true;
        }

        self.arena.collect_all();
    }

    pub fn gc_metrics(&self) -> &Metrics {
        self.arena.metrics()
    }

    pub fn run<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        const COLLECTOR_GRANULARITY: f64 = 1024.0;

        let r = self.arena.mutate(move |mc, state| f(state.ctx(mc)));
        if self.arena.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
            if self.arena.collection_phase() == CollectionPhase::Sleeping {
                self.finalized_this_cycle = false;
            }

            if self.finalized_this_cycle {
                self.arena.collect_debt();
            } else {
                self.arena.mark_debt();
                if self.arena.collection_phase() == CollectionPhase::Marked {
                    self.arena.mutate(|mc, root| {
                        root.finalizers.finalize(mc);
                    });
                    self.finalized_this_cycle = true;
                }
            }
        }
        r
    }

    pub fn try_run<F, R>(&mut self, f: F) -> Result<R, StaticError>
    where
        F: for<'gc> FnOnce(Context<'gc>) -> Result<R, Error<'gc>>,
    {
        self.run(move |ctx| f(ctx).map_err(Error::into_static))
    }

    /// Run the given executor to completion.
    pub fn finish(&mut self, executor: &StashedExecutor) {
        const FUEL_PER_GC: i32 = 4096;

        loop {
            let mut fuel = Fuel::with_fuel(FUEL_PER_GC);

            if self.run(|ctx| {
                let executor = ctx.state.registry.fetch(executor);
                executor.step(ctx, &mut fuel)
            }) {
                break;
            }
        }
    }

    /// Run the given executor to completion and then take return values from the returning thread.
    pub fn execute<R: for<'gc> FromMultiValue<'gc>>(
        &mut self,
        executor: &StashedExecutor,
    ) -> Result<R, StaticError> {
        self.finish(executor);
        self.try_run(|ctx| ctx.state.registry.fetch(executor).take_result::<R>(ctx)?)
    }
}
