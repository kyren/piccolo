use std::ops;

use gc_arena::{metrics::Metrics, Arena, Collect, Mutation, Rootable};

use crate::{
    error::RuntimeError,
    stdlib::{load_base, load_coroutine, load_io, load_math, load_string},
    string::InternedStringSet,
    Error, FromMultiValue, Registry, StaticError, StaticThread, Table, ThreadMode,
};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct State<'gc> {
    pub globals: Table<'gc>,
    pub registry: Registry<'gc>,
    pub strings: InternedStringSet<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> State<'gc> {
        Self {
            globals: Table::new(mc),
            registry: Registry::new(mc),
            strings: InternedStringSet::new(mc),
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

pub struct Lua(Arena<Rootable![State<'_>]>);

const COLLECTOR_GRANULARITY: f64 = 1024.0;

impl Default for Lua {
    fn default() -> Self {
        Lua::core()
    }
}

impl Lua {
    /// Create a new `Lua` instance with no parts of the stdlib loaded.
    pub fn empty() -> Self {
        Lua(Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc)))
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
    /// Calls: `load_base`, `load_coroutine`, `load_math`, and `load_string`.
    pub fn load_core(&mut self) {
        self.run(|ctx| {
            load_base(ctx);
            load_coroutine(ctx);
            load_math(ctx);
            load_string(ctx);
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
    /// as "external allocations" in via gc-arena.
    pub fn total_memory(&self) -> usize {
        self.gc_metrics().total_allocation()
    }

    /// Finish the current collection cycle completely, calls `gc_arena::Arena::collect_all()`.
    pub fn gc_collect(&mut self) {
        self.0.collect_all();
    }

    pub fn gc_metrics(&self) -> &Metrics {
        self.0.metrics()
    }

    pub fn run<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        let r = self.0.mutate(move |mc, state| f(state.ctx(mc)));
        if self.0.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
            self.0.collect_debt();
        }
        r
    }

    pub fn try_run<F, R>(&mut self, f: F) -> Result<R, StaticError>
    where
        F: for<'gc> FnOnce(Context<'gc>) -> Result<R, Error<'gc>>,
    {
        self.run(move |ctx| f(ctx).map_err(Error::into_static))
    }

    pub fn finish_thread(&mut self, thread: &StaticThread) {
        loop {
            if self.run(|ctx| {
                let thread = ctx.state.registry.fetch(thread);
                match thread.mode() {
                    ThreadMode::Normal => {
                        thread.step(ctx).unwrap();
                        false
                    }
                    _ => true,
                }
            }) {
                break;
            }
        }
    }

    pub fn run_thread<R: for<'gc> FromMultiValue<'gc>>(
        &mut self,
        thread: &StaticThread,
    ) -> Result<R, StaticError> {
        self.finish_thread(thread);
        self.run(|ctx| {
            ctx.state
                .registry
                .fetch(thread)
                .take_return::<R>(ctx)
                .map_err(RuntimeError::from)?
                .map_err(Error::into_static)
        })
    }
}
