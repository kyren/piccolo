use gc_arena::{Arena, ArenaParameters, Collect, Mutation, Rootable};

use crate::{
    stdlib::{load_base, load_coroutine, load_math, load_string},
    string::InternedStringSet,
    Error, FromMultiValue, Registry, StaticError, StaticThread, Table, Thread, ThreadMode,
};

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct State<'gc> {
    pub main_thread: Thread<'gc>,
    pub globals: Table<'gc>,
    pub registry: Registry<'gc>,
    pub strings: InternedStringSet<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> State<'gc> {
        let state = State {
            main_thread: Thread::new(mc),
            globals: Table::new(mc),
            registry: Registry::new(mc),
            strings: InternedStringSet::new(mc),
        };

        load_base(mc, state);
        load_coroutine(mc, state);
        load_math(mc, state);
        load_string(mc, state);

        state
    }
}

pub struct Lua(Arena<Rootable![State<'gc>]>);

const COLLECTOR_GRANULARITY: f64 = 1024.0;

impl Lua {
    pub fn new() -> Lua {
        Lua(Arena::new(ArenaParameters::default(), |mc| State::new(mc)))
    }

    /// Runs a single action inside the Lua arena, during which no garbage collection may take
    /// place.
    pub fn run<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(&Mutation<'gc>, State<'gc>) -> R,
    {
        let r = self.0.mutate(move |mc, state| f(mc, *state));
        if self.0.allocation_debt() > COLLECTOR_GRANULARITY {
            self.0.collect_debt();
        }
        r
    }

    pub fn try_run<F, T>(&mut self, f: F) -> Result<T, StaticError>
    where
        F: for<'gc> FnOnce(&Mutation<'gc>, State<'gc>) -> Result<T, Error<'gc>>,
    {
        self.run(move |mc, state| f(mc, state).map_err(Error::to_static))
    }

    pub fn finish_main_thread(&mut self) {
        loop {
            if self.run(|mc, state| match state.main_thread.mode() {
                ThreadMode::Normal => {
                    state.main_thread.step(mc).unwrap();
                    false
                }
                _ => true,
            }) {
                break;
            }
        }
    }

    pub fn finish_thread(&mut self, thread: &StaticThread) {
        loop {
            if self.run(|mc, state| {
                let thread = state.registry.fetch(thread);
                match thread.mode() {
                    ThreadMode::Normal => {
                        thread.step(mc).unwrap();
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
        self.try_run(|mc, state| state.registry.fetch(thread).take_return::<R>(mc)?)
    }

    pub fn run_main_thread<R: for<'gc> FromMultiValue<'gc>>(&mut self) -> Result<R, StaticError> {
        self.finish_main_thread();
        self.try_run(|mc, state| state.main_thread.take_return::<R>(mc)?)
    }
}
