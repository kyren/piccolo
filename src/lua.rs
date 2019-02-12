use gc_arena::{ArenaParameters, Collect};
use gc_sequence::{make_sequencable_arena, Sequence};

use crate::{
    stdlib::{load_base, load_coroutine},
    InternedStringSet, Table, Thread,
};

#[derive(Collect, Clone, Copy)]
#[collect(require_copy)]
pub struct LuaRoot<'gc> {
    pub main_thread: Thread<'gc>,
    pub globals: Table<'gc>,
    pub interned_strings: InternedStringSet<'gc>,
}

pub struct Lua {
    arena: Option<lua_arena::Arena>,
}

impl Lua {
    pub fn new() -> Lua {
        let arena = lua_arena::Arena::new(ArenaParameters::default(), |mc| {
            let root = LuaRoot {
                main_thread: Thread::new(mc, false),
                globals: Table::new(mc),
                interned_strings: InternedStringSet::new(mc),
            };

            load_base(mc, root, root.globals);
            load_coroutine(mc, root, root.globals);

            root
        });
        Lua { arena: Some(arena) }
    }

    /// Runs a sequence of actions inside the given Lua context and return the result.
    pub fn sequence<F, O>(&mut self, f: F) -> O
    where
        O: 'static,
        F: for<'gc> FnOnce(LuaRoot<'gc>) -> Box<dyn Sequence<'gc, Output = O> + 'gc>,
    {
        let mut sequencer = self.arena.take().unwrap().sequence(move |root| f(*root));
        loop {
            match sequencer.step() {
                Ok((arena, output)) => {
                    self.arena = Some(arena);
                    return output;
                }
                Err(s) => sequencer = s,
            }
        }
    }
}

make_sequencable_arena!(lua_arena, LuaRoot);
