use std::any::Any;

use failure::Error;

use gc_arena::{make_arena, ArenaParameters, Collect, GcCell, MutationContext};

use crate::sequence::{Sequence, SequenceExt};
use crate::thread::Thread;

#[derive(Collect)]
#[collect(empty_drop)]
pub struct LuaContext<'gc> {
    pub main_thread: Thread<'gc>,
}

pub struct Lua {
    arena: LuaArena,
}

impl Lua {
    pub fn new() -> Lua {
        let arena = LuaArena::new(ArenaParameters::default(), |mc| LuaRoot {
            context: LuaContext {
                main_thread: Thread::new(mc),
            },
            current_sequence: GcCell::allocate(mc, None),
        });
        Lua { arena }
    }

    pub fn sequence<F, R>(&mut self, f: F) -> Result<R, Error>
    where
        F: for<'gc> FnOnce(
            MutationContext<'gc, '_>,
            &LuaContext<'gc>,
        ) -> Box<dyn Sequence<'gc, Item = R> + 'gc>,
        R: 'static,
    {
        self.arena.mutate(move |mc, lua_root| {
            *lua_root.current_sequence.write(mc) = Some(Box::new(
                f(mc, &lua_root.context)
                    .map(move |_, r| -> Result<Box<Any>, Error> { Ok(Box::new(r)) }),
            ));
        });
        self.arena.collect_debt();

        loop {
            let r = self.arena.mutate(move |mc, lua_root| {
                let r = lua_root
                    .current_sequence
                    .write(mc)
                    .as_mut()
                    .unwrap()
                    .pump(mc);
                if r.is_some() {
                    *lua_root.current_sequence.write(mc) = None;
                }
                r
            });
            self.arena.collect_debt();

            if let Some(r) = r {
                match r {
                    Ok(r) => {
                        return Ok(*Box::<Any + 'static>::downcast(r).unwrap());
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
        }
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
struct LuaRoot<'gc> {
    context: LuaContext<'gc>,
    current_sequence:
        GcCell<'gc, Option<Box<dyn Sequence<'gc, Item = Box<dyn Any + 'static>> + 'gc>>>,
}

make_arena!(LuaArena, LuaRoot);
