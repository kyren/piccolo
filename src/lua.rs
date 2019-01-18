use std::any::Any;

use failure::Error;

use gc_arena::{make_arena, ArenaParameters, Collect, GcCell};

use crate::sequence::{GenSequence, Sequence, SequenceExt};
use crate::string::InternedStringSet;
use crate::table::Table;
use crate::thread::Thread;

#[derive(Collect, Clone, Copy)]
#[collect(require_copy)]
pub struct LuaContext<'gc> {
    pub main_thread: Thread<'gc>,
    pub globals: Table<'gc>,
    pub interned_strings: InternedStringSet<'gc>,
}

pub struct Lua {
    arena: LuaArena,
}

impl Lua {
    pub fn new() -> Lua {
        let arena = LuaArena::new(ArenaParameters::default(), |mc| LuaRoot {
            context: LuaContext {
                main_thread: Thread::new(mc),
                globals: Table::new(mc),
                interned_strings: InternedStringSet::new(mc),
            },
            current_sequence: GcCell::allocate(mc, None),
        });
        Lua { arena }
    }

    pub fn sequence<G>(&mut self, g: G) -> Result<G::Item, Error>
    where
        G: GenSequence,
        G::Item: 'static,
    {
        self.arena
            .mutate(move |mc, lua_root| -> Result<(), Error> {
                *lua_root.current_sequence.write(mc) = Some(Box::new(
                    g.gen_sequence()
                        .and_then(move |_, _, r| -> Result<Box<Any>, Error> { Ok(Box::new(r)) }),
                ));
                Ok(())
            })?;

        loop {
            let r = self.arena.mutate(move |mc, lua_root| {
                let r = lua_root
                    .current_sequence
                    .write(mc)
                    .as_mut()
                    .unwrap()
                    .pump(mc, lua_root.context);
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
