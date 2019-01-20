use std::any::Any;
use std::marker::PhantomData;

use gc_arena::{make_arena, ArenaParameters, Collect, GcCell};

use crate::sequence::{Sequence, SequenceExt};
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

    #[doc(hidden)]
    pub fn sequence<F, I, E>(&mut self, f: F) -> Result<I, E>
    where
        I: 'static,
        E: 'static,
        F: for<'gc> FnOnce(PhantomData<&'gc ()>) -> Box<Sequence<'gc, Item = I, Error = E> + 'gc>,
    {
        self.arena.mutate(move |mc, lua_root| {
            *lua_root.current_sequence.write(mc) = Some(Box::new(
                f(PhantomData)
                    .map(|r| -> Box<Any> { Box::new(r) })
                    .map_err(|e| -> Box<Any> { Box::new(e) }),
            ));
        });

        loop {
            let r = self.arena.mutate(move |mc, lua_root| {
                lua_root
                    .current_sequence
                    .write(mc)
                    .as_mut()
                    .unwrap()
                    .step(mc, lua_root.context)
            });
            self.arena.collect_debt();

            if let Some(r) = r {
                self.arena.mutate(|mc, lua_root| {
                    *lua_root.current_sequence.write(mc) = None;
                });

                match r {
                    Ok(r) => {
                        return Ok(*Box::<Any + 'static>::downcast(r).unwrap());
                    }
                    Err(e) => {
                        return Err(*Box::<Any + 'static>::downcast(e).unwrap());
                    }
                }
            }
        }
    }
}

type DynSequence<'gc> =
    dyn Sequence<'gc, Item = Box<dyn Any + 'static>, Error = Box<dyn Any + 'static>> + 'gc;

#[derive(Collect)]
#[collect(empty_drop)]
struct LuaRoot<'gc> {
    context: LuaContext<'gc>,
    current_sequence: GcCell<'gc, Option<Box<DynSequence<'gc>>>>,
}

make_arena!(LuaArena, LuaRoot);

/// Runs a sequence of actions inside the given Lua context and return the result.
///
/// The first argument must be a `Lua` instance, and the second argument must be an expression of
/// type:
///
/// `S: for<'gc> Sequence<'gc> + 'gc`
///
/// This type is unweildy to express in Rust, so for ergonomics reasons this function must be a
/// macro.
#[macro_export]
macro_rules! lua_sequence {
    ($lua:expr, $seq:expr) => {
        $lua.sequence(|_| Box::new($seq))
    };
}
