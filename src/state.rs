use failure::{err_msg, Error};

use gc_arena::ArenaParameters;

use compiler::compile_chunk;
use conversion::FromLua;
use function::Closure;
use parser::parse_chunk;
use thread::Thread;
use value::Value;

#[derive(Collect)]
#[collect(empty_drop)]
pub struct LuaRoot<'gc> {
    main_thread: Thread<'gc>,
}
make_arena!(LuaArena, LuaRoot);

pub struct Lua {
    arena: LuaArena,
}

impl Lua {
    pub fn load(src: &[u8]) -> Result<Lua, Error> {
        let arena =
            LuaArena::try_new(ArenaParameters::default(), |mc| -> Result<LuaRoot, Error> {
                let chunk = parse_chunk(src)?;
                let main_proto = compile_chunk(mc, &chunk)?;
                let main_closure = Closure::new(mc, main_proto);
                Ok(LuaRoot {
                    main_thread: Thread::new(mc, main_closure, &[]),
                })
            })?;
        Ok(Lua { arena })
    }

    pub fn run(&mut self, instruction_limit: Option<u64>) -> bool {
        self.arena
            .mutate(move |mc, lua_root| lua_root.main_thread.run(mc, instruction_limit))
    }

    pub fn visit_results<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(Option<Vec<Value<'gc>>>) -> R,
    {
        self.arena
            .mutate(move |_, lua_root| f(lua_root.main_thread.results()))
    }
}

pub fn run_lua<T: FromLua>(src: &[u8]) -> Result<T, Error> {
    let mut lua = Lua::load(src)?;
    while !lua.run(Some(1024)) {}
    lua.visit_results(|results| {
        let results = results.unwrap();
        if let Some(first) = results.first() {
            T::from_lua(*first)
        } else {
            Err(err_msg("script did not return a result"))
        }
    })
}
