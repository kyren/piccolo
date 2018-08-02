use failure::Error;

use gc_arena::ArenaParameters;

use code::compile_chunk;
use function::FunctionProto;
use parser::parse_chunk;
use value::Value;

#[derive(Collect)]
#[collect(empty_drop)]
pub struct LuaRoot<'gc> {
    main_function: FunctionProto<'gc>,
    stack: Vec<Value<'gc>>,
    pc: usize,
    main_results: Option<Vec<Value<'gc>>>,
}
make_arena!(LuaArena, LuaRoot);

pub struct Lua {
    arena: LuaArena,
}

impl Lua {
    pub fn load(src: &[u8]) -> Result<Lua, Error> {
        let chunk = parse_chunk(src)?;
        let arena =
            LuaArena::try_new(ArenaParameters::default(), |mc| -> Result<LuaRoot, Error> {
                let main_function = compile_chunk(mc, &chunk)?;
                println!("compiled chunk: {:?}", main_function);
                Ok(LuaRoot {
                    main_function,
                    stack: Vec::new(),
                    pc: 0,
                    main_results: None,
                })
            })?;
        Ok(Lua { arena })
    }

    pub fn run(&mut self, _instruction_limit: Option<u64>) -> bool {
        unimplemented!()
    }

    pub fn visit_results<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(&Option<Vec<Value<'gc>>>) -> R,
    {
        self.arena
            .mutate(move |_, lua_root| f(&lua_root.main_results))
    }
}
