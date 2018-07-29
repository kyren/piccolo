use function::FunctionProto;
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
    pub fn load(_lua_src: &[u8]) -> Lua {
        unimplemented!()
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
