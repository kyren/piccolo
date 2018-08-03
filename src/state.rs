use failure::Error;

use gc_arena::{ArenaParameters, GcCell};

use code::compile_chunk;
use function::FunctionProto;
use opcode::OpCode;
use parser::parse_chunk;
use value::Value;

#[derive(Collect)]
#[collect(empty_drop)]
pub struct State<'gc> {
    stack: Vec<Value<'gc>>,
    pc: usize,
    main_results: Option<Vec<Value<'gc>>>,
}

#[derive(Collect)]
#[collect(empty_drop)]
pub struct LuaRoot<'gc> {
    main_function: FunctionProto<'gc>,
    state: GcCell<'gc, State<'gc>>,
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
                Ok(LuaRoot {
                    main_function,
                    state: GcCell::allocate(
                        mc,
                        State {
                            stack: vec![Value::Nil; 256],
                            pc: 0,
                            main_results: None,
                        },
                    ),
                })
            })?;
        Ok(Lua { arena })
    }

    pub fn run(&mut self, mut instruction_limit: Option<u64>) -> bool {
        self.arena.mutate(move |mc, lua_root| {
            let mut state = lua_root.state.write(mc);
            let state: &mut State = &mut *state;

            if state.main_results.is_some() {
                return true;
            }

            let frame = &mut state.stack[0..256];
            loop {
                match lua_root.main_function.opcodes[state.pc] {
                    OpCode::Move { dest, source } => {
                        frame[dest as usize] = frame[source as usize];
                    }
                    OpCode::LoadConstant { dest, constant } => {
                        frame[dest as usize] = lua_root.main_function.constants[constant as usize];
                    }
                    OpCode::LoadBool {
                        dest,
                        value,
                        skip_next,
                    } => {
                        frame[dest as usize] = Value::Boolean(value);
                        if skip_next {
                            state.pc += 1;
                        }
                    }
                    OpCode::LoadNil { dest, count } => {
                        for i in dest..dest + count {
                            frame[i as usize] = Value::Nil;
                        }
                    }
                    OpCode::Return { start, count } => {
                        if let Some(count) = count.get_count() {
                            state.main_results =
                                Some(frame[start as usize..(start + count) as usize].to_vec());
                        } else {
                            unimplemented!("no variable return");
                        }
                        break;
                    }
                }
                state.pc += 1;

                if let Some(instruction_limit) = instruction_limit.as_mut() {
                    if *instruction_limit == 0 {
                        break;
                    } else {
                        *instruction_limit -= 1
                    }
                }
            }

            state.main_results.is_some()
        })
    }

    pub fn visit_results<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(Option<&Vec<Value<'gc>>>) -> R,
    {
        self.arena
            .mutate(move |_, lua_root| f(lua_root.state.read().main_results.as_ref()))
    }
}
