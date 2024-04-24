//! Tools for introspection and debugging

use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, Collect};

use crate::{closure::UpValue, thread::thread::Frame, Closure, Context, Function, Value};

use super::{
    executor::ExecutorState,
    thread::{LuaReturn, MetaReturn},
};

// harhar very funny
fn generate_thread_name(mut position: usize) -> String {
    const RADIO_ALPHABET: [&str; 26] = [
        "alfa", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india",
        "juliett", "kilo", "lima", "mike", "november", "oscar", "papa", "quebec", "romeo",
        "sierra", "tango", "uniform", "victor", "whiskey", "xray", "yankee", "zulu",
    ];
    let mut name = String::new();
    while position >= RADIO_ALPHABET.len() {
        name = format!(
            "{name}-{}-",
            RADIO_ALPHABET[position % RADIO_ALPHABET.len()]
        );
        position -= RADIO_ALPHABET.len();
    }

    // position *HAS* to be within indexable range by this point
    if name.is_empty() {
        RADIO_ALPHABET[position].to_string()
    } else {
        format!("{name}-{}", RADIO_ALPHABET[position])
    }
}

/// Inert representation of the current executor state
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct DebugState<'gc> {
    pub threads: vec::Vec<DumpedThread<'gc>, MetricsAlloc<'gc>>,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct DumpedThread<'gc> {
    pub name: crate::String<'gc>,
    pub top_frame: DumpedLuaFrame<'gc>,
    pub stack: vec::Vec<Value<'gc>, MetricsAlloc<'gc>>,
    pub open_upvalues: vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub enum DumpedLuaReturn {
    // Normal function call, place return values at the bottom of the returning function's stack,
    // as normal.
    Normal(Option<u8>),
    // Synthetic metamethod call, do the operation specified in MetaReturn.
    Meta(DumpedMetaReturn),
}

impl From<LuaReturn> for DumpedLuaReturn {
    fn from(value: LuaReturn) -> Self {
        match value {
            LuaReturn::Normal(opt) => Self::Normal(opt.to_constant()),
            LuaReturn::Meta(mr) => Self::Meta(mr.into()),
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub enum DumpedMetaReturn {
    // No return value is expected.
    None,
    // Place a single return value at an index relative to the returned to function's stack bottom.
    Register(u8),
    // Increment the PC by one if the returned value converted to a boolean is equal to this.
    SkipIf(bool),
}

impl From<MetaReturn> for DumpedMetaReturn {
    fn from(value: MetaReturn) -> Self {
        match value {
            MetaReturn::None => Self::None,
            MetaReturn::Register(_) => todo!(),
            MetaReturn::SkipIf(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct DumpedLuaFrame<'gc> {
    pub pc: usize,
    pub base: usize,
    pub is_variable: bool,
    pub stack_size: usize,
    pub expected_return: Option<DumpedLuaReturn>,
    pub closure: Closure<'gc>,
}

impl<'gc> ExecutorState<'gc> {
    pub fn inspect(&self, ctx: Context<'gc>) -> DebugState<'gc> {
        let mut threads = vec::Vec::new_in(MetricsAlloc::new(&ctx));
        for (idx, thread) in self.thread_stack.iter().enumerate() {
            let state = thread.into_inner().borrow();
            if let Some(Frame::Lua {
                bottom,
                base,
                pc,
                stack_size,
                expected_return,
                is_variable,
                ..
            }) = state.frames.last()
            {
                if let Value::Function(Function::Closure(c)) = state.stack[*bottom] {
                    threads.push(DumpedThread {
                        name: ctx.intern(generate_thread_name(idx).as_bytes()),
                        top_frame: DumpedLuaFrame {
                            pc: *pc,
                            base: *base,
                            closure: c,
                            expected_return: expected_return.map(|er| er.into()),
                            is_variable: *is_variable,
                            stack_size: *stack_size,
                        },
                        stack: state.stack.clone(),
                        open_upvalues: state.open_upvalues.clone(),
                    });
                }
            }
        }
        DebugState { threads }
    }
}
