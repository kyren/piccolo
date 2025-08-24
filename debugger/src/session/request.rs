use crate::WatchMode;

#[derive(Debug)]
pub enum Request {
    AddBreakpoint {
        chunk: String,
        line: usize,
    },
    AddFunctionBreakpoint(String),
    RemoveBreakpoint {
        chunk: String,
        line: usize,
    },
    ListBreakpoints,
    MatchesBreakpoint {
        chunk: String,
        line: usize,
    },
    WatchRegister {
        register: usize,
        mode: Option<WatchMode>,
    },
    WatchGlobal {
        name: String,
        mode: Option<WatchMode>,
    },
    ContinueRun,
    StepInto,
    StepOver,
    StepOut,
    Backtrace,
    StackSnapshot,
    ReadRegister(usize),
    ReadUpvalues,
    ReadRegisters,
    ReadGlobal(String),
    ReadMemory(String, Option<usize>, Option<usize>),
    Disassemble(String, Option<usize>, Option<usize>),
}
