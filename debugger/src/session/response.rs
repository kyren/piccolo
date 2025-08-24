use std::collections::HashMap;

use crate::{Disassembled, Error, Location, StopReason};

#[derive(Debug)]
pub enum Response {
    BreakpointAdded(usize),
    BreakpointRemoved,
    FunctionBreakpointAdded(Option<usize>),
    ListBreakpoints(HashMap<String, Vec<usize>>),
    MatchesBreakpoint(Vec<usize>),
    WatchRegisterAdded,
    WatchGlobalAdded,
    ContinueRun(Result<StopReason, Error>),
    StepInto(Result<StopReason, Error>),
    StepOver(Result<StopReason, Error>),
    StepOut(Result<StopReason, Error>),
    Backtrace(Result<Vec<Location>, Error>),
    StackSnapshot(Result<Vec<(Location, Vec<String>)>, Error>),
    ReadRegister(Result<Option<String>, Error>),
    ReadUpvalues(Result<Option<Vec<String>>, Error>),
    ReadRegisters(Result<Option<Vec<String>>, Error>),
    ReadGlobal(String),
    ReadMemory(Option<Vec<u8>>),
    Disassemble(Option<Vec<Disassembled>>),
}
