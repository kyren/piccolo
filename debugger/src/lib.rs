#[allow(dead_code)]
#[derive(Debug, Clone)]
struct PrototypeReference {
    chunk_name: String,
    prototype_index: usize,
}

use piccolo::opcode::Operation;
use std::collections::HashMap;

pub use self::{
    adapter::*, error::Error, location::Location, stop_reason::StopReason, watch::WatchMode,
};

use self::session::Session;

pub mod adapter;
mod breakpoints;
pub mod error;
pub mod location;
mod session;
pub mod stop_reason;
mod watch;

#[derive(Debug)]
pub struct Disassembled {
    pub symbol: &'static str,
    pub index: usize,
    pub line: usize,
    pub operation: Operation,
}

pub struct Debugger {
    next_session: usize,
    sessions: Vec<Session>,
}

impl Debugger {
    pub fn new() -> Self {
        Self {
            next_session: 0,
            sessions: Vec::new(),
        }
    }

    pub fn launch(&mut self, session_id: usize) {
        self.sessions
            .get_mut(session_id)
            .map(|session| session.launch());
    }

    pub fn restart(&mut self, session_id: usize) {
        self.sessions
            .get_mut(session_id)
            .map(|session| session.restart());
    }

    pub fn disconnect(&mut self, session_id: usize) {
        self.sessions
            .get_mut(session_id)
            .map(|session| session.disconnect());
    }

    pub fn terminate(&mut self, session_id: usize) {
        self.sessions
            .get_mut(session_id)
            .map(|session| session.terminate());
    }

    pub fn map_by_name(&self, name: &str) -> Option<usize> {
        self.sessions.iter().position(|s| s.name() == name)
    }

    pub fn add_session(&mut self, source: impl Into<String>, path: impl Into<String>) -> usize {
        let id = self.next_session;
        self.next_session += 1;
        let source = source.into();
        let path = path.into();
        let session = Session::new(id, source, path);
        self.sessions.push(session);
        id
    }

    pub fn add_breakpoint(
        &mut self,
        session_id: usize,
        chunk: String,
        line: usize,
    ) -> Result<usize, Error> {
        self.get_mut(session_id, |session| session.add_breakpoint(chunk, line))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn add_function_breakpoint(
        &mut self,
        session_id: usize,
        name: String,
    ) -> Result<Option<usize>, Error> {
        self.get_mut(session_id, |session| session.add_function_breakpoint(name))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn remove_breakpoint(
        &mut self,
        session_id: usize,
        chunk: String,
        line: usize,
    ) -> Result<(), Error> {
        self.get_mut(session_id, |session| session.remove_breakpoint(chunk, line))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn list_breakpoints(
        &self,
        session_id: usize,
    ) -> Result<HashMap<String, Vec<usize>>, Error> {
        self.get(session_id, |session| session.list_breakpoints())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn matches_breakpoint(
        &self,
        session_id: usize,
        chunk: String,
        line: usize,
    ) -> Result<Vec<usize>, Error> {
        self.get(session_id, |session| {
            session.matches_breakpoint(chunk, line)
        })
        .ok_or(Error::SessionNotFound)?
    }

    pub fn watch_register(
        &mut self,
        session_id: usize,
        register: usize,
        mode: Option<WatchMode>,
    ) -> Result<(), Error> {
        self.get_mut(session_id, |session| session.watch_register(register, mode))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn watch_global(
        &mut self,
        session_id: usize,
        name: impl Into<String>,
        mode: Option<WatchMode>,
    ) -> Result<(), Error> {
        self.get_mut(session_id, |session| session.watch_global(name, mode))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn continue_run(&mut self, session_id: usize) -> Result<StopReason, Error> {
        self.get_mut(session_id, |session| session.continue_run())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn step_into(&mut self, session_id: usize) -> Result<StopReason, Error> {
        self.get_mut(session_id, |session| session.step_into())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn step_over(&mut self, session_id: usize) -> Result<StopReason, Error> {
        self.get_mut(session_id, |session| session.step_over())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn step_out(&mut self, session_id: usize) -> Result<StopReason, Error> {
        self.get_mut(session_id, |session| session.step_out())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn backtrace(&mut self, session_id: usize) -> Result<Vec<Location>, Error> {
        self.get_mut(session_id, |session| session.backtrace())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn stack_snapshot(
        &mut self,
        session_id: usize,
    ) -> Result<Vec<(Location, Vec<String>)>, Error> {
        self.get_mut(session_id, |session| session.stack_snapshot())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn read_register(
        &mut self,
        session_id: usize,
        register: usize,
    ) -> Result<Option<String>, Error> {
        self.get_mut(session_id, |session| session.read_register(register))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn read_upvalues(&mut self, session_id: usize) -> Result<Option<Vec<String>>, Error> {
        self.get_mut(session_id, |session| session.read_upvalues())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn read_registers(&mut self, session_id: usize) -> Result<Option<Vec<String>>, Error> {
        self.get_mut(session_id, |session| session.read_registers())
            .ok_or(Error::SessionNotFound)?
    }

    pub fn read_global(&mut self, session_id: usize, name: String) -> Result<String, Error> {
        self.get_mut(session_id, |session| session.read_global(name))
            .ok_or(Error::SessionNotFound)?
    }

    pub fn read_memory(
        &mut self,
        session_id: usize,
        memory_reference: String,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Result<Option<Vec<u8>>, Error> {
        self.get_mut(session_id, |session| {
            session.read_memory(memory_reference, byte_offset, byte_count)
        })
        .ok_or(Error::SessionNotFound)?
    }

    pub fn disassemble(
        &mut self,
        session_id: usize,
        memory_reference: String,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Result<Option<Vec<Disassembled>>, Error> {
        self.get_mut(session_id, |session| {
            session.disassemble(memory_reference, instruction_offset, instruction_count)
        })
        .ok_or(Error::SessionNotFound)?
    }

    fn get<F, R>(&self, session_id: usize, f: F) -> Option<R>
    where
        F: FnOnce(&Session) -> R,
    {
        self.sessions.get(session_id).map(f)
    }

    fn get_mut<F, R>(&mut self, session_id: usize, f: F) -> Option<R>
    where
        F: FnOnce(&mut Session) -> R,
    {
        self.sessions.get_mut(session_id).map(f)
    }
}
