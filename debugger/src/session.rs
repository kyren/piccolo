use std::{
    collections::HashMap,
    sync::mpsc::{Receiver, Sender},
    thread::JoinHandle,
};

mod debug;
mod request;
mod response;

use piccolo::{Closure, Executor, Lua};

use crate::{
    breakpoints::Breakpoints,
    watch::{WatchEntry, WatchSpec},
    Disassembled, Error, Location, StopReason, WatchMode,
};

use self::{request::Request, response::Response};

#[derive(Debug)]
pub struct Session {
    id: usize,
    source: String,
    path: String,
    thread: Option<JoinHandle<()>>,
    sender: Option<Sender<Request>>,
    receiver: Option<Receiver<Response>>,
}

impl<'gc> Session {
    pub fn new(id: usize, source: impl Into<String>, path: impl Into<String>) -> Self {
        Self {
            id,
            source: source.into(),
            path: path.into(),
            thread: None,
            sender: None,
            receiver: None,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.source
    }

    pub fn launch(&mut self) {
        let (first, second) = std::sync::mpsc::channel();
        let (thirst, fourth) = std::sync::mpsc::channel();
        self.sender = Some(first);
        self.receiver = Some(fourth);

        let source = self.source.as_bytes().to_vec();
        let path = self.path.to_owned();
        let thread = std::thread::spawn(move || {
            let mut lua = Lua::full();
            lua.enter(|ctx| {
                let mut breakpoints = Breakpoints::default();
                let mut watchpoints = Vec::new();
                let closure = Closure::load(ctx, Some(&path), &source).unwrap();
                let executor = Executor::start(ctx, closure.into(), ());

                loop {
                    match second.recv() {
                        Ok(message) => match message {
                            Request::AddBreakpoint { chunk, line } => {
                                let id = breakpoints.add(chunk, line);
                                if let Err(_) = thirst.send(Response::BreakpointAdded(id)) {
                                    break;
                                }
                            }
                            Request::RemoveBreakpoint { chunk, line } => {
                                breakpoints.remove(&chunk, line);
                                if let Err(_) = thirst.send(Response::BreakpointRemoved) {
                                    break;
                                }
                            }
                            Request::AddFunctionBreakpoint(name) => {
                                let id = debug::resolve_function_breakpoint(
                                    executor,
                                    ctx,
                                    name.as_str(),
                                )
                                .map(|(chunk, line)| breakpoints.add(chunk, line));
                                if let Err(_) = thirst.send(Response::FunctionBreakpointAdded(id)) {
                                    break;
                                }
                            }
                            Request::ListBreakpoints => {
                                let list = breakpoints.list();
                                if let Err(_) = thirst.send(Response::ListBreakpoints(list)) {
                                    break;
                                }
                            }
                            Request::MatchesBreakpoint { chunk, line } => {
                                let matches = breakpoints.matches(&chunk, line);
                                if let Err(_) = thirst.send(Response::MatchesBreakpoint(matches)) {
                                    break;
                                }
                            }
                            Request::WatchRegister { register, mode } => {
                                watchpoints.push(WatchEntry {
                                    spec: WatchSpec::Register(register),
                                    last: None,
                                    mode: mode.unwrap_or(WatchMode::Modify),
                                    last_line_seen: None,
                                });
                                if let Err(_) = thirst.send(Response::WatchRegisterAdded) {
                                    break;
                                }
                            }
                            Request::WatchGlobal { name, mode } => {
                                watchpoints.push(WatchEntry {
                                    spec: WatchSpec::Global(name),
                                    last: None,
                                    mode: mode.unwrap_or(WatchMode::Modify),
                                    last_line_seen: None,
                                });
                                if let Err(_) = thirst.send(Response::WatchGlobalAdded) {
                                    break;
                                }
                            }
                            Request::ContinueRun => {
                                let stop_reason = debug::continue_run(
                                    executor,
                                    ctx,
                                    &breakpoints,
                                    &mut watchpoints,
                                );
                                if let Err(_) = thirst.send(Response::ContinueRun(stop_reason)) {
                                    break;
                                }
                            }
                            Request::StepInto => {
                                let stop_reason =
                                    debug::step_into(executor, ctx, &breakpoints, &mut watchpoints);
                                if let Err(_) = thirst.send(Response::StepInto(stop_reason)) {
                                    break;
                                }
                            }
                            Request::StepOver => {
                                let stop_reason =
                                    debug::step_over(executor, ctx, &breakpoints, &mut watchpoints);
                                if let Err(_) = thirst.send(Response::StepOver(stop_reason)) {
                                    break;
                                }
                            }
                            Request::StepOut => {
                                let stop_reason =
                                    debug::step_out(executor, ctx, &breakpoints, &mut watchpoints);
                                if let Err(_) = thirst.send(Response::StepOut(stop_reason)) {
                                    break;
                                }
                            }
                            Request::Backtrace => {
                                let backtrace = debug::backtrace(executor);
                                if let Err(_) = thirst.send(Response::Backtrace(backtrace)) {
                                    break;
                                }
                            }
                            Request::StackSnapshot => {
                                let stack_snapshot = debug::stack_snapshot(executor);
                                if let Err(_) = thirst.send(Response::StackSnapshot(stack_snapshot))
                                {
                                    break;
                                }
                            }
                            Request::ReadRegister(register) => {
                                let register = debug::read_register(executor, register);
                                if let Err(_) = thirst.send(Response::ReadRegister(register)) {
                                    break;
                                }
                            }
                            Request::ReadRegisters => {
                                let registers = debug::read_registers(executor);
                                if let Err(_) = thirst.send(Response::ReadRegisters(registers)) {
                                    break;
                                }
                            }
                            Request::ReadGlobal(name) => {
                                let global = debug::read_global(ctx, name.as_str());
                                if let Err(_) = thirst.send(Response::ReadGlobal(global)) {
                                    break;
                                }
                            }
                            Request::ReadMemory(memory_reference, byte_offset, byte_count) => {
                                let memory = debug::read_memory(
                                    executor,
                                    memory_reference.as_str(),
                                    byte_offset,
                                    byte_count,
                                );
                                if let Err(_) = thirst.send(Response::ReadMemory(memory)) {
                                    break;
                                }
                            }
                            Request::Disassemble(
                                memory_reference,
                                instruction_offset,
                                instruction_count,
                            ) => {
                                let disassembled = debug::disassemble(
                                    executor,
                                    memory_reference.as_str(),
                                    instruction_offset,
                                    instruction_count,
                                );
                                if let Err(_) = thirst.send(Response::Disassemble(disassembled)) {
                                    break;
                                }
                            }
                            Request::ReadUpvalues => {
                                let upvalues = debug::read_upvalues(executor);
                                if let Err(_) = thirst.send(Response::ReadUpvalues(upvalues)) {
                                    break;
                                }
                            }
                        },
                        _ => break,
                    }
                }
            });
        });

        self.thread = Some(thread);
    }

    pub fn restart(&mut self) {
        self.thread.take().map(|thread| thread.join().unwrap());
        self.sender.take();
        self.receiver.take();
        self.launch();
    }

    pub fn disconnect(&mut self) {
        self.sender.take();
    }

    pub fn terminate(&mut self) {
        self.thread.take().map(|thread| thread.join().unwrap());
        self.sender.take();
    }

    pub fn add_breakpoint(&mut self, chunk: String, line: usize) -> Result<usize, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::AddBreakpoint { chunk, line })
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::BreakpointAdded(id)) => Ok(id),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn add_function_breakpoint(&mut self, name: String) -> Result<Option<usize>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::AddFunctionBreakpoint(name))
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::FunctionBreakpointAdded(id)) => Ok(id),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn remove_breakpoint(&mut self, chunk: String, line: usize) -> Result<(), Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::RemoveBreakpoint { chunk, line })
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::BreakpointRemoved) => Ok(()),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn list_breakpoints(&self) -> Result<HashMap<String, Vec<usize>>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ListBreakpoints)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ListBreakpoints(list)) => Ok(list),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn matches_breakpoint(&self, chunk: String, line: usize) -> Result<Vec<usize>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::MatchesBreakpoint { chunk, line })
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::MatchesBreakpoint(matches)) => Ok(matches),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn watch_register(
        &mut self,
        register: usize,
        mode: Option<WatchMode>,
    ) -> Result<(), Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::WatchRegister { register, mode })
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::WatchRegisterAdded) => Ok(()),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn watch_global(
        &mut self,
        name: impl Into<String>,
        mode: Option<WatchMode>,
    ) -> Result<(), Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::WatchGlobal {
                name: name.into(),
                mode,
            })
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::WatchGlobalAdded) => Ok(()),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn continue_run(&mut self) -> Result<StopReason, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ContinueRun)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ContinueRun(stop_reason)) => stop_reason,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn step_into(&mut self) -> Result<StopReason, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::StepInto)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::StepInto(stop_reason)) => stop_reason,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn step_over(&mut self) -> Result<StopReason, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::StepOver)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::StepOver(stop_reason)) => stop_reason,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn step_out(&mut self) -> Result<StopReason, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::StepOut)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::StepOut(stop_reason)) => stop_reason,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn backtrace(&mut self) -> Result<Vec<Location>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::Backtrace)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::Backtrace(backtrace)) => backtrace,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn stack_snapshot(&mut self) -> Result<Vec<(Location, Vec<String>)>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::StackSnapshot)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::StackSnapshot(stack_snapshot)) => stack_snapshot,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn read_register(&mut self, register: usize) -> Result<Option<String>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ReadRegister(register))
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ReadRegister(register)) => register,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn read_upvalues(&mut self) -> Result<Option<Vec<String>>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ReadUpvalues)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ReadUpvalues(upvalues)) => upvalues,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn read_registers(&mut self) -> Result<Option<Vec<String>>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ReadRegisters)
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ReadRegisters(registers)) => registers,
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn read_global(&mut self, name: String) -> Result<String, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ReadGlobal(name))
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ReadGlobal(global)) => Ok(global),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn read_memory(
        &mut self,
        memory_reference: String,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Result<Option<Vec<u8>>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::ReadMemory(
                memory_reference,
                byte_offset,
                byte_count,
            ))
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::ReadMemory(memory)) => Ok(memory),
            _ => Err(Error::UnexpectedResponse),
        }
    }

    pub fn disassemble(
        &mut self,
        memory_reference: String,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Result<Option<Vec<Disassembled>>, Error> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Error::SenderDead);
        };

        sender
            .send(Request::Disassemble(
                memory_reference,
                instruction_offset,
                instruction_count,
            ))
            .map_err(|_| Error::ReceiverDead)?;

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Error::ReceiverDead);
        };

        match receiver.recv() {
            Ok(Response::Disassemble(disassemble)) => Ok(disassemble),
            _ => Err(Error::UnexpectedResponse),
        }
    }
}
