use piccolo::{
    compiler::LineNumber,
    opcode::OpCode,
    thread::{vm::run_vm, Frame, LuaFrame},
    Context, Executor, ExecutorMode, Fuel, FunctionPrototype, String as LuaString, Value,
};

use crate::{
    breakpoints::Breakpoints,
    watch::{WatchEntry, WatchSpec},
    Disassembled, Error, Location, StopReason, WatchMode,
};

fn current_location_from_state_in_thread<'gc>(
    thread_state: &piccolo::thread::ThreadState<'gc>,
) -> Option<Location> {
    match thread_state.frames.last() {
        Some(Frame::Lua { closure, pc, .. }) => {
            let proto = closure.prototype();
            let chunk_name = lua_string_to_string(proto.chunk_name);
            let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
            Some(Location::new(
                chunk_name,
                proto.reference.map_strings(lua_string_to_string),
                line,
            ))
        }
        _ => None,
    }
}

fn opcode_index_to_line(op_lines: &[(usize, LineNumber)], pc: usize) -> usize {
    if op_lines.is_empty() {
        return 0;
    }
    match op_lines.binary_search_by_key(&pc, |(opi, _)| *opi) {
        Ok(i) => op_lines[i].1 .0 as usize,
        Err(0) => op_lines[0].1 .0 as usize,
        Err(i) => op_lines[i - 1].1 .0 as usize,
    }
}

/// Calculates how many VM instructions remain in the current source line starting at `pc`.
/// This uses the prototype's `opcode_line_numbers` table which stores boundaries where the
/// line number changes.
fn instructions_to_end_of_line(prototype: &FunctionPrototype, pc: usize) -> u32 {
    let op_lines = &prototype.opcode_line_numbers;
    if op_lines.is_empty() {
        return 1;
    }

    let seg_index = match op_lines.binary_search_by_key(&pc, |(opi, _)| *opi) {
        Ok(i) => i,
        Err(0) => 0,
        Err(i) => i - 1,
    };

    let end_op_index = op_lines
        .get(seg_index + 1)
        .map(|(opi, _)| *opi)
        .unwrap_or(prototype.opcodes.len());

    let remaining = end_op_index.saturating_sub(pc);
    u32::try_from(remaining.max(1)).unwrap_or(1)
}

fn value_to_string<'gc>(value: Value<'gc>) -> String {
    format!("{}", value.display())
}

fn lua_string_to_string(s: LuaString<'_>) -> String {
    format!("{}", s.display_lossy())
}

fn current_stack_depth<'gc>(executor: Executor<'gc>) -> Result<usize, Error> {
    with_top_thread_state(executor, |state| state.frames.len())
}

fn stack_depth_and_location<'gc>(
    executor: Executor<'gc>,
) -> Result<(usize, Option<Location>), Error> {
    with_top_thread_state(executor, |state| {
        let depth = state.frames.len();
        let loc = current_location_from_state_in_thread(state);
        (depth, loc)
    })
}

fn current_location<'gc>(executor: Executor<'gc>) -> Result<Option<Location>, Error> {
    with_top_thread_state(executor, |state| {
        current_location_from_state_in_thread(state)
    })
}

fn with_top_thread_state<'gc, R>(
    executor: Executor<'gc>,
    f: impl FnOnce(&piccolo::thread::ThreadState<'gc>) -> R,
) -> Result<R, Error> {
    let guard = executor.0.borrow();
    let thread = *guard.thread_stack.last().ok_or(Error::ExecutorFinished)?;
    drop(guard);
    let thread_state = thread.into_inner().borrow();
    Ok(f(&thread_state))
}

fn with_top_thread_state_mut<'gc, R>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    f: impl FnOnce(piccolo::Thread<'gc>, &mut piccolo::thread::ThreadState<'gc>) -> R,
) -> Result<R, Error> {
    let guard = executor.0.borrow();
    let top_thread = *guard.thread_stack.last().ok_or(Error::ExecutorFinished)?;
    drop(guard);
    let mut thread_state = top_thread.into_inner().borrow_mut(&ctx);
    let r = f(top_thread, &mut thread_state);
    drop(thread_state);
    Ok(r)
}

fn stop_if_breakpoint(location: &Location, breakpoints: &Breakpoints) -> Option<StopReason> {
    let breakpoint_ids = breakpoints.matches(location.chunk(), location.line());
    if breakpoint_ids.is_empty() {
        None
    } else {
        Some(StopReason::Breakpoint {
            location: location.to_owned(),
            breakpoint_ids,
        })
    }
}

fn eval_watch<'gc>(spec: &WatchSpec, executor: Executor<'gc>, ctx: Context<'gc>) -> Option<String> {
    match spec {
        WatchSpec::Register(index) => with_top_thread_state(executor, |state| {
            if let Some(Frame::Lua {
                base, stack_size, ..
            }) = state.frames.last()
            {
                let index = *index;
                let base = *base;
                let size = *stack_size;
                if index < size {
                    return Some(state.stack[base + index].display().to_string());
                }
            }
            None
        })
        .ok()?,
        WatchSpec::Global(name) => {
            let key = ctx.intern(name.as_bytes());
            Some(ctx.globals().get_value(ctx, key).display().to_string())
        }
    }
}

pub fn resolve_function_breakpoint<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    name: &str,
) -> Option<(String, usize)> {
    fn find_named<'gc>(proto: &FunctionPrototype<'gc>, target: &str) -> Option<(String, usize)> {
        for child in proto.prototypes.iter() {
            match child.reference {
                piccolo::compiler::FunctionRef::Named(n, _) => {
                    let child_name = lua_string_to_string(n);
                    if child_name == target {
                        let chunk_name = lua_string_to_string(child.chunk_name);
                        let first_line = if child.opcode_line_numbers.is_empty() {
                            0
                        } else {
                            child.opcode_line_numbers[0].1 .0 as usize
                        };
                        return Some((chunk_name, first_line));
                    }
                }
                _ => {}
            }
            if let Some(found) = find_named(&*child, target) {
                return Some(found);
            }
        }
        None
    }

    let last_segment = name.rsplit('.').next().unwrap_or(name);

    if let Ok(found) = with_top_thread_state(executor, |state| match state.frames.last() {
        Some(Frame::Lua { closure, .. }) => {
            let root = closure.prototype();
            find_named(&root, last_segment)
        }
        Some(Frame::Start(func)) => match func {
            piccolo::Function::Closure(c) => {
                let root = c.prototype();
                find_named(&root, last_segment)
            }
            _ => None,
        },
        _ => None,
    }) {
        if let Some(pair) = found {
            return Some(pair);
        }
    }

    let env = ctx.globals();
    let mut segments = name.split('.');
    let first = segments.next()?;
    let mut current = env.get_value(ctx, first.to_string());
    for segment in segments {
        match current {
            Value::Table(t) => {
                current = t.get_raw(Value::String(ctx.intern(segment.as_bytes())));
            }
            _ => return None,
        }
    }
    if let Value::Function(piccolo::Function::Closure(closure)) = current {
        let proto = closure.prototype();
        let chunk_name = lua_string_to_string(proto.chunk_name);
        let first_line = if proto.opcode_line_numbers.is_empty() {
            0
        } else {
            proto.opcode_line_numbers[0].1 .0 as usize
        };
        Some((chunk_name, first_line))
    } else {
        None
    }
}

fn describe_watch_change<'gc>(
    spec: &WatchSpec,
    old: &Option<String>,
    new: &Option<String>,
) -> String {
    let spec = match spec {
        WatchSpec::Register(index) => format!("register[{}]", index),
        WatchSpec::Global(name) => format!("global['{}']", name),
    };
    format!(
        "{}: {} -> {}",
        spec,
        old.as_ref().unwrap_or(&"<none>".to_string()),
        new.as_ref().unwrap_or(&"<none>".to_string())
    )
}

fn poll_watchpoints<'gc>(
    watchpoints: &mut Vec<WatchEntry>,
    executor: Executor<'gc>,
    ctx: Context<'gc>,
) -> Option<String> {
    for index in 0..watchpoints.len() {
        let Some(entry) = watchpoints.get(index) else {
            continue;
        };
        let entry = entry.to_owned();
        let spec = match &entry.spec {
            WatchSpec::Register(i) => WatchSpec::Register(*i),
            WatchSpec::Global(n) => WatchSpec::Global(n.clone()),
        };
        let last = entry.last;

        let current = eval_watch(&spec, executor, ctx);
        let should_fire = match entry.mode {
            WatchMode::Modify => match (last.as_ref(), current.as_ref()) {
                (None, Some(_)) => true,
                (Some(_), None) => true,
                (Some(left), Some(right)) => left != right,
                (None, None) => false,
            },
            WatchMode::Access => {
                if current.is_some() {
                    let current_line = current_location(executor)
                        .ok()
                        .flatten()
                        .map(|l| (l.chunk().to_owned(), l.line()));
                    if let (Some((chunk, line)), Some(entry)) =
                        (current_line, watchpoints.get(index))
                    {
                        if entry.last_line_seen != Some((chunk.to_owned(), line)) {
                            if let Some(entry) = watchpoints.get_mut(index) {
                                entry.last_line_seen = Some((chunk.to_owned(), line));
                            }
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        };
        if should_fire {
            let message = format!(
                "watchpoint hit: {}",
                describe_watch_change(&spec, &last, &current)
            );
            if let Some(entry) = watchpoints.get_mut(index) {
                entry.last = current;
            }
            return Some(message);
        }
        if let Some(entry) = watchpoints.get_mut(index) {
            entry.last = current;
        }
    }
    None
}

fn step_one<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    watchpoints: &mut Vec<WatchEntry>,
) -> Result<StopReason, Error> {
    const FUEL_NON_LUA_CALLBACK_STEP: i32 = 11;
    const FUEL_NON_LUA_SEQUENCE_STEP: i32 = 7;
    const FUEL_NON_LUA_ERROR_STEP: i32 = 3;

    match executor.mode() {
        ExecutorMode::Stopped => return Err(Error::ExecutorFinished),
        ExecutorMode::Result => return Err(Error::ExecutorFinished),
        ExecutorMode::Suspended => return Ok(StopReason::Suspended),
        ExecutorMode::Running => return Err(Error::ExecutorAlreadyRunning),
        ExecutorMode::Normal => {}
    }

    let (is_lua_top, non_lua_fuel) =
        match with_top_thread_state(executor, |state| match state.frames.last() {
            Some(Frame::Lua { .. }) => (true, 0),
            Some(Frame::Callback { .. }) => (false, FUEL_NON_LUA_CALLBACK_STEP),
            Some(Frame::Sequence { .. }) => (false, FUEL_NON_LUA_SEQUENCE_STEP),
            Some(Frame::Error(_)) => (false, FUEL_NON_LUA_ERROR_STEP),
            Some(Frame::Result { .. }) => (false, -1),
            Some(Frame::WaitThread) | Some(Frame::Yielded) | Some(Frame::Start(_)) => (false, 3),
            None => (false, -1),
        }) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };

    if non_lua_fuel == -1 {
        return Err(Error::ExecutorFinished);
    }

    if is_lua_top {
        let _ = with_top_thread_state_mut(executor, ctx, |thread, state| {
            let mut max_instructions: u32 = 1;
            if let Some(Frame::Lua { closure, pc, .. }) = state.frames.last() {
                let proto = closure.prototype();
                max_instructions = instructions_to_end_of_line(&proto, *pc);
            }

            let mut fuel = Fuel::with(1_000_000);
            let lua_frame = LuaFrame {
                state,
                thread,
                fuel: &mut fuel,
            };
            let _ = run_vm(ctx, lua_frame, max_instructions);
        });
    } else {
        let mut fuel = Fuel::with(non_lua_fuel);
        let _ = executor.step(ctx, &mut fuel);
    }

    let location = match current_location(executor) {
        Ok(Some(location)) => location,
        Ok(None) => return Err(Error::ExecutorFinished),
        Err(err) => return Err(err),
    };
    if let Some(hit) = poll_watchpoints(watchpoints, executor, ctx) {
        return Ok(StopReason::Watchpoint(hit));
    }
    Ok(StopReason::Step(location))
}

pub fn continue_run<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    breakpoints: &Breakpoints,
    watchpoints: &mut Vec<WatchEntry>,
) -> Result<StopReason, Error> {
    loop {
        match step_one(executor, ctx, watchpoints)? {
            StopReason::Step(location) => {
                if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                    return Ok(stop_reason);
                }
            }
            other => return Ok(other),
        }
    }
}

pub fn step_into<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    breakpoints: &Breakpoints,
    watchpoints: &mut Vec<WatchEntry>,
) -> Result<StopReason, Error> {
    let start_location = match current_location(executor) {
        Ok(Some(location)) => location,
        Ok(None) => return Err(Error::ExecutorFinished),
        Err(err) => return Err(err),
    };

    loop {
        match step_one(executor, ctx, watchpoints)? {
            StopReason::Step(location) => {
                if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                    return Ok(stop_reason);
                }
                if location.line() != start_location.line()
                    || location.chunk() != start_location.chunk()
                {
                    return Ok(StopReason::Step(location));
                }
            }
            StopReason::Watchpoint(hit) => return Ok(StopReason::Watchpoint(hit)),
            other => return Ok(other),
        }
    }
}

pub fn step_over<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    breakpoints: &Breakpoints,
    watchpoints: &mut Vec<WatchEntry>,
) -> Result<StopReason, Error> {
    let (start_depth, start_location) = match stack_depth_and_location(executor) {
        Ok((depth, Some(location))) => (depth, location),
        Ok((_depth, None)) => return Err(Error::ExecutorFinished),
        Err(err) => return Err(err),
    };

    loop {
        match step_one(executor, ctx, watchpoints)? {
            StopReason::Step(location) => {
                if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                    return Ok(stop_reason);
                }
                let depth = match current_stack_depth(executor) {
                    Ok(depth) => depth,
                    Err(err) => return Err(err),
                };
                if depth <= start_depth
                    && (location.line() != start_location.line()
                        || location.chunk() != start_location.chunk())
                {
                    return Ok(StopReason::Step(location));
                }
            }
            StopReason::Watchpoint(hit) => return Ok(StopReason::Watchpoint(hit)),
            other => return Ok(other),
        }
    }
}

pub fn step_out<'gc>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    breakpoints: &Breakpoints,
    watchpoints: &mut Vec<WatchEntry>,
) -> Result<StopReason, Error> {
    let start_depth = match current_stack_depth(executor) {
        Ok(depth) => depth,
        Err(err) => return Err(err),
    };

    loop {
        match step_one(executor, ctx, watchpoints)? {
            StopReason::Step(location) => {
                if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                    return Ok(stop_reason);
                }
                let depth = match current_stack_depth(executor) {
                    Ok(depth) => depth,
                    Err(err) => return Err(err),
                };
                if depth < start_depth {
                    return Ok(StopReason::Step(location));
                }
            }
            StopReason::Watchpoint(hit) => return Ok(StopReason::Watchpoint(hit)),
            other => return Ok(other),
        }
    }
}

pub fn backtrace<'gc>(executor: Executor<'gc>) -> Result<Vec<Location>, Error> {
    with_top_thread_state(executor, |state| {
        let mut out = Vec::with_capacity(state.frames.len());
        for frame in state.frames.iter() {
            if let Frame::Lua { closure, pc, .. } = frame {
                let proto = closure.prototype();
                let chunk_name = lua_string_to_string(proto.chunk_name);
                let function_ref = proto.reference.map_strings(lua_string_to_string);
                let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
                out.push(Location::new(chunk_name, function_ref, line));
            }
        }
        out
    })
}

pub fn read_register<'gc>(
    executor: Executor<'gc>,
    register_index: usize,
) -> Result<Option<String>, Error> {
    with_top_thread_state(executor, |state| {
        if let Some(Frame::Lua {
            base, stack_size, ..
        }) = state.frames.last()
        {
            let base = *base;
            let size = *stack_size;
            if register_index < size {
                return Some(value_to_string(state.stack[base + register_index]));
            }
        }
        None
    })
}

pub fn read_global<'gc>(ctx: Context<'gc>, name: &str) -> String {
    let key = ctx.intern(name.as_bytes());
    value_to_string(ctx.globals().get_value(ctx, key))
}

pub fn read_registers<'gc>(executor: Executor<'gc>) -> Result<Option<Vec<String>>, Error> {
    with_top_thread_state(executor, |state| {
        if let Some(Frame::Lua {
            base, stack_size, ..
        }) = state.frames.last()
        {
            let base = *base;
            let size = *stack_size;
            return Some(
                state.stack[base..base + size]
                    .iter()
                    .map(|v| value_to_string(*v))
                    .collect(),
            );
        }
        None
    })
}

pub fn read_upvalues<'gc>(executor: Executor<'gc>) -> Result<Option<Vec<String>>, Error> {
    with_top_thread_state(executor, |state| {
        if let Some(Frame::Lua { closure, .. }) = state.frames.last() {
            let mut values = Vec::new();
            for &upvalue in closure.upvalues() {
                if let piccolo::closure::UpValueState::Closed(value) = upvalue.get() {
                    values.push(value_to_string(value));
                } else {
                    values.push(String::new());
                }
            }
            return Some(values);
        }
        None
    })
}

pub fn stack_snapshot<'gc>(executor: Executor<'gc>) -> Result<Vec<(Location, Vec<String>)>, Error> {
    with_top_thread_state(executor, |state| {
        let mut out = Vec::with_capacity(state.frames.len());
        for frame in state.frames.iter() {
            if let Frame::Lua {
                base,
                stack_size,
                closure,
                pc,
                ..
            } = frame
            {
                let proto = closure.prototype();
                let chunk_name = lua_string_to_string(proto.chunk_name);
                let function_ref = proto.reference.map_strings(lua_string_to_string);
                let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
                let location = Location::new(chunk_name, function_ref, line);
                let base = *base;
                let size = *stack_size;
                out.push((
                    location,
                    state.stack[base..base + size]
                        .iter()
                        .map(|v| value_to_string(*v))
                        .collect(),
                ));
            }
        }
        out
    })
}

fn disassemble_frame<'gc>(
    executor: Executor<'gc>,
    frame: usize,
    instruction_offset: Option<usize>,
    instruction_count: Option<usize>,
) -> Option<Vec<Disassembled>> {
    with_top_thread_state(executor, |state| {
        if let Some(Frame::Lua { closure, pc, .. }) = if frame == usize::MAX {
            state.frames.last()
        } else {
            state.frames.get(frame)
        } {
            let prototype = closure.prototype();
            let current_pc = *pc;
            Some(disassemble_prototype_opcodes(
                &prototype,
                Some(current_pc),
                instruction_offset,
                instruction_count,
            ))
        } else {
            None
        }
    })
    .ok()?
}

fn disassemble_prototype<'gc>(
    executor: Executor<'gc>,
    prototype: &str,
    instruction_offset: Option<usize>,
    instruction_count: Option<usize>,
) -> Option<Vec<Disassembled>> {
    let (chunk, index) = prototype.split_once(':')?;

    let index: usize = index.parse().ok()?;

    with_top_thread_state(executor, |state| {
        for frame in state.frames.iter() {
            if let Frame::Lua { closure, .. } = frame {
                let proto = closure.prototype();
                let frame_chunk = lua_string_to_string(proto.chunk_name);

                if frame_chunk == chunk {
                    if let Some(nested_proto) = proto.prototypes.get(index) {
                        return Some(disassemble_prototype_opcodes(
                            nested_proto,
                            None,
                            instruction_offset,
                            instruction_count,
                        ));
                    }
                }
            }
        }
        None
    })
    .ok()?
}

fn disassemble_prototype_opcodes<'gc>(
    prototype: &FunctionPrototype,
    current_pc: Option<usize>,
    instruction_offset: Option<usize>,
    instruction_count: Option<usize>,
) -> Vec<Disassembled> {
    let start_offset = instruction_offset.unwrap_or(0);
    let max_count = instruction_count.unwrap_or(prototype.opcodes.len());

    let end_offset = std::cmp::min(start_offset + max_count, prototype.opcodes.len());

    let mut disassembled = Vec::with_capacity(end_offset - start_offset);

    for index in start_offset..end_offset {
        if let Some(opcode) = prototype.opcodes.get(index) {
            let operation = (*opcode).decode();
            let line = opcode_index_to_line(&prototype.opcode_line_numbers, index) + 1;
            let symbol = if Some(index) == current_pc {
                "=>"
            } else {
                "  "
            };
            disassembled.push(Disassembled {
                symbol,
                index,
                line,
                operation,
            });
        }
    }

    disassembled
}

pub fn disassemble<'gc>(
    executor: Executor<'gc>,
    memory_reference: &str,
    instruction_offset: Option<usize>,
    instruction_count: Option<usize>,
) -> Option<Vec<Disassembled>> {
    let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));
    match ty {
        "frame" => disassemble_frame(
            executor,
            if s.is_empty() {
                usize::MAX
            } else {
                s.parse().ok()?
            },
            instruction_offset,
            instruction_count,
        ),
        "prototype" => disassemble_prototype(executor, s, instruction_offset, instruction_count),
        _ => None,
    }
}

pub fn read_memory<'gc>(
    executor: Executor<'gc>,
    memory_reference: &str,
    byte_offset: Option<usize>,
    byte_count: Option<usize>,
) -> Option<Vec<u8>> {
    let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));

    match ty {
        "frame" => read_frame_memory(
            executor,
            if s.is_empty() {
                usize::MAX
            } else {
                s.parse().ok()?
            },
            byte_offset,
            byte_count,
        ),
        "prototype" => read_prototype_memory(executor, s, byte_offset, byte_count),
        _ => None,
    }
}

fn read_frame_memory<'gc>(
    executor: Executor<'gc>,
    frame: usize,
    byte_offset: Option<usize>,
    byte_count: Option<usize>,
) -> Option<Vec<u8>> {
    with_top_thread_state(executor, |state| {
        if let Some(piccolo::thread::Frame::Lua { closure, .. }) = if frame == usize::MAX {
            state.frames.last()
        } else {
            state.frames.get(frame)
        } {
            let prototype = closure.prototype();
            Some(read_opcodes(&prototype.opcodes, byte_offset, byte_count))
        } else {
            None
        }
    })
    .ok()?
}

fn read_prototype_memory<'gc>(
    executor: Executor<'gc>,
    prototype: &str,
    byte_offset: Option<usize>,
    byte_count: Option<usize>,
) -> Option<Vec<u8>> {
    let (chunk, index) = prototype.split_once(':')?;

    let index: usize = index.parse().ok()?;

    with_top_thread_state(executor, |state| {
        for frame in state.frames.iter() {
            if let Frame::Lua { closure, .. } = frame {
                let proto = closure.prototype();
                let frame_chunk = lua_string_to_string(proto.chunk_name);

                if frame_chunk == chunk {
                    if let Some(nested_proto) = proto.prototypes.get(index) {
                        return Some(read_opcodes(&nested_proto.opcodes, byte_offset, byte_count));
                    }
                }
            }
        }
        None
    })
    .ok()?
}

fn read_opcodes<'gc>(
    opcodes: &[OpCode],
    byte_offset: Option<usize>,
    byte_count: Option<usize>,
) -> Vec<u8> {
    fn to_bytes<T: Copy>(value: T) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(size_of::<T>());

        // SAFETY: We are copying the bytes of a plain-old-data (POD) type T into a Vec<u8> with enough capacity.
        // The Vec is pre-allocated with the correct size, and we set the length after copying.
        unsafe {
            std::ptr::copy_nonoverlapping(
                std::ptr::addr_of!(value).cast::<u8>(),
                bytes.as_mut_ptr(),
                size_of::<T>(),
            );
            bytes.set_len(size_of::<T>());
        }

        bytes
    }

    let mut raw = Vec::new();

    for opcode in opcodes {
        let bytes = to_bytes(*opcode);
        raw.extend_from_slice(&bytes);
    }

    let byte_offset = byte_offset.unwrap_or(0);
    let byte_count = byte_count.unwrap_or(raw.len());

    let start = std::cmp::min(byte_offset, raw.len());
    let end = std::cmp::min(start + byte_count, raw.len());

    raw[start..end].to_vec()
}
