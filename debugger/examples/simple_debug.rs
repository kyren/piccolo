use piccolo::compiler::LineNumber;
use piccolo_debugger::{Debugger, Error, Location, StopReason};

fn main() -> Result<(), Error> {
    let source = r#"
            x = 0
            local outer = 10
            function foo(n)
                outer = outer + n
                x = x + n
                return outer + x
            end

            function bar()
                local y = foo(1)
                return y
            end

            local z = bar()
        "#;
    let path = "test.lua";

    let mut debugger = Debugger::new();
    let session_id = debugger.add_session(source, path);

    debugger.launch(session_id);

    debugger.add_breakpoint(session_id, "test.lua".to_string(), 3)?;
    debugger.add_breakpoint(session_id, "test.lua".to_string(), 9)?;
    debugger.add_breakpoint(session_id, "test.lua".to_string(), 7)?;

    debugger.add_function_breakpoint(session_id, "bar".to_string())?;

    let breakpoints = debugger.list_breakpoints(session_id)?;

    assert_eq!(breakpoints.len(), 1);
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&3)),
        Some(true)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&9)),
        Some(true)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&7)),
        Some(true)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&10)),
        Some(true)
    ); // function breakpoint

    println!("Breakpoints:");
    for (source, lines) in debugger.list_breakpoints(session_id)? {
        let mut v: Vec<usize> = lines.iter().copied().collect();
        v.sort_unstable();
        println!("  {source}: {:?}", v);
    }

    debugger.remove_breakpoint(session_id, "test.lua".to_string(), 7)?;

    let breakpoints = debugger.list_breakpoints(session_id)?;
    assert_eq!(breakpoints.len(), 1);
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&3)),
        Some(true)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&9)),
        Some(true)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&7)),
        Some(false)
    );
    assert_eq!(
        breakpoints.get("test.lua").map(|b| b.contains(&10)),
        Some(true)
    ); // function breakpoint

    println!("breakpoints: {:?}", debugger.list_breakpoints(session_id)?);

    debugger.watch_global(session_id, "x".to_string(), None)?;

    println!("Continue…");
    let mut stop = debugger.continue_run(session_id)?;
    assert_eq!(
        stop,
        StopReason::Watchpoint("watchpoint hit: global['x']: <none> -> 0".to_string())
    );
    println!("Stopped: {:?}", stop);

    let backtrace = debugger.backtrace(session_id)?;
    assert_eq!(backtrace.len(), 1);
    assert_eq!(backtrace.first().map(|b| b.chunk()), Some("test.lua"));
    assert_eq!(backtrace.first().map(|b| b.line()), Some(2));
    assert_eq!(
        backtrace.first().map(|b| b.function_ref().to_string()),
        Some("<chunk>".to_string())
    );

    println!("Backtrace:");
    for (index, location) in backtrace.iter().enumerate() {
        println!(
            "  #{index} {}:{} {}",
            location.chunk(),
            location.line(),
            location.function_ref()
        );
    }

    let disassembled_instructions = debugger.disassemble(session_id, "".to_string(), None, None)?;

    assert_eq!(
        disassembled_instructions.as_ref().map(|d| d.len()),
        Some(10)
    );

    if let Some(disassembled_instructions) = disassembled_instructions {
        println!("Disassembly (current function):");
        for instruction in disassembled_instructions.iter().take(10) {
            println!("  {instruction:?}");
        }
    }

    if let Some(readed_memory) = debugger.read_memory(session_id, "".to_string(), None, None)? {
        println!("Readed: {:?}", readed_memory);
    }

    if let Some(registers) = debugger.read_registers(session_id)? {
        println!("Top frame registers ({}):", registers.len());
        for (index, value) in registers.iter().enumerate() {
            println!("  r{index} = {}", value);
        }
    }

    println!("Stack snapshot (before stepping):");
    for (location, registers) in debugger.stack_snapshot(session_id)? {
        println!(
            "  {}:{} {} ({} regs)",
            location.chunk(),
            location.line(),
            location.function_ref(),
            registers.len()
        );
    }

    println!("Step into…");
    stop = debugger.step_into(session_id)?;
    assert_eq!(
        stop,
        StopReason::Breakpoint {
            location: Location::new(
                "test.lua".to_string(),
                piccolo::compiler::FunctionRef::Chunk,
                3
            ),
            breakpoint_ids: vec![0]
        }
    );
    println!("Stopped: {:?}", stop);

    if let Some(upvalues) = debugger.read_upvalues(session_id)? {
        println!("Upvalues ({}):", upvalues.len());
        for (index, value) in upvalues.iter().enumerate() {
            println!("  uv{index} = {}", value);
        }
    }

    println!("Step over...");
    stop = debugger.step_over(session_id)?;
    assert_eq!(
        stop,
        StopReason::Breakpoint {
            location: Location::new(
                "test.lua".to_string(),
                piccolo::compiler::FunctionRef::Chunk,
                9
            ),
            breakpoint_ids: vec![1]
        }
    );
    println!("Stopped: {:?}", stop);

    println!("Step out…");
    stop = debugger.step_out(session_id)?;
    assert_eq!(
        stop,
        StopReason::Breakpoint {
            location: Location::new(
                "test.lua".to_string(),
                piccolo::compiler::FunctionRef::Named("bar".to_string(), LineNumber(9)),
                10
            ),
            breakpoint_ids: vec![3]
        }
    );
    println!("Stopped: {:?}", stop);

    println!("Continue again…");
    stop = debugger.continue_run(session_id)?;
    assert_eq!(
        stop,
        StopReason::Watchpoint("watchpoint hit: global['x']: 0 -> 1".to_string())
    );
    println!("Stopped: {:?}", stop);

    if let Some(v0) = debugger.read_register(session_id, 0)? {
        println!("r0 = {}", v0);
    }

    Ok(())
}
