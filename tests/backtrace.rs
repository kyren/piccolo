use piccolo::{error::BacktraceFrame, Closure, Executor, ExternError, Lua};
use std::collections::HashMap;

#[test]
fn test_backtrace() {
    const SCRIPT: &str = include_str!("test_backtrace.lua");

    let mut lua = Lua::full();

    let executor = lua
        .try_enter(|ctx| {
            let closure = Closure::load(ctx, Some("test_backtrace.lua"), SCRIPT.as_bytes())?;
            Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
        })
        .unwrap();

    lua.finish(&executor).unwrap();
    let result = lua.try_enter(|ctx| ctx.fetch(&executor).take_result::<()>(ctx)?);

    let mut source_map = std::collections::HashMap::new();
    source_map.insert("test_backtrace.lua".to_string(), SCRIPT.to_string());

    let Err(
        ref err @ ExternError::Lua {
            ref error,
            ref backtrace,
            ..
        },
    ) = result
    else {
        panic!("expected an error, got: {:?}", result);
    };

    assert!(error.to_string().contains("test error from rust"));

    let backtrace = backtrace.as_ref().unwrap();
    let mut backtrace_string = String::new();
    eprintln!("Error: {err:#}");
    err.pretty_print(&mut backtrace_string, Some(&source_map))
        .unwrap();
    eprintln!("Backtrace: {backtrace_string}");
    assert_eq!(backtrace.len(), 5);

    if let BacktraceFrame::Lua {
        chunk_name,
        function_name,
        line_number,
        args,
    } = &backtrace[0]
    {
        assert_eq!(chunk_name, "test_backtrace.lua");
        assert!(function_name.starts_with("<function 'error_callback'"));
        assert_eq!(line_number.0, 3);
        assert_eq!(
            args,
            &vec![("err_msg".to_string(), "test error from rust".to_string())]
        );
    } else {
        panic!("Expected first frame to be Lua frame");
    }

    if let BacktraceFrame::Lua {
        chunk_name,
        function_name,
        line_number,
        args,
    } = &backtrace[1]
    {
        assert_eq!(chunk_name, "test_backtrace.lua");
        assert!(function_name.starts_with("<function 'baz' at line 7>"));
        assert_eq!(line_number.0, 8);
        assert_eq!(
            args,
            &vec![
                ("a".to_string(), "test".to_string()),
                ("b".to_string(), " error from rust".to_string())
            ]
        );
    } else {
        panic!("Expected second frame to be Lua frame");
    }

    if let BacktraceFrame::Lua {
        chunk_name,
        function_name,
        line_number,
        args,
    } = &backtrace[2]
    {
        assert_eq!(chunk_name, "test_backtrace.lua");
        assert!(function_name.starts_with("<function 'bar' at line 12>"));
        assert_eq!(line_number.0, 12);
        assert_eq!(
            args,
            &vec![
                ("x".to_string(), "test".to_string()),
                ("...".to_string(), " error from rust".to_string())
            ]
        );
    } else {
        panic!("Expected third frame to be Lua frame");
    }

    if let BacktraceFrame::Lua {
        chunk_name,
        function_name,
        line_number,
        args,
    } = &backtrace[3]
    {
        assert_eq!(chunk_name, "test_backtrace.lua");
        assert!(function_name.starts_with("<function 'foo' at line 16>"));
        assert_eq!(line_number.0, 18);
        assert_eq!(args, &vec![("arg1".to_string(), "test".to_string())]);
    } else {
        panic!("Expected fourth frame to be Lua frame");
    }

    if let BacktraceFrame::Lua {
        chunk_name,
        function_name,
        line_number,
        args,
    } = &backtrace[4]
    {
        assert_eq!(chunk_name, "test_backtrace.lua");
        assert_eq!(function_name, "<chunk>");
        assert_eq!(line_number.0, 21);
        assert!(args.is_empty());
    } else {
        panic!("Expected fifth frame to be Chunk frame");
    }

    let mut output = String::new();
    err.pretty_print(&mut output, Some(&source_map)).unwrap();

    // Check we lookup the correct source map entries without being too pedantic about the format
    assert!(output.contains("-- This is line 4"));
    assert!(output.contains("-- This is line 9"));
    assert!(output.contains("-- This is line 13"));
    assert!(output.contains("-- This is line 19"));
    assert!(output.contains("-- This is line 22"));
}

#[test]
fn test_pretty_print_backtrace() -> Result<(), ExternError> {
    let mut lua = Lua::core();
    let source = br#"
function f() g() end
function g() h() end
function h() error("an error") end
f()
"#;

    let executor = lua.try_enter(|ctx| {
        let closure = Closure::load(ctx, Some("test.lua"), source)?;
        Ok(ctx.stash(Executor::start(ctx, closure.into(), ())))
    })?;

    lua.finish(&executor).unwrap();
    let err = lua
        .try_enter(|ctx| ctx.fetch(&executor).take_result::<()>(ctx)?)
        .unwrap_err();

    let mut out = String::new();
    let mut source_map = HashMap::new();
    source_map.insert(
        "test.lua".to_string(),
        String::from_utf8_lossy(source).into_owned(),
    );
    err.pretty_print(&mut out, Some(&source_map)).unwrap();
    eprintln!("{}", out);
    let expected_lines = [
        "an error",
        "stack traceback:",
        "test.lua:4 in <function 'h' at line 4>: `function h() error(\"an error\") end`",
        "test.lua:3 in <function 'g' at line 3>: `function g() h() end`",
        "test.lua:2 in <function 'f' at line 2>: `function f() g() end`",
        "test.lua:5 in <chunk>: `f()`",
    ];

    for line in expected_lines {
        assert!(out.contains(line), "output missing line: '{}'", line);
    }

    Ok(())
}
