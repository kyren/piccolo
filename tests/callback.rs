use luster::callback::Callback;
use luster::compiler::compile;
use luster::error::Error;
use luster::function::Closure;
use luster::lua::Lua;
use luster::sequence::{sequence_fn, ContinuationResult, SequenceExt};
use luster::value::Value;
use luster::{lua_callback, lua_sequence};

#[test]
fn callback() -> Result<(), Box<Error>> {
    let mut lua = Lua::new();
    lua_sequence!(
        lua,
        sequence_fn(|mc, lc| -> Result<(), Error> {
            let callback = lua_callback!(|args: &[_]| {
                let mut ret = args.to_vec();
                ret.push(Value::Integer(42));
                Ok(ContinuationResult::Finish(ret))
            });
            lc.globals.set(
                mc,
                Value::String(lc.interned_strings.new_string(mc, b"callback")),
                Value::Callback(Callback::new(mc, callback)),
            )?;
            Ok(())
        })
        .and_then(|mc, lc, _| Ok(Closure::new(
            mc,
            compile(
                mc,
                lc.interned_strings,
                &br#"
                    local a, b, c = callback(1, 2)
                    return a == 1 and b == 2 and c == 42
                "#[..]
            )?,
            Some(lc.globals),
        )?))
        .and_then(|mc, lc, closure| lc.main_thread.call_function(mc, closure, &[], 64))
        .map(|b| assert_eq!(b, vec![Value::Boolean(true)]))
    )?;

    Ok(())
}
