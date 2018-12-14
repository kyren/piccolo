use luster::compiler::compile_chunk;
use luster::function::Closure;
use luster::lua::Lua;
use luster::parser::parse_chunk;
use luster::sequence::{sequence_fn, SequenceExt};
use luster::value::Value;

pub fn run_script<A, F>(script: A, f: F)
where
    A: AsRef<[u8]>,
    F: 'static + for<'gc> FnOnce(&[Value<'gc>]),
{
    let chunk = parse_chunk(script.as_ref()).expect("could not parse script");
    let mut lua = Lua::new();
    lua.sequence(move |_, lc| {
        Box::new(
            sequence_fn(move |mc| Closure::new(mc, compile_chunk(mc, &chunk)?))
                .and_then_with(lc.main_thread, move |mc, main_thread, closure| {
                    Ok(main_thread.call_function(mc, closure, &[], 64))
                })
                .map(|_, r| {
                    f(&r[..]);
                    Ok(())
                }),
        )
    })
    .expect("error in script");
}
