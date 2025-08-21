use crate::{Context, Error, IntoValue, Value};

mod base;
mod coroutine;
mod io;
mod math;
mod string;
mod table;

pub use self::{
    base::load_base, coroutine::load_coroutine, io::load_io, math::load_math, string::load_string,
    table::load_table,
};

/// Build `bad argument #N to 'func' (message)` like [`luaL_argerror`](https://www.lua.org/manual/5.4/manual.html#luaL_argerror)
pub fn argument_error<'gc>(ctx: Context<'gc>, func: &str, arg: usize, msg: &str) -> Error<'gc> {
    format!("bad argument #{} to '{}' ({})", arg, func, msg)
        .into_value(ctx)
        .into()
}

/// Build `bad argument #N to 'func' (expected, got)` like [`luaL_typeerror`](https://www.lua.org/manual/5.4/manual.html#luaL_typeerror)
pub fn type_error<'gc>(
    ctx: Context<'gc>,
    func: &str,
    arg: usize,
    expected: &str,
    found: &str,
) -> Error<'gc> {
    argument_error(
        ctx,
        func,
        arg,
        &format!("{} expected, got {}", expected, found),
    )
}

/// Build the special integer-conversion message used by [`luaL_checkinteger`](https://www.lua.org/manual/5.4/manual.html#luaL_checkinteger)
pub fn integer_representation_error<'gc>(ctx: Context<'gc>, func: &str, arg: usize) -> Error<'gc> {
    argument_error(ctx, func, arg, "number has no integer representation")
}

/// Equivalent of [`luaL_optinteger`](https://www.lua.org/manual/5.4/manual.html#luaL_optinteger).
/// # Semantics
/// - `None` if `value` is `nil`
/// - `Ok(i64)` if `value` converts to `integer`
/// - `Err` with precise message otherwise
pub fn parse_opt_integer_arg<'gc>(
    ctx: Context<'gc>,
    func: &str,
    arg: usize,
    value: Option<Value<'gc>>,
) -> Result<Option<i64>, Error<'gc>> {
    let Some(v) = value else { return Ok(None) };
    if v.is_nil() {
        return Ok(None);
    }
    if let Some(i) = v.to_integer() {
        return Ok(Some(i));
    }
    if v.to_number().is_some() {
        return Err(integer_representation_error(ctx, func, arg));
    }
    Err(type_error(ctx, func, arg, "number", v.type_name()))
}

/// Equivalent of [`luaL_optlstring`](https://www.lua.org/manual/5.4/manual.html#luaL_optlstring) semantics used for separators in [`table.concat`](https://www.lua.org/manual/5.4/manual.html#pdf-table.concat).
/// # Semantics
/// - `None` if `value` is `nil`
/// - `Some(Value)` if `value` is a string or a number
/// - `Err` with precise message otherwise
pub fn parse_opt_stringlike_arg<'gc>(
    ctx: Context<'gc>,
    func: &str,
    arg: usize,
    value: Option<Value<'gc>>,
) -> Result<Option<Value<'gc>>, Error<'gc>> {
    let Some(v) = value else { return Ok(None) };
    if v.is_nil() {
        return Ok(None);
    }
    if v.into_string(ctx).is_some() {
        return Ok(Some(v));
    }
    Err(type_error(ctx, func, arg, "string", v.type_name()))
}
