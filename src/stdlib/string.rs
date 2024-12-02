use std::num::TryFromIntError;

use crate::{Callback, CallbackReturn, Context, IntoValue, String, Table, Value};

mod pattern;

/// Convert a lua 1-indexed slice offset, which may be relative to the
/// string length, to a positive zero-indexed Rust index.  Note that the
/// index is *not* bounded to `len` when it is positive.
///
/// This can only fail on 32 bit platforms, where i64 values may not fit
/// into a usize.
fn convert_index(i: i64, len: i64) -> Result<usize, TryFromIntError> {
    match i {
        0 => 0,
        v @ 1.. => v - 1,
        v @ ..=-1 => (len + v).max(0),
    }
    .try_into()
}

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string
        .set(
            ctx,
            "len",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let len = string.len();
                stack.replace(ctx, len);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "sub",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                fn operate_sub(
                    string: &[u8],
                    i: i64,
                    j: Option<i64>,
                ) -> Result<&[u8], std::num::TryFromIntError> {
                    let len = string.len();
                    let i = convert_index(i, len as i64)?;
                    let j = convert_index(j.unwrap_or(len as i64).saturating_add(1), len as i64)?
                        .min(len);

                    if i > j {
                        Ok(&[])
                    } else {
                        Ok(&string[i..j])
                    }
                }

                let (string, i, j) = stack.consume::<(String, i64, Option<i64>)>(ctx)?;
                let substr = ctx.intern(operate_sub(string.as_bytes(), i, j)?);
                stack.replace(ctx, substr);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "lower",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let lowered = ctx.intern(
                    &string
                        .as_bytes()
                        .iter()
                        .map(u8::to_ascii_lowercase)
                        .collect::<Vec<_>>(),
                );
                stack.replace(ctx, lowered);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "reverse",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let reversed =
                    ctx.intern(&string.as_bytes().iter().copied().rev().collect::<Vec<_>>());
                stack.replace(ctx, reversed);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "upper",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let string = stack.consume::<String>(ctx)?;
                let uppered = ctx.intern(
                    &string
                        .as_bytes()
                        .iter()
                        .map(u8::to_ascii_uppercase)
                        .collect::<Vec<_>>(),
                );
                stack.replace(ctx, uppered);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "char",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let bytes = stack
                    .drain(..)
                    .map(|v| try_to_byte(v))
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(|e| e.into_value(ctx))?;
                stack.replace(ctx, ctx.intern(&bytes));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn try_to_byte<'gc>(v: Value<'gc>) -> Result<u8, &'static str> {
        match v.to_integer() {
            Some(n @ 0..=255) => Ok(n as u8),
            Some(_) => Err("value out of range"),
            None => Err("expected integer"),
        }
    }

    string
        .set(
            ctx,
            "rep",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let (string, count) = stack.consume::<(String, i64)>(ctx)?;
                let repeated = string.repeat(count as usize);
                stack.replace(ctx, ctx.intern(&repeated));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string);

    let mode = std::env::var("PAT_BACKEND");
    let mode = mode.as_deref().unwrap_or("async");
    match mode {
        "stack" => load_pattern::<pattern::StackBackend>(ctx),
        "seq" => load_pattern::<pattern::SeqBackend>(ctx),
        "async" | _ => load_pattern_async(ctx),
    }
}

pub fn load_pattern<'gc, F: pattern::FindBackend>(ctx: Context<'gc>) {
    let table: Option<Table> = ctx.get_global("string").unwrap();
    let string = match table {
        Some(t) => t,
        None => {
            let string = Table::new(&ctx);
            ctx.set_global("string", string);
            string
        }
    };

    string
        .set(
            ctx,
            "find",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern::lua::lua_find::<F>(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "match",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern::lua::lua_match::<F>(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "gmatch",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern::lua::lua_gmatch::<F>(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(ctx, "gsub", pattern::lua::lua_gsub_impl::<F>(ctx))
        .unwrap();
}

pub fn load_pattern_async<'gc>(ctx: Context<'gc>) {
    let table: Option<Table> = ctx.get_global("string").unwrap();
    let string = match table {
        Some(t) => t,
        None => {
            let string = Table::new(&ctx);
            ctx.set_global("string", string);
            string
        }
    };

    string
        .set(ctx, "find", pattern::lua::lua_find_async(ctx))
        .unwrap();

    string
        .set(ctx, "match", pattern::lua::lua_match_async(ctx))
        .unwrap();

    string
        .set(ctx, "gmatch", pattern::lua::lua_gmatch_async(ctx))
        .unwrap();

    string
        .set(ctx, "gsub", pattern::lua::lua_gsub_impl_async(ctx))
        .unwrap();
}
