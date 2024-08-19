use std::io::Write;

use gc_arena::Collect;
use thiserror::Error;

use crate::async_callback::{AsyncSequence, Locals};
use crate::{async_sequence, SequenceReturn, Stack};
use crate::{
    table::InvalidTableKey, Callback, CallbackReturn, Context, Function, IntoValue, Table, Value,
};

/// An enum of every possible Lua metamethod.
///
/// The [`MetaMethod::name`] method will return the name that Lua expects to be the key
/// for the metamethod in a metatable. For example, `MetaMethod::Add.name()` is `"__add"`,
/// `MetaMethod::Sub.name()` is `"__sub"`, etc.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum MetaMethod {
    Len,
    Index,
    NewIndex,
    Call,
    Pairs,
    ToString,
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Unm,
    IDiv,
    BAnd,
    BOr,
    BXor,
    BNot,
    Shl,
    Shr,
    Concat,
    Lt,
    Le,
}

impl MetaMethod {
    pub const fn name(self) -> &'static str {
        match self {
            MetaMethod::Len => "__len",
            MetaMethod::Index => "__index",
            MetaMethod::NewIndex => "__newindex",
            MetaMethod::Call => "__call",
            MetaMethod::Pairs => "__pairs",
            MetaMethod::ToString => "__tostring",
            MetaMethod::Eq => "__eq",
            MetaMethod::Add => "__add",
            MetaMethod::Sub => "__sub",
            MetaMethod::Mul => "__mul",
            MetaMethod::Div => "__div",
            MetaMethod::Mod => "__mod",
            MetaMethod::Pow => "__pow",
            MetaMethod::Unm => "__unm",
            MetaMethod::IDiv => "__idiv",
            MetaMethod::BAnd => "__band",
            MetaMethod::BOr => "__bor",
            MetaMethod::BXor => "__bxor",
            MetaMethod::BNot => "__bnot",
            MetaMethod::Shl => "__shl",
            MetaMethod::Shr => "__shr",
            MetaMethod::Concat => "__concat",
            MetaMethod::Lt => "__lt",
            MetaMethod::Le => "__le",
        }
    }

    /// Sentence-form verb of this metamethod's action
    ///
    /// - unary: "Could not {verb} a {type} value"
    /// - index: "Could not {verb} a {type} value"
    /// - binary: "Could not {verb} values of type {lhs_type} and {rhs_type}"
    pub const fn verb(self) -> &'static str {
        match self {
            MetaMethod::Len => "determine length of",
            MetaMethod::Call => "call",
            MetaMethod::Pairs => "get pairs of",
            MetaMethod::ToString => "convert to string", // a bit awkward, but works
            MetaMethod::Index => "index into",
            MetaMethod::NewIndex => "index-assign into",
            MetaMethod::Eq => "compare equality of",
            MetaMethod::Add => "add",
            MetaMethod::Sub => "subtract",
            MetaMethod::Mul => "multiply",
            MetaMethod::Div => "divide",
            MetaMethod::Mod => "take modulus of",
            MetaMethod::Pow => "exponentiate",
            MetaMethod::Unm => "negate",
            MetaMethod::IDiv => "flooring divide",
            MetaMethod::BAnd => "binary and",
            MetaMethod::BOr => "binary or",
            MetaMethod::BXor => "binary xor",
            MetaMethod::BNot => "binary negate",
            MetaMethod::Shl => "left shift",
            MetaMethod::Shr => "right shift",
            MetaMethod::Concat => "concatenate",
            MetaMethod::Lt => "compare",
            MetaMethod::Le => "compare",
        }
    }
}

impl<'gc> IntoValue<'gc> for MetaMethod {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.name().into_value(ctx)
    }
}

/// If invoking a metamethod must call Lua code, this will contain a function and arguments to call
/// to trigger it.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct MetaCall<'gc, const N: usize> {
    pub function: Function<'gc>,
    pub args: [Value<'gc>; N],
}

/// Return value for metamethods that return a value *or* require calling into Lua.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum MetaResult<'gc, const N: usize> {
    Value(Value<'gc>),
    Call(MetaCall<'gc, N>),
}

impl<'gc, const N: usize> From<Value<'gc>> for MetaResult<'gc, N> {
    fn from(value: Value<'gc>) -> Self {
        Self::Value(value)
    }
}

impl<'gc, const N: usize> From<MetaCall<'gc, N>> for MetaResult<'gc, N> {
    fn from(call: MetaCall<'gc, N>) -> Self {
        MetaResult::Call(call)
    }
}

#[derive(Debug, Clone, Error)]
pub enum MetaOperatorError {
    #[error("could not call metamethod {}: {}", .0.name(), .1)]
    Call(MetaMethod, #[source] MetaCallError),
    #[error("could not {} a {} value", .0.verb(), .1)]
    Unary(MetaMethod, &'static str),
    #[error("could not {} values of type {} and {}", .0.verb(), .1, .2)]
    Binary(MetaMethod, &'static str, &'static str),
    #[error("invalid table key")]
    IndexKeyError(#[from] InvalidTableKey),
    #[error("concatenation result is too long")]
    ConcatOverflow,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("could not call a {} value", .0)]
pub struct MetaCallError(&'static str);

fn get_metatable<'gc>(val: Value<'gc>) -> Option<Table<'gc>> {
    match val {
        Value::Table(t) => t.metatable(),
        Value::UserData(u) => u.metatable(),
        _ => None,
    }
}

fn get_metamethod<'gc>(
    ctx: Context<'gc>,
    val: Value<'gc>,
    method: MetaMethod,
) -> Option<Value<'gc>> {
    get_metatable(val)
        .map(|mt| mt.get_value(ctx, method))
        .filter(|v| !v.is_nil())
}

pub fn index<'gc>(
    ctx: Context<'gc>,
    table: Value<'gc>,
    key: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    let idx = match table {
        Value::Table(table) => {
            let v = table.get_value(ctx, key);
            if !v.is_nil() {
                return Ok(MetaResult::Value(v));
            }

            let idx = if let Some(mt) = table.metatable() {
                mt.get_value(ctx, MetaMethod::Index)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                return Ok(MetaResult::Value(Value::Nil));
            }

            idx
        }
        Value::UserData(u) if u.metatable().is_some() => {
            let idx = if let Some(mt) = u.metatable() {
                mt.get_value(ctx, MetaMethod::Index)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                return Err(MetaOperatorError::Unary(
                    MetaMethod::Index,
                    table.type_name(),
                ));
            }

            idx
        }
        _ => {
            return Err(MetaOperatorError::Unary(
                MetaMethod::Index,
                table.type_name(),
            ))
        }
    };

    // NOTE: The __index metamethod (and others) can easily infinite loop or enter arbitrarily long
    // chains:
    //
    // `t = {}; setmetatable(t, { __index = t }); t.a`
    //
    // PUC-Rio Lua guards the maximum length of metamethod chains to `MAXTAGLOOP` in cases where no
    // Lua code is invoked. It must do this, because otherwise Lua code could cause the interpreter
    // to infinite loop without triggering hook functions. We don't HAVE to mimic this behavior here
    // due to piccolo's flexibility: the `Executor` design allows us to ensure that control is still
    // periodically returned by performing the access through a separate callback.
    //
    // We could introduce a maximum chain depth, or try to detect infinite chains in simple cases,
    // or just follow chains of metamethods in blocks to reduce the number of separate callback
    // calls. Right now, it works in the absolute *simplest* possible way.
    //
    // We could also make it a little nicer to deal with arbitrary long metamethod chains by
    // replacing the `MetaCall` machinery with a `Sequence` and allowing `Sequence` impls to
    // participate in custom backtrace printing. If done generically, every metamethod chain call
    // could print its current chain depth as part of the backtrace, helping to debug infinite
    // loops due to metamethod chains. Changing `MetaCall` to use sequences also has a potential
    // performance benefit because a `BoxSequence` can avoid allocation when the sequence is a ZST.
    Ok(MetaResult::Call(match idx {
        table @ (Value::Table(_) | Value::UserData(_)) => MetaCall {
            function: Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let table = stack.get(0);
                let key = stack.get(1);
                stack.clear();

                match index(ctx, table, key)? {
                    MetaResult::Value(v) => {
                        stack.push_back(v);
                        Ok(CallbackReturn::Return)
                    }
                    MetaResult::Call(call) => {
                        stack.extend(call.args);
                        Ok(CallbackReturn::Call {
                            function: call.function,
                            then: None,
                        })
                    }
                }
            })
            .into(),
            args: [table, key],
        },
        _ => MetaCall {
            function: call(ctx, idx).map_err(|e| MetaOperatorError::Call(MetaMethod::Index, e))?,
            args: [table, key],
        },
    }))
}

pub fn new_index<'gc>(
    ctx: Context<'gc>,
    table: Value<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
) -> Result<Option<MetaCall<'gc, 3>>, MetaOperatorError> {
    let idx = match table {
        Value::Table(table) => {
            let v = table.get_value(ctx, key);
            if !v.is_nil() {
                // If the value is present in the table, then we do not invoke the metamethod.
                table.set_raw(&ctx, key, value)?;
                return Ok(None);
            }

            let idx = if let Some(mt) = table.metatable() {
                mt.get_value(ctx, MetaMethod::NewIndex)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                // If we do not have a __newindex metamethod, then just set the table value
                // directly.
                table.set_raw(&ctx, key, value)?;
                return Ok(None);
            }

            idx
        }
        Value::UserData(u) if u.metatable().is_some() => {
            let idx = if let Some(mt) = u.metatable() {
                mt.get_value(ctx, MetaMethod::NewIndex)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                return Err(
                    MetaOperatorError::Unary(MetaMethod::NewIndex, table.type_name()).into(),
                );
            }

            idx
        }
        _ => {
            return Err(MetaOperatorError::Unary(MetaMethod::NewIndex, table.type_name()).into());
        }
    };

    Ok(Some(match idx {
        table @ (Value::Table(_) | Value::UserData(_)) => MetaCall {
            function: Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // NOTE: Potential for indexing loop here, see note in __index.
                let (table, key, value): (Value, Value, Value) = stack.consume(ctx)?;
                if let Some(call) = new_index(ctx, table, key, value)? {
                    stack.extend(call.args);
                    Ok(CallbackReturn::Call {
                        function: call.function,
                        then: None,
                    })
                } else {
                    Ok(CallbackReturn::Return)
                }
            })
            .into(),
            args: [table, key, value],
        },
        _ => MetaCall {
            function: call(ctx, idx)
                .map_err(|e| MetaOperatorError::Call(MetaMethod::NewIndex, e))?,
            args: [table, key, value],
        },
    }))
}

pub fn call<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<Function<'gc>, MetaCallError> {
    let metatable = match v {
        Value::Function(f) => return Ok(f),
        Value::Table(t) => t.metatable(),
        Value::UserData(ud) => ud.metatable(),
        _ => None,
    }
    .ok_or(MetaCallError(v.type_name()))?;

    match metatable.get_value(ctx, MetaMethod::Call) {
        f @ (Value::Function(_) | Value::Table(_) | Value::UserData(_)) => Ok(
            // NOTE: Potential for infinite or arbitrarily long chains here, see note in __index.
            //
            // Example: `t = {}; setmetatable(t, { __call = t }); t()`
            Callback::from_fn_with(&ctx, (v, f), |&(v, f), ctx, _, mut stack| {
                stack.push_front(v);
                Ok(CallbackReturn::Call {
                    function: call(ctx, f)?,
                    then: None,
                })
            })
            .into(),
        ),
        f => Err(MetaCallError(f.type_name())),
    }
}

pub fn len<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, MetaOperatorError> {
    if let Some(metatable) = match v {
        Value::Table(t) => t.metatable(),
        Value::UserData(u) => u.metatable(),
        _ => None,
    } {
        let len = metatable.get_value(ctx, MetaMethod::Len);
        if !len.is_nil() {
            return Ok(MetaResult::Call(MetaCall {
                function: call(ctx, len)
                    .map_err(|e| MetaOperatorError::Call(MetaMethod::Len, e))?,
                args: [v],
            }));
        }
    }

    match v {
        Value::String(s) => Ok(MetaResult::Value(s.len().into())),
        Value::Table(t) => Ok(MetaResult::Value(t.length().into())),
        f => Err(MetaOperatorError::Unary(MetaMethod::Len, f.type_name())),
    }
}

pub fn tostring<'gc>(
    ctx: Context<'gc>,
    v: Value<'gc>,
) -> Result<MetaResult<'gc, 1>, MetaOperatorError> {
    if let Some(metatable) = match v {
        Value::Table(t) => t.metatable(),
        Value::UserData(u) => u.metatable(),
        _ => None,
    } {
        let tostring = metatable.get_value(ctx, MetaMethod::ToString);
        if !tostring.is_nil() {
            return Ok(MetaResult::Call(MetaCall {
                function: call(ctx, tostring)
                    .map_err(|e| MetaOperatorError::Call(MetaMethod::ToString, e))?,
                args: [v],
            }));
        }
    }

    Ok(match v {
        v @ Value::String(_) => MetaResult::Value(v),
        v => MetaResult::Value(ctx.intern(v.display().to_string().as_bytes()).into()),
    })
}

pub fn equal<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    Ok(match (lhs, rhs) {
        (Value::Nil, Value::Nil) => Value::Boolean(true).into(),
        (Value::Nil, _) => Value::Boolean(false).into(),

        (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b).into(),
        (Value::Boolean(_), _) => Value::Boolean(false).into(),

        (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a == b).into(),
        (Value::Integer(a), Value::Number(b)) => Value::Boolean(a as f64 == b).into(),
        (Value::Integer(_), _) => Value::Boolean(false).into(),

        (Value::Number(a), Value::Number(b)) => Value::Boolean(a == b).into(),
        (Value::Number(a), Value::Integer(b)) => Value::Boolean(b as f64 == a).into(),
        (Value::Number(_), _) => Value::Boolean(false).into(),

        (Value::String(a), Value::String(b)) => Value::Boolean(a == b).into(),
        (Value::String(_), _) => Value::Boolean(false).into(),

        (Value::Function(a), Value::Function(b)) => Value::Boolean(a == b).into(),
        (Value::Function(_), _) => Value::Boolean(false).into(),

        (Value::Thread(a), Value::Thread(b)) => Value::Boolean(a == b).into(),
        (Value::Thread(_), _) => Value::Boolean(false).into(),

        (Value::Table(a), Value::Table(b)) if a == b => Value::Boolean(true).into(),
        (Value::Table(_), Value::Table(_)) => {
            if let Some(m) = get_metamethod(ctx, lhs, MetaMethod::Eq) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m)
                        .map_err(|e| MetaOperatorError::Call(MetaMethod::Eq, e))?,
                    args: [lhs, rhs],
                })
            } else if let Some(m) = get_metamethod(ctx, rhs, MetaMethod::Eq) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m)
                        .map_err(|e| MetaOperatorError::Call(MetaMethod::Eq, e))?,
                    args: [lhs, rhs],
                })
            } else {
                Value::Boolean(false).into()
            }
        }
        (Value::Table(_), _) => Value::Boolean(false).into(),

        (Value::UserData(a), Value::UserData(b)) if a == b => Value::Boolean(true).into(),
        (Value::UserData(_), Value::UserData(_)) => {
            if let Some(m) = get_metamethod(ctx, lhs, MetaMethod::Eq) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m)
                        .map_err(|e| MetaOperatorError::Call(MetaMethod::Eq, e))?,
                    args: [lhs, rhs],
                })
            } else if let Some(m) = get_metamethod(ctx, rhs, MetaMethod::Eq) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m)
                        .map_err(|e| MetaOperatorError::Call(MetaMethod::Eq, e))?,
                    args: [lhs, rhs],
                })
            } else {
                Value::Boolean(false).into()
            }
        }
        (Value::UserData(_), _) => Value::Boolean(false).into(),
    })
}

fn meta_metaop<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
    method: MetaMethod,
    const_op: impl Fn(Context<'gc>, Value<'gc>, Value<'gc>) -> Option<Value<'gc>>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    Ok(match (lhs, rhs) {
        (Value::Table(_) | Value::UserData(_), Value::Table(_) | Value::UserData(_)) => {
            if let Some(m) = get_metamethod(ctx, lhs, method) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m).map_err(|e| MetaOperatorError::Call(method, e))?,
                    args: [lhs, rhs],
                })
            } else if let Some(m) = get_metamethod(ctx, rhs, method) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m).map_err(|e| MetaOperatorError::Call(method, e))?,
                    args: [lhs, rhs],
                })
            } else {
                return Err(MetaOperatorError::Binary(
                    method,
                    lhs.type_name(),
                    rhs.type_name(),
                ));
            }
        }
        (Value::Table(_) | Value::UserData(_), _) => {
            if let Some(m) = get_metamethod(ctx, lhs, method) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m).map_err(|e| MetaOperatorError::Call(method, e))?,
                    args: [lhs, rhs],
                })
            } else {
                return Err(MetaOperatorError::Binary(
                    method,
                    lhs.type_name(),
                    rhs.type_name(),
                ));
            }
        }
        (_, Value::Table(_) | Value::UserData(_)) => {
            if let Some(m) = get_metamethod(ctx, rhs, method) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m).map_err(|e| MetaOperatorError::Call(method, e))?,
                    args: [lhs, rhs],
                })
            } else {
                return Err(MetaOperatorError::Binary(
                    method,
                    lhs.type_name(),
                    rhs.type_name(),
                ));
            }
        }
        (a, b) => const_op(ctx, a, b)
            .ok_or_else(|| MetaOperatorError::Binary(method, lhs.type_name(), rhs.type_name()))?
            .into(),
    })
}

fn meta_unary_metaop<'gc>(
    ctx: Context<'gc>,
    arg: Value<'gc>,
    method: MetaMethod,
    const_op: impl Fn(Value<'gc>) -> Option<Value<'gc>>,
) -> Result<MetaResult<'gc, 1>, MetaOperatorError> {
    Ok(match arg {
        Value::Table(_) | Value::UserData(_) => {
            if let Some(m) = get_metamethod(ctx, arg, method) {
                MetaResult::Call(MetaCall {
                    function: call(ctx, m).map_err(|e| MetaOperatorError::Call(method, e))?,
                    args: [arg],
                })
            } else {
                return Err(MetaOperatorError::Unary(method, arg.type_name()));
            }
        }
        val => const_op(val)
            .ok_or_else(|| MetaOperatorError::Unary(method, arg.type_name()))?
            .into(),
    })
}

pub fn add<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Add, |_, a, b| {
        Some(a.to_constant()?.add(&b.to_constant()?)?.into())
    })
}

pub fn subtract<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Sub, |_, a, b| {
        Some(a.to_constant()?.subtract(&b.to_constant()?)?.into())
    })
}

pub fn multiply<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Mul, |_, a, b| {
        Some(a.to_constant()?.multiply(&b.to_constant()?)?.into())
    })
}

pub fn float_divide<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Div, |_, a, b| {
        Some(a.to_constant()?.float_divide(&b.to_constant()?)?.into())
    })
}

pub fn floor_divide<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::IDiv, |_, a, b| {
        Some(a.to_constant()?.floor_divide(&b.to_constant()?)?.into())
    })
}

pub fn modulo<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Mod, |_, a, b| {
        Some(a.to_constant()?.modulo(&b.to_constant()?)?.into())
    })
}

pub fn exponentiate<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Pow, |_, a, b| {
        Some(a.to_constant()?.exponentiate(&b.to_constant()?)?.into())
    })
}

pub fn negate<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
) -> Result<MetaResult<'gc, 1>, MetaOperatorError> {
    meta_unary_metaop(ctx, lhs, MetaMethod::Unm, |val| {
        Some(val.to_constant()?.negate()?.into())
    })
}

pub fn bitwise_not<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
) -> Result<MetaResult<'gc, 1>, MetaOperatorError> {
    meta_unary_metaop(ctx, lhs, MetaMethod::BNot, |val| {
        Some(val.to_constant()?.bitwise_not()?.into())
    })
}

pub fn bitwise_and<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::BAnd, |_, a, b| {
        Some(a.to_constant()?.bitwise_and(&b.to_constant()?)?.into())
    })
}

pub fn bitwise_or<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::BOr, |_, a, b| {
        Some(a.to_constant()?.bitwise_or(&b.to_constant()?)?.into())
    })
}

pub fn bitwise_xor<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::BXor, |_, a, b| {
        Some(a.to_constant()?.bitwise_xor(&b.to_constant()?)?.into())
    })
}

pub fn shift_left<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Shl, |_, a, b| {
        Some(a.to_constant()?.shift_left(&b.to_constant()?)?.into())
    })
}

pub fn shift_right<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Shr, |_, a, b| {
        Some(a.to_constant()?.shift_right(&b.to_constant()?)?.into())
    })
}

pub fn less_than<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Lt, |_, a, b| {
        Some(a.to_constant()?.less_than(&b.to_constant()?)?.into())
    })
}

pub fn less_equal<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Le, |_, a, b| {
        Some(a.to_constant()?.less_equal(&b.to_constant()?)?.into())
    })
}

/// The result of a concat metaoperation, either a completed [`Value`]
/// or a [`Function`] that must be called with the values to
/// concatenate.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum ConcatMetaResult<'gc> {
    Value(Value<'gc>),
    Call(Function<'gc>),
}

pub fn concat<'gc>(
    ctx: Context<'gc>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, MetaOperatorError> {
    meta_metaop(ctx, lhs, rhs, MetaMethod::Concat, |ctx, a, b| {
        if a.is_implicit_string() && b.is_implicit_string() {
            let mut bytes = Vec::new();
            for value in [a, b] {
                match value {
                    Value::Integer(i) => write!(&mut bytes, "{}", i).unwrap(),
                    Value::Number(n) => write!(&mut bytes, "{}", n).unwrap(),
                    Value::String(s) => bytes.extend(s.as_bytes()),
                    _ => return None,
                }
            }
            Some(Value::String(ctx.intern(&bytes)))
        } else {
            None
        }
    })
}

/// Returns an estimate of the length of the concatenation of a list of values,
/// or returns [`None`] if any value is not implicitly coercible to a string.
fn estimate_concatenated_len<'gc>(
    values: &[Value<'gc>],
) -> Result<Option<usize>, MetaOperatorError> {
    let mut len = 0usize;
    for value in values {
        let value_len = match value {
            Value::Integer(i) => i.abs().max(1).ilog10() as usize + i.is_negative() as usize,
            Value::Number(_n) => 10,
            Value::String(s) => s.as_bytes().len(),
            _ => return Ok(None),
        };
        len = len
            .checked_add(value_len)
            .ok_or(MetaOperatorError::ConcatOverflow)?;
    }
    Ok(Some(len))
}

pub fn concat_many<'gc>(
    ctx: Context<'gc>,
    values: &[Value<'gc>],
) -> Result<ConcatMetaResult<'gc>, MetaOperatorError> {
    // Fast path scope; never loops, returns if successful, otherwise
    // breaks to fall back to the slow impl.
    loop {
        // Since we have to make two passes to check for complex types,
        // estimate the length in the first pass.
        let Some(len) = estimate_concatenated_len(values)? else {
            break;
        };

        let mut bytes = Vec::with_capacity(len);
        for value in values {
            match value {
                Value::Integer(i) => write!(&mut bytes, "{}", i).unwrap(),
                Value::Number(n) => write!(&mut bytes, "{}", n).unwrap(),
                Value::String(s) => bytes.extend(s.as_bytes()),
                _ => unreachable!(),
            }
        }
        return Ok(ConcatMetaResult::Value(Value::String(ctx.intern(&bytes))));
    }

    // Fall back to a sequence-based implemenation to handle metamethods
    let func = Callback::from_fn(&ctx, |ctx, _, stack| {
        let args = stack.len();
        let s = async_sequence(&ctx, |_, mut seq| async move {
            for i in (1..args).into_iter().rev() {
                let call = seq.try_enter(|ctx, locals, _, mut stack| {
                    let bottom = i - 1;
                    let call = concat(ctx, stack[i - 1], stack[i])?;
                    let p = prepare_async_metaop(ctx, &mut stack, locals, bottom, call, 1);
                    Ok(p)
                })?;
                call.execute(&mut seq).await?;
            }
            Ok(SequenceReturn::Return)
        });
        Ok(CallbackReturn::Sequence(s))
    });
    Ok(ConcatMetaResult::Call(func.into()))
}

pub fn concat_separated<'gc>(
    ctx: Context<'gc>,
    values: &[Value<'gc>],
    separator: Value<'gc>,
) -> Result<ConcatMetaResult<'gc>, MetaOperatorError> {
    if separator.is_nil() {
        return concat_many(ctx, values);
    }

    // Fast path scope; never loops, returns if successful, otherwise
    // breaks to fall back to the slow impl.
    loop {
        let sep_str = match separator.into_string(ctx) {
            Some(s) => s,
            None => break,
        };

        // Since we have to make two passes to check for complex types,
        // estimate the length in the first pass.
        let Some(len) = estimate_concatenated_len(values)? else {
            break;
        };

        let sep_count = values.len().saturating_sub(1);
        let total_len = sep_count
            .checked_mul(sep_str.len() as usize)
            .and_then(|l| l.checked_add(len))
            .ok_or(MetaOperatorError::ConcatOverflow)?;

        // Should this be allocated in-place in the GC heap?
        let mut bytes = Vec::with_capacity(total_len);

        let mut iter = values.iter();
        if let Some(val) = iter.next() {
            match val {
                Value::Integer(i) => write!(&mut bytes, "{}", i).unwrap(),
                Value::Number(n) => write!(&mut bytes, "{}", n).unwrap(),
                Value::String(s) => bytes.extend(s.as_bytes()),
                _ => unreachable!(),
            }

            while let Some(val) = iter.next() {
                bytes.extend(&*sep_str);
                match val {
                    Value::Integer(i) => write!(&mut bytes, "{}", i).unwrap(),
                    Value::Number(n) => write!(&mut bytes, "{}", n).unwrap(),
                    Value::String(s) => bytes.extend(s.as_bytes()),
                    _ => unreachable!(),
                }
            }
        }
        drop(iter);

        return Ok(ConcatMetaResult::Value(Value::String(ctx.intern(&bytes))));
    }

    // Fall back to a sequence-based implemenation to handle metamethods
    let func = Callback::from_fn_with(&ctx, separator, move |&sep, ctx, _, stack| {
        let args = stack.len();
        let b = async_sequence(&ctx, |locals, mut seq| {
            let sep = locals.stash(&ctx, sep);
            async move {
                for i in (1..args).into_iter().rev() {
                    let call = seq.try_enter(|ctx, locals, _, mut stack| {
                        let bottom = i;
                        let call = concat(ctx, locals.fetch(&sep), stack[i])?;
                        let p = prepare_async_metaop(ctx, &mut stack, locals, bottom, call, 1);
                        Ok(p)
                    })?;
                    call.execute(&mut seq).await?;

                    let call = seq.try_enter(|ctx, locals, _, mut stack| {
                        let bottom = i - 1;
                        let call = concat(ctx, stack[i - 1], stack[i])?;
                        let p = prepare_async_metaop(ctx, &mut stack, locals, bottom, call, 1);
                        Ok(p)
                    })?;
                    call.execute(&mut seq).await?;
                }
                Ok(SequenceReturn::Return)
            }
        });
        Ok(CallbackReturn::Sequence(b))
    });
    Ok(ConcatMetaResult::Call(func.into()))
}

#[must_use]
struct PreparedCall {
    func: Option<crate::StashedFunction>,
    bottom: usize,
    returns: usize,
}

impl PreparedCall {
    async fn execute(self, seq: &mut AsyncSequence) -> Result<(), crate::StashedError> {
        if let Some(func) = self.func {
            seq.call(&func, self.bottom).await?;
        }
        seq.enter(|_, _, _, mut stack| {
            stack.resize(self.bottom + self.returns);
        });
        Ok(())
    }
}

fn prepare_async_metaop<'gc, const N: usize>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
    locals: Locals<'gc, '_>,
    bottom: usize,
    call: MetaResult<'gc, N>,
    returns: usize,
) -> PreparedCall {
    match call {
        MetaResult::Value(v) => {
            stack.resize(bottom);
            stack.push_back(v);
            PreparedCall {
                func: None,
                bottom,
                returns,
            }
        }
        MetaResult::Call(MetaCall { function, args }) => {
            stack.resize(bottom);
            stack.extend(args);
            PreparedCall {
                func: Some(locals.stash(&ctx, function)),
                bottom,
                returns,
            }
        }
    }
}
