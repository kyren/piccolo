use gc_arena::Collect;

use crate::{
    AnyCallback, CallbackReturn, Context, Function, IntoValue, RuntimeError, TypeError, Value,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum MetaMethod {
    Len,
    Index,
    NewIndex,
    Call,
    Pairs,
    ToString,
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
        }
    }
}

impl<'gc> IntoValue<'gc> for MetaMethod {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.name().into_value(ctx)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct MetaCall<'gc, const N: usize> {
    pub function: Function<'gc>,
    pub args: [Value<'gc>; N],
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum MetaResult<'gc, const N: usize> {
    Value(Value<'gc>),
    Call(MetaCall<'gc, N>),
}

pub fn index<'gc>(
    ctx: Context<'gc>,
    table: Value<'gc>,
    key: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, TypeError> {
    let idx = match table {
        Value::Table(table) => {
            let v = table.get(ctx, key);
            if !v.is_nil() {
                return Ok(MetaResult::Value(v));
            }

            let idx = if let Some(mt) = table.metatable() {
                mt.get(ctx, MetaMethod::Index)
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
                mt.get(ctx, MetaMethod::Index)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                return Err(TypeError {
                    expected: "table",
                    found: table.type_name(),
                });
            }

            idx
        }
        _ => {
            return Err(TypeError {
                expected: "table",
                found: table.type_name(),
            })
        }
    };

    Ok(MetaResult::Call(match idx {
        Value::Table(table) => MetaCall {
            function: AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let table = stack.get(0);
                let key = stack.get(1);
                stack.clear();
                match index(ctx, table, key)? {
                    MetaResult::Value(v) => {
                        stack.push_back(v);
                        Ok(CallbackReturn::Return.into())
                    }
                    MetaResult::Call(call) => {
                        stack.extend(call.args);
                        Ok(CallbackReturn::Call {
                            function: call.function,
                            then: None,
                        }
                        .into())
                    }
                }
            })
            .into(),
            args: [table.into(), key],
        },
        _ => MetaCall {
            function: call(ctx, idx)?,
            args: [table, key],
        },
    }))
}

pub fn new_index<'gc>(
    ctx: Context<'gc>,
    table: Value<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
) -> Result<Option<MetaCall<'gc, 3>>, RuntimeError> {
    let idx = match table {
        Value::Table(table) => {
            let v = table.get(ctx, key);
            if !v.is_nil() {
                // If the value is present in the table, then we do not invoke the metamethod.
                table.set_value(&ctx, key, value)?;
                return Ok(None);
            }

            let idx = if let Some(mt) = table.metatable() {
                mt.get(ctx, MetaMethod::NewIndex)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                // If we do not have a __newindex metamethod, then just set the table value
                // directly.
                table.set_value(&ctx, key, value)?;
                return Ok(None);
            }

            idx
        }
        Value::UserData(u) if u.metatable().is_some() => {
            let idx = if let Some(mt) = u.metatable() {
                mt.get(ctx, MetaMethod::NewIndex)
            } else {
                Value::Nil
            };

            if idx.is_nil() {
                return Err(TypeError {
                    expected: "table",
                    found: table.type_name(),
                }
                .into());
            }

            idx
        }
        _ => {
            return Err(TypeError {
                expected: "table",
                found: table.type_name(),
            }
            .into())
        }
    };

    Ok(Some(match idx {
        Value::Table(table) => MetaCall {
            function: AnyCallback::from_fn(&ctx, |ctx, _, mut stack| {
                let (table, key, value): (Value, Value, Value) = stack.consume(ctx)?;
                if let Some(call) = new_index(ctx, table, key, value)? {
                    stack.extend(call.args);
                    Ok(CallbackReturn::Call {
                        function: call.function,
                        then: None,
                    }
                    .into())
                } else {
                    Ok(CallbackReturn::Return)
                }
            })
            .into(),
            args: [table.into(), key, value],
        },
        _ => MetaCall {
            function: call(ctx, idx)?,
            args: [table, key, value],
        },
    }))
}

pub fn call<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<Function<'gc>, TypeError> {
    let metatable = match v {
        Value::Function(f) => return Ok(f),
        Value::Table(t) => t.metatable(),
        Value::UserData(ud) => ud.metatable(),
        _ => None,
    }
    .ok_or(TypeError {
        expected: "function",
        found: v.type_name(),
    })?;

    match metatable.get(ctx, MetaMethod::Call) {
        f @ (Value::Function(_) | Value::Table(_) | Value::UserData(_)) => Ok(
            AnyCallback::from_fn_with(&ctx, (v, f), |&(v, f), ctx, _, mut stack| {
                stack.push_front(v);
                Ok(CallbackReturn::Call {
                    function: call(ctx, f)?,
                    then: None,
                }
                .into())
            })
            .into(),
        ),
        f => Err(TypeError {
            expected: "function",
            found: f.type_name(),
        }),
    }
}

pub fn len<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, TypeError> {
    if let Some(metatable) = match v {
        Value::Table(t) => t.metatable(),
        Value::UserData(u) => u.metatable(),
        _ => None,
    } {
        let len = metatable.get(ctx, MetaMethod::Len);
        if !len.is_nil() {
            return Ok(MetaResult::Call(MetaCall {
                function: call(ctx, len)?,
                args: [v],
            }));
        }
    }

    match v {
        Value::String(s) => Ok(MetaResult::Value(s.len().into())),
        Value::Table(t) => Ok(MetaResult::Value(t.length().into())),
        f => Err(TypeError {
            expected: "string or table",
            found: f.type_name(),
        }),
    }
}

pub fn tostring<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, TypeError> {
    if let Some(metatable) = match v {
        Value::Table(t) => t.metatable(),
        Value::UserData(u) => u.metatable(),
        _ => None,
    } {
        let tostring = metatable.get(ctx, MetaMethod::ToString);
        if !tostring.is_nil() {
            return Ok(MetaResult::Call(MetaCall {
                function: call(ctx, tostring)?,
                args: [v],
            }));
        }
    }

    Ok(match v {
        v @ Value::String(_) => MetaResult::Value(v),
        v => MetaResult::Value(
            ctx.state
                .strings
                .intern(&ctx, v.to_string().as_bytes())
                .into(),
        ),
    })
}
