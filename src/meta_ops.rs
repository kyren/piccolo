use gc_arena::{Collect, MutationContext};

use crate::{AnyCallback, CallbackReturn, Function, IntoValue, TypeError, Value};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum MetaMethod {
    Index,
    Call,
    Pairs,
}

impl MetaMethod {
    pub const fn name(self) -> &'static str {
        match self {
            MetaMethod::Index => "__index",
            MetaMethod::Call => "__call",
            MetaMethod::Pairs => "__pairs",
        }
    }
}

impl<'gc> IntoValue<'gc> for MetaMethod {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        self.name().into_value(mc)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum MetaResult<'gc, const N: usize> {
    Value(Value<'gc>),
    Call(Function<'gc>, [Value<'gc>; N]),
}

pub fn index<'gc>(
    mc: MutationContext<'gc, '_>,
    table: Value<'gc>,
    key: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, TypeError> {
    let idx = match table {
        Value::Table(table) => {
            let v = table.get(mc, key);
            if !v.is_nil() {
                return Ok(MetaResult::Value(v));
            }

            let idx = if let Some(mt) = table.metatable() {
                mt.get(mc, MetaMethod::Index)
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
                mt.get(mc, MetaMethod::Index)
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

    match idx {
        Value::Table(table) => Ok(MetaResult::Call(
            AnyCallback::from_fn(mc, |mc, stack| {
                let table = stack.get(0).copied().unwrap_or_default();
                let key = stack.get(1).copied().unwrap_or_default();
                stack.clear();
                match index(mc, table, key)? {
                    MetaResult::Value(v) => {
                        stack.push(v);
                        Ok(CallbackReturn::Return.into())
                    }
                    MetaResult::Call(f, args) => {
                        stack.extend(args);
                        Ok(CallbackReturn::TailCall(f, None).into())
                    }
                }
            })
            .into(),
            [table.into(), key],
        )),
        _ => Ok(MetaResult::Call(call(mc, idx)?, [table, key])),
    }
}

pub fn call<'gc>(mc: MutationContext<'gc, '_>, v: Value<'gc>) -> Result<Function<'gc>, TypeError> {
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

    match metatable.get(mc, MetaMethod::Call) {
        f @ (Value::Function(_) | Value::Table(_) | Value::UserData(_)) => {
            Ok(AnyCallback::from_fn_with(mc, (v, f), |&(v, f), mc, stack| {
                stack.insert(0, v);
                Ok(CallbackReturn::TailCall(call(mc, f)?, None).into())
            })
            .into())
        }
        f => Err(TypeError {
            expected: "function",
            found: f.type_name(),
        }),
    }
}
