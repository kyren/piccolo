use gc_arena::{Collect, MutationContext};

use crate::{AnyCallback, CallbackReturn, Function, TypeError, Value};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum MetaMethod {
    Call,
}

impl MetaMethod {
    pub const fn name(self) -> &'static str {
        match self {
            MetaMethod::Call => "__call",
        }
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

    match metatable.get(MetaMethod::Call.name()) {
        f @ (Value::Function(_) | Value::Table(_) | Value::UserData(_)) => {
            Ok(AnyCallback::from_fn_with(mc, (v, f), |&(v, f), mc, stack| {
                stack.insert(0, v);
                Ok(CallbackReturn::TailCall {
                    function: call(mc, f)?,
                    continuation: None,
                }
                .into())
            })
            .into())
        }
        f => Err(TypeError {
            expected: "function",
            found: f.type_name(),
        }),
    }
}
