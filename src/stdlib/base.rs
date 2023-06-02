use std::io::{self, Write};

use gc_arena::Collect;

use crate::{
    meta_ops::{self, MetaMethod, MetaResult},
    table::NextValue,
    AnyCallback, CallbackReturn, Context, Error, IntoValue, Sequence, SequencePoll, Stack, Table,
    Value,
};

pub fn load_base<'gc>(ctx: Context<'gc>) {
    ctx.state
        .globals
        .set(
            ctx,
            "tostring",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                if stack.is_empty() {
                    Err("Bad argument to tostring".into_value(ctx).into())
                } else {
                    match meta_ops::tostring(ctx, stack.get(0))? {
                        MetaResult::Value(v) => {
                            stack[0] = v;
                            stack.drain(1..);
                            Ok(CallbackReturn::Return)
                        }
                        MetaResult::Call(func, args) => {
                            stack.replace(ctx, args);
                            Ok(CallbackReturn::TailCall(func, None))
                        }
                    }
                }
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "print",
            AnyCallback::from_fn(&ctx, |_, stack| {
                #[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
                #[collect(require_static)]
                enum Mode {
                    Init,
                    First,
                    Rest,
                }

                #[derive(Collect)]
                #[collect(no_drop)]
                struct PrintSeq<'gc> {
                    mode: Mode,
                    values: Vec<Value<'gc>>,
                }

                impl<'gc> Sequence<'gc> for PrintSeq<'gc> {
                    fn poll(
                        &mut self,
                        ctx: Context<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<crate::SequencePoll<'gc>, Error<'gc>> {
                        let mut stdout = io::stdout();

                        if self.mode == Mode::Init {
                            self.mode = Mode::First;
                        } else {
                            self.values.push(stack.get(0));
                        }
                        stack.clear();

                        while let Some(value) = self.values.pop() {
                            match meta_ops::tostring(ctx, value)? {
                                MetaResult::Value(v) => {
                                    if self.mode == Mode::First {
                                        self.mode = Mode::Rest;
                                    } else {
                                        stdout.write_all(&b"\t"[..])?;
                                    }
                                    v.display(&mut stdout)?
                                }
                                MetaResult::Call(function, args) => {
                                    stack.extend(args);
                                    return Ok(SequencePoll::Call {
                                        function,
                                        is_tail: false,
                                    });
                                }
                            }
                        }

                        stdout.write_all(&b"\n"[..])?;
                        stdout.flush()?;
                        Ok(SequencePoll::Return)
                    }
                }

                Ok(CallbackReturn::Sequence(
                    PrintSeq {
                        mode: Mode::Init,
                        values: stack.drain(..).rev().collect(),
                    }
                    .into(),
                ))
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "error",
            AnyCallback::from_fn(&ctx, |_, stack| {
                Err(stack.pop_front().unwrap_or_default().into())
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "assert",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                if stack.get(0).to_bool() {
                    Ok(CallbackReturn::Return)
                } else if stack.get(1).is_nil() {
                    Err("assertion failed!".into_value(ctx).into())
                } else {
                    Err(stack.get(1).into())
                }
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "pcall",
            AnyCallback::from_fn(&ctx, move |ctx, stack| {
                #[derive(Collect)]
                #[collect(require_static)]
                struct PCall;

                impl<'gc> Sequence<'gc> for PCall {
                    fn poll(
                        &mut self,
                        _ctx: Context<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                        stack.push_front(Value::Boolean(true));
                        Ok(SequencePoll::Return)
                    }

                    fn error(
                        &mut self,
                        ctx: Context<'gc>,
                        error: Error<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                        stack.clear();
                        stack.extend([Value::Boolean(false), error.to_value(&ctx)]);
                        Ok(SequencePoll::Return)
                    }
                }

                let function = meta_ops::call(ctx, stack.get(0))?;
                stack.pop_front();
                Ok(CallbackReturn::TailCall(function, Some(PCall.into())))
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "type",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                if let Some(v) = stack.consume::<Option<Value>>(ctx)? {
                    stack.replace(ctx, v.type_name());
                    Ok(CallbackReturn::Return)
                } else {
                    Err("Missing argument to type".into_value(ctx).into())
                }
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "select",
            AnyCallback::from_fn(&ctx, |ctx, stack| match stack.get(0).to_integer() {
                Some(n) if n >= 1 => {
                    let last = (n as usize).min(stack.len());
                    stack.drain(0..last);
                    Ok(CallbackReturn::Return)
                }
                _ => Err("Bad argument to 'select'".into_value(ctx).into()),
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "rawget",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let (table, key): (Table, Value) = stack.consume(ctx)?;
                stack.replace(ctx, table.get(ctx, key));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "rawset",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let (table, key, value): (Table, Value, Value) = stack.consume(ctx)?;
                table.set(ctx, key, value)?;
                stack.replace(ctx, table);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "getmetatable",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                if let Value::Table(t) = stack.get(0) {
                    stack.replace(ctx, t.metatable());
                    Ok(CallbackReturn::Return)
                } else {
                    Err("'getmetatable' can only be used on table types"
                        .into_value(ctx)
                        .into())
                }
            }),
        )
        .unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "setmetatable",
            AnyCallback::from_fn(&ctx, |ctx, stack| {
                let (t, mt): (Table, Option<Table>) = stack.consume(ctx)?;
                t.set_metatable(&ctx, mt);
                stack.replace(ctx, t);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn next<'gc>(
        ctx: Context<'gc>,
        table: Table<'gc>,
        index: Value<'gc>,
    ) -> Result<(Value<'gc>, Value<'gc>), Value<'gc>> {
        match table.next(ctx, index) {
            NextValue::Found { key, value } => Ok((key, value)),
            NextValue::Last => Ok((Value::Nil, Value::Nil)),
            NextValue::NotFound => Err("invalid table key".into_value(ctx)),
        }
    }

    let next = AnyCallback::from_fn(&ctx, |ctx, stack| {
        let (table, index): (Table, Value) = stack.consume(ctx)?;
        stack.replace(ctx, next(ctx, table, index)?);
        Ok(CallbackReturn::Return)
    });

    ctx.state.globals.set(ctx, "next", next).unwrap();

    ctx.state
        .globals
        .set(
            ctx,
            "pairs",
            AnyCallback::from_fn_with(&ctx, next, move |next, ctx, stack| {
                let table = stack.get(0);
                if let Some(mt) = match table {
                    Value::Table(t) => t.metatable(),
                    Value::UserData(u) => u.metatable(),
                    _ => None,
                } {
                    let pairs = mt.get(ctx, MetaMethod::Pairs);
                    if !pairs.is_nil() {
                        let f = meta_ops::call(ctx, pairs)?;
                        stack.replace(ctx, (table, Value::Nil));
                        return Ok(CallbackReturn::TailCall(f, None));
                    }
                }

                stack.replace(ctx, (*next, table));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    let inext = AnyCallback::from_fn(&ctx, |ctx, stack| {
        let (table, index): (Value, Option<i64>) = stack.consume(ctx)?;
        let next_index = index.unwrap_or(0) + 1;
        Ok(match meta_ops::index(ctx, table, next_index.into())? {
            MetaResult::Value(v) => {
                if !v.is_nil() {
                    stack.extend([next_index.into(), v]);
                }
                CallbackReturn::Return
            }
            MetaResult::Call(f, args) => {
                #[derive(Collect)]
                #[collect(require_static)]
                struct INext(i64);

                impl<'gc> Sequence<'gc> for INext {
                    fn poll(
                        &mut self,
                        _ctx: Context<'gc>,
                        stack: &mut Stack<'gc>,
                    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                        if !stack.get(0).is_nil() {
                            stack.push_front(self.0.into());
                        }
                        Ok(SequencePoll::Return)
                    }
                }

                stack.extend(args);
                CallbackReturn::TailCall(f, Some(INext(next_index).into()))
            }
        })
    });

    ctx.state
        .globals
        .set(
            ctx,
            "ipairs",
            AnyCallback::from_fn_with(&ctx, inext, move |inext, ctx, stack| {
                stack.into_front(ctx, *inext);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();
}
