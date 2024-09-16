use std::pin::Pin;

use gc_arena::Collect;

use crate::{
    BoxSequence, Callback, CallbackReturn, Closure, Context, Error, Execution, Function, Sequence,
    SequencePoll, Stack, String, Table, TypeError, Value,
};

pub fn load_load<'gc>(ctx: Context<'gc>) {
    ctx.set_global(
        "load",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (chunk, chunk_name, mode, env): (
                Value,
                Option<String>,
                Option<String>,
                Option<Table>,
            ) = stack.consume(ctx)?;

            let name = chunk_name.unwrap_or_else(|| ctx.intern_static(b"=(load)"));
            let mode = mode.unwrap_or_else(|| ctx.intern_static(b"bt"));
            let env = env.unwrap_or_else(|| ctx.globals());

            #[derive(Collect)]
            #[collect(require_static)]
            enum LoadMode {
                Text,
                Binary,
                BinaryOrText,
            }

            let mode = match mode.as_ref() {
                b"t" => LoadMode::Text,
                b"b" => LoadMode::Binary,
                b"bt" => LoadMode::BinaryOrText,
                _m => {
                    let error = "invalid load mode";
                    stack.replace(ctx, (Value::Nil, error));
                    return Ok(CallbackReturn::Return);
                }
            };

            if matches!(mode, LoadMode::Binary) {
                let error = "loading binary chunks is not currently supported";
                stack.replace(ctx, (Value::Nil, error));
                return Ok(CallbackReturn::Return);
            }

            let root = (name, mode, env);
            let inner = Callback::from_fn_with(&ctx, root, |root, ctx, _, mut stack| {
                let (name, _mode, env) = root;
                let chunk: String = stack.consume(ctx)?;
                let name = format!("{}", name.display_lossy());

                match Closure::load_with_env(ctx, Some(&*name), chunk.as_ref(), *env) {
                    Ok(closure) => {
                        stack.push_back(Value::Function(closure.into()));
                        Ok(CallbackReturn::Return)
                    }
                    Err(e) => {
                        let error = Error::from(e).to_string();
                        stack.replace(ctx, (Value::Nil, error));
                        Ok(CallbackReturn::Return)
                    }
                }
            });
            let inner: Function = inner.into();

            match chunk {
                Value::String(_) => {
                    stack.push_back(chunk);
                    Ok(CallbackReturn::Call {
                        function: inner,
                        then: None,
                    })
                }
                Value::Function(func) => {
                    // Should this support metamethod-callable values?
                    // PRLua only allows raw functions here.
                    Ok(CallbackReturn::Sequence(BoxSequence::new(
                        &ctx,
                        BuildLoadString {
                            step: 0,
                            total_len: 0,
                            func,
                            then: inner,
                        },
                    )))
                }
                _ => Err(TypeError {
                    expected: "string or function",
                    found: chunk.type_name(),
                }
                .into()),
            }
        }),
    );
}

#[derive(Collect)]
#[collect(no_drop)]
struct BuildLoadString<'gc> {
    step: usize,
    total_len: usize,
    func: Function<'gc>,
    then: Function<'gc>,
}
impl BuildLoadString<'_> {
    fn finalize<'gc>(&self, ctx: Context<'gc>, stack: &mut Stack<'gc, '_>) -> String<'gc> {
        // There's no easy way to construct the string in-place with gc-arena,
        // so we construct the string on the normal heap and copy then it to a
        // new piccolo String allocation.
        let mut bytes = Vec::with_capacity(self.total_len);
        for value in stack.drain(..) {
            let Value::String(s) = value else {
                unreachable!() // guaranteed by the BuildLoadString sequence
            };
            bytes.extend(s.as_bytes());
        }
        // This isn't interned as it will only be used by the parser
        String::from_slice(&ctx, &bytes)
    }
}

impl<'gc> Sequence<'gc> for BuildLoadString<'gc> {
    fn poll(
        mut self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        stack.resize(self.step);

        if self.step != 0 {
            let done = match stack.get_mut(self.step - 1) {
                None | Some(Value::Nil) => true,
                Some(v) => {
                    // PRLua implicitly converts integer/number values to strings in load
                    let Some(s) = v.into_string(ctx) else {
                        let error = Error::from(TypeError {
                            expected: "string",
                            found: v.type_name(),
                        });
                        stack.replace(ctx, (Value::Nil, error.to_value(ctx)));
                        return Ok(SequencePoll::Return);
                    };
                    *v = Value::String(s);
                    self.total_len += s.len() as usize;
                    s.is_empty()
                }
            };
            if done {
                // The last arg was nil or an empty string, so the load
                // function is done.
                stack.pop_back();
                let str = self.finalize(ctx, &mut stack);
                stack.push_back(Value::String(str));
                return Ok(SequencePoll::TailCall(self.then));
            }
        }

        let bottom = self.step;
        self.step += 1;
        Ok(SequencePoll::Call {
            function: self.func,
            bottom,
        })
    }

    fn error(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        error: Error<'gc>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        // This catches errors thrown by the inner function;
        // PUC-Rio's tests require it, but it's not documented.
        let error = error.to_value(ctx);
        stack.replace(ctx, (Value::Nil, error));
        return Ok(SequencePoll::Return);
    }
}
