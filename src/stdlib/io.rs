use std::io::{self, Write};

use gc_arena::Collect;

use crate::{
    meta_ops::{self, MetaResult},
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, Sequence, SequencePoll,
    Stack, Value,
};

pub fn load_io<'gc>(ctx: Context<'gc>) {
    ctx.set_global(
        "print",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
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
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
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
                            MetaResult::Call(call) => {
                                stack.extend(call.args);
                                return Ok(SequencePoll::Call {
                                    function: call.function,
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

            Ok(CallbackReturn::Sequence(BoxSequence::new(
                &ctx,
                PrintSeq {
                    mode: Mode::Init,
                    values: stack.drain(..).rev().collect(),
                },
            )))
        }),
    )
    .unwrap();
}
