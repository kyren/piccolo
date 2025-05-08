use either::Either;
use gc_arena::Collect;
use std::{
    fs::OpenOptions,
    io::{self, Seek, SeekFrom, Write},
    pin::Pin,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

mod file;
mod state;
mod std_file_kind;

use self::{state::IoState, std_file_kind::StdFileKind};
use crate::{
    meta_ops::{self, MetaResult}, BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, MetaMethod, Sequence, SequencePoll, Stack, String, Table, UserData, Value
};

pub use file::IoFile;

pub fn load_io<'gc>(ctx: Context<'gc>) {
    thread_local! {
        static IO_STATE: IoState = IoState::new();
    }

    let io = Table::new(&ctx);

    io.set_field(
        ctx,
        "stdin",
        UserData::new_static(&ctx, IoFile::stdin()),
    );
    io.set_field(
        ctx,
        "stdout",
        UserData::new_static(&ctx, IoFile::stdout()),
    );
    io.set_field(
        ctx,
        "stderr",
        UserData::new_static(&ctx, IoFile::stderr()),
    );

    ctx.set_global(
        "print",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct PrintSeq {
                first: bool,
            }

            impl<'gc> Sequence<'gc> for PrintSeq {
                fn poll(
                    mut self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let mut stdout = io::stdout();

                    while let Some(value) = stack.pop_back() {
                        match meta_ops::tostring(ctx, value)? {
                            MetaResult::Value(v) => {
                                if self.first {
                                    self.first = false;
                                } else {
                                    stdout.write_all(b"\t")?;
                                }
                                if let Value::String(s) = v {
                                    stdout.write_all(s.as_bytes())?;
                                } else {
                                    write!(stdout, "{}", v.display())?;
                                }
                            }
                            MetaResult::Call(call) => {
                                let bottom = stack.len();
                                stack.extend(call.args);
                                return Ok(SequencePoll::Call {
                                    function: call.function,
                                    bottom,
                                });
                            }
                        }
                    }

                    stdout.write_all(b"\n")?;
                    stdout.flush()?;
                    Ok(SequencePoll::Return)
                }
            }

            stack[..].reverse();

            Ok(CallbackReturn::Sequence(BoxSequence::new(
                &ctx,
                PrintSeq { first: true },
            )))
        }),
    );

    io.set_field(
        ctx,
        "open",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (filename, mode) = stack.consume::<(String, Option<String>)>(ctx)?;
            let filename = filename.to_str()?;
            let mode = mode.map(|s| s.to_str()).unwrap_or(Ok("r"))?;

            let mut file = OpenOptions::new();

            let mode = mode.replace("b", "");

            let file = match mode.as_str() {
                "r" => file.read(true),
                "w" => file.write(true).create(true).truncate(true),
                "a" => file.write(true).create(true).append(true),
                "r+" => file.read(true).write(true),
                "w+" => file.read(true).write(true).create(true).truncate(true),
                "a+" => file.read(true).write(true).create(true).append(true),
                _ => return Err("invalid `mode`".into_value(ctx).into()),
            };

            match file.open(filename) {
                Ok(file) => {
                    stack.replace(ctx, UserData::new_static(&ctx, IoFile::new(file)));
                    Ok(CallbackReturn::Return)
                }
                Err(err) => {
                    stack.replace(
                        ctx,
                        (Value::Nil, err.to_string(), err.raw_os_error().unwrap_or(0)),
                    );
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    io.set_field(
        ctx,
        "input",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Collect)]
            #[collect(no_drop)]
            struct InputOpenThen;

            impl<'gc> Sequence<'gc> for InputOpenThen {
                fn poll(
                    self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let file: Value = stack.get(0);

                    if file.is_nil() {
                        return Ok(SequencePoll::Return);
                    }

                    match file {
                        Value::UserData(file) => {
                            if let Ok(file) = file.downcast_static::<IoFile>() {
                                IO_STATE.with(|state| state.replace_input(file.clone()));
                                Ok(SequencePoll::Return)
                            } else {
                                Err("bad argument #1 to 'input' (file expected)"
                                    .into_value(ctx)
                                    .into())
                            }
                        }
                        _ => Err("bad argument #1 to 'input' (file expected)"
                            .into_value(ctx)
                            .into()),
                    }
                }
            }

            if stack.is_empty() {
                stack.replace(ctx, UserData::new_static(&ctx, IO_STATE.with(|state| state.input())));
                return Ok(CallbackReturn::Return);
            }

            let value = stack.consume::<Value>(ctx)?;

            match value {
                Value::String(filename) => {
                    let io: Table = ctx.globals().get(ctx, "io")?;
                    let open: Callback = io.get(ctx, "open")?;

                    stack.replace(ctx, (filename, "r"));
                    Ok(CallbackReturn::Call {
                        function: open.into(),
                        then: Some(BoxSequence::new(&ctx, InputOpenThen)),
                    })
                }
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<IoFile>() {
                        IO_STATE.with(|state| state.replace_input(file.clone()));

                        stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                        Ok(CallbackReturn::Return)
                    } else {
                        Err("expected `file`".into_value(ctx).into())
                    }
                }
                _ => Err("expected `string` or `file`".into_value(ctx).into()),
            }
        }),
    );

    io.set_field(
        ctx,
        "output",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Collect)]
            #[collect(no_drop)]
            struct OutputOpenThen;

            impl<'gc> Sequence<'gc> for OutputOpenThen {
                fn poll(
                    self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let file: Value = stack.get(0);

                    if file.is_nil() {
                        return Ok(SequencePoll::Return);
                    }

                    match file {
                        Value::UserData(file) => {
                            if let Ok(file) = file.downcast_static::<IoFile>() {
                                IO_STATE.with(|state| state.replace_output(file.clone()));
                                Ok(SequencePoll::Return)
                            } else {
                                Err("bad argument #1 to 'input' (file expected)"
                                    .into_value(ctx)
                                    .into())
                            }
                        }
                        _ => Err("bad argument #1 to 'input' (file expected)"
                            .into_value(ctx)
                            .into()),
                    }
                }
            }

            if stack.is_empty() {
                stack.replace(ctx, UserData::new_static(&ctx, IO_STATE.with(|state| state.output())));
                return Ok(CallbackReturn::Return);
            }

            let value = stack.consume::<Value>(ctx)?;

            match value {
                Value::String(filename) => {
                    let io: Table = ctx.globals().get(ctx, "io")?;
                    let open: Callback = io.get(ctx, "open")?;

                    stack.replace(ctx, (filename, "w"));
                    return Ok(CallbackReturn::Call {
                        function: open.into(),
                        then: Some(BoxSequence::new(&ctx, OutputOpenThen)),
                    });
                }
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<IoFile>() {
                        IO_STATE.with(|state| state.replace_output(file.clone()));

                        stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                        Ok(CallbackReturn::Return)
                    } else {
                        Err("expected `file`".into_value(ctx).into())
                    }
                }
                _ => Err("expected `string` or `file`".into_value(ctx).into()),
            }
        }),
    );

    io.set_field(
        ctx,
        "close",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                let output = IO_STATE.with(|state| state.output());

                if !output.is_std() {
                    if let Err(err) = output.close() {
                        stack.replace(
                            ctx,
                            (Value::Nil, err.to_string(), err.raw_os_error().unwrap_or(0)),
                        );
                        return Ok(CallbackReturn::Return);
                    }
                    IO_STATE.with(|state| state.replace_output(IoFile::stdout()));
                    stack.replace(ctx, true);
                    return Ok(CallbackReturn::Return);
                }

                stack.replace(ctx, true);
                return Ok(CallbackReturn::Return);
            }

            let file: UserData = stack.consume(ctx)?;
            let file_wrapper = if let Ok(fw) = file.downcast_static::<IoFile>() {
                fw
            } else {
                return Err("expected `file`".into_value(ctx).into());
            };

            match file_wrapper.close() {
                Ok(_) => {
                    stack.replace(ctx, true);
                    Ok(CallbackReturn::Return)
                }
                Err(e) => {
                    stack.replace(
                        ctx,
                        (Value::Nil, e.to_string(), e.raw_os_error().unwrap_or(0)),
                    );
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    io.set_field(
        ctx,
        "flush",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let output = IO_STATE.with(|state| state.output());

            if let Err(err) = output.flush() {
                stack.replace(
                    ctx,
                    (Value::Nil, err.to_string(), err.raw_os_error().unwrap_or(0)),
                );
                Ok(CallbackReturn::Return)
            } else {
                stack.replace(ctx, UserData::new_static(&ctx, output.clone()));
                Ok(CallbackReturn::Return)
            }
        }),
    );

    io.set_field(
        ctx,
        "read",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let input = IO_STATE.with(|state| state.input());

            let file: Table = ctx.io_metatable().get(ctx, MetaMethod::Index)?;
            let read: Callback = file.get(ctx, "read")?;

            stack.into_front(ctx, Value::UserData(UserData::new_static(&ctx, input)));

            Ok(CallbackReturn::Call { function: read.into(), then: None })
        }),
    );

    io.set_field(
        ctx,
        "write",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let output = IO_STATE.with(|state| state.output());
            let file: Table = ctx.io_metatable().get(ctx, MetaMethod::Index)?;
            let write: Callback = file.get(ctx, "write")?;

            stack.into_front(ctx, Value::UserData(UserData::new_static(&ctx, output)));

            Ok(CallbackReturn::Call { function: write.into(), then: None })
        }),
    );

    io.set_field(
        ctx,
        "lines",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Collect)]
            #[collect(no_drop)]
            enum LinesState {
                Open,
                Lines,
            }

            #[derive(Collect)]
            #[collect(no_drop)]
            struct LinesOpenThen<'gc> {
                open: Callback<'gc>,
                lines: Callback<'gc>,
                formats: Vec<Value<'gc>>,
                state: LinesState,
            }

            impl<'gc> LinesOpenThen<'gc> {
                fn new(
                    open: Callback<'gc>,
                    lines: Callback<'gc>,
                    formats: Vec<Value<'gc>>,
                ) -> Self {
                    Self {
                        open,
                        lines,
                        formats,
                        state: LinesState::Open,
                    }
                }
            }

            impl<'gc> Sequence<'gc> for LinesOpenThen<'gc> {
                fn poll(
                    mut self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    match self.state {
                        LinesState::Open => {
                            self.state = LinesState::Lines;
                            Ok(SequencePoll::Call {
                                function: self.open.into(),
                                bottom: 0,
                            })
                        }
                        LinesState::Lines => {
                            let file: Value = stack.consume(ctx)?;

                            if file.is_nil() {
                                return Ok(SequencePoll::Return);
                            }

                            stack.clear();
                            stack.into_back(ctx, file);
                            self.formats.iter().for_each(|format| {
                                stack.into_back(ctx, *format);
                            });

                            Ok(SequencePoll::Call {
                                function: self.lines.into(),
                                bottom: 0,
                            })
                        }
                    }
                }
            }

            let file: Table = ctx.io_metatable().get(ctx, MetaMethod::Index)?;
            let lines: Callback = file.get(ctx, "lines")?;

            if stack.is_empty() {
                let input = IO_STATE.with(|state| state.input());

                stack.replace(ctx, (UserData::new_static(&ctx, input), "l"));

                Ok(CallbackReturn::Call { function: lines.into(), then: None })
            } else {
                let filename = stack.consume(ctx)?;
                if let Value::String(_) = filename {
                    let io: Table = ctx.io_metatable().get(ctx, MetaMethod::Index)?;
                    let open: Callback = io.get(ctx, "open")?;

                    let formats = if stack.is_empty() {
                        vec![ctx.intern(b"l").into_value(ctx)] // default format
                    } else {
                        stack.into_iter().collect::<Vec<_>>()
                    };

                    stack.replace(ctx, (filename, "r"));

                    Ok(CallbackReturn::Sequence(BoxSequence::new(
                        &ctx,
                        LinesOpenThen::new(open, lines, formats),
                    )))
                } else {
                    Err("bad argument #1 to 'lines' (string expected)"
                        .into_value(ctx)
                        .into())
                }
            }
        }),
    );

    io.set_field(
        ctx,
        "type",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'type' (value expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: Value = stack.consume(ctx)?;

            match file {
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<IoFile>() {
                        if file.is_some() {
                            stack.replace(ctx, "file")
                        } else {
                            stack.replace(ctx, "closed file")
                        }
                    } else {
                        stack.replace(ctx, Value::Nil);
                    }
                }
                _ => stack.replace(ctx, Value::Nil),
            }

            Ok(CallbackReturn::Return)
        }),
    );

    io.set_field(
        ctx,
        "tmpfile",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let tmpfile = tempfile::tempfile()?;

            stack.replace(ctx, UserData::new_static(&ctx, IoFile::new(tmpfile)));

            Ok(CallbackReturn::Return)
        }),
    );

    let file = Table::new(&ctx);

    ctx.io_metatable().set(ctx, MetaMethod::Index, file).unwrap();

    file.set_field(
        ctx,
        "close",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'close' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: UserData = stack.consume(ctx)?;
            let file = if let Ok(file) = file.downcast_static::<IoFile>() {
                file
            } else {
                return Err("bad argument #1 to 'close' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            match file.close() {
                Ok(_) => {
                    stack.replace(ctx, true);
                    Ok(CallbackReturn::Return)
                }
                Err(e) => {
                    stack.replace(
                        ctx,
                        (Value::Nil, e.to_string(), e.raw_os_error().unwrap_or(0)),
                    );
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    file.set_field(
        ctx,
        "flush",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'flush' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: UserData = stack.consume(ctx)?;
            let file = if let Ok(file) = file.downcast_static::<IoFile>() {
                file
            } else {
                return Err("bad argument #1 to 'flush' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            match file.flush() {
                Ok(_) => {
                    stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                    Ok(CallbackReturn::Return)
                }
                Err(e) => {
                    stack.replace(
                        ctx,
                        (Value::Nil, e.to_string(), e.raw_os_error().unwrap_or(0)),
                    );
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    file.set_field(
        ctx,
        "read",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'read' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: Value = stack.get(0);
            let file = if let Value::UserData(file) = file {
                if let Ok(file) = file.downcast_static::<IoFile>() {
                    file
                } else {
                    return Err("bad argument #1 to 'read' (file expected)"
                        .into_value(ctx)
                        .into());
                }
            } else {
                return Err("bad argument #1 to 'read' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            let formats = stack
                .into_iter()
                .skip(1)
                .enumerate()
                .map(|(n, value)| {
                    let Some(format) = value.into_string(ctx) else {
                        return Err(format!(
                            "bad argument #{} to 'read' (string expected, got {})",
                            n + 1,
                            value.type_name()
                        )
                        .into_value(ctx)
                        .into());
                    };
                    match format.to_str() {
                        Ok(s) => Ok(s),
                        Err(err) => Err(err.to_string().into_value(ctx).into()),
                    }
                })
                .collect::<Result<Vec<_>, Error<'_>>>()?;

            stack.clear();
            for format in formats {
                match file.read_with_format(ctx, format)? {
                    Some(value) => {
                        stack.into_back(ctx, value);
                    }
                    None => {
                        stack.into_back(ctx, Value::Nil);
                        break;
                    }
                }
            }

            Ok(CallbackReturn::Return)
        }),
    );

    file.set_field(
        ctx,
        "lines",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Collect, Clone)]
            #[collect(require_static)]
            struct Lines {
                position: Rc<AtomicUsize>,
                file: IoFile,
                formats: Vec<std::string::String>,
            }

            impl<'gc> Sequence<'gc> for Lines {
                fn poll(
                    self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    if let Some(format) = self.formats.get(self.position.load(Ordering::Relaxed)) {
                        match self.file.read_with_format(ctx, format)? {
                            Some(value) => {
                                stack.into_back(ctx, value);
                            }
                            None => {}
                        }
                        self.position.fetch_add(1, Ordering::Relaxed);
                        return Ok(SequencePoll::Return);
                    } else {
                        return Ok(SequencePoll::Return);
                    }
                }
            }

            if stack.is_empty() {
                return Err("bad argument #1 to 'lines' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: Value = stack.get(0);
            let file = if let Value::UserData(file) = file {
                if let Ok(file) = file.downcast_static::<IoFile>() {
                    file
                } else {
                    return Err("bad argument #1 to 'lines' (file expected)"
                        .into_value(ctx)
                        .into());
                }
            } else {
                return Err("bad argument #1 to 'lines' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            let values = if stack.is_empty() {
                vec![ctx.intern(b"l").into_value(ctx)] // default format
            } else {
                stack.into_iter().skip(1).collect::<Vec<_>>()
            };

            let formats = values
                .into_iter()
                .enumerate()
                .map(|(n, value)| {
                    let Some(format) = value.into_string(ctx) else {
                        return Err(format!(
                            "bad argument #{} to 'lines' (string expected, got {})",
                            n + 1,
                            value.type_name()
                        )
                        .into_value(ctx)
                        .into());
                    };
                    match format.to_str() {
                        Ok(s) => Ok(s.to_string()),
                        Err(err) => Err(err.to_string().into_value(ctx).into()),
                    }
                })
                .collect::<Result<Vec<_>, Error<'_>>>()?;

            let root = Lines {
                position: Rc::new(AtomicUsize::new(0)),
                file: file.clone(),
                formats,
            };

            let lines = Callback::from_fn_with(&ctx, root, |root, ctx, _, _| {
                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    root.clone(),
                )))
            });

            stack.replace(ctx, lines);
            Ok(CallbackReturn::Return)
        }),
    );

    file.set_field(
        ctx,
        "seek",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'seek' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let (file, whence, offset) =
                stack.consume::<(UserData, Option<String>, Option<i64>)>(ctx)?;
            let file = if let Ok(file) = file.downcast_static::<IoFile>() {
                file
            } else {
                return Err("bad argument #1 to 'seek' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            let whence_str = whence.map(|s| s.to_str().unwrap_or("cur")).unwrap_or("cur");
            let offset = offset.unwrap_or(0);

            let seek_from = match whence_str {
                "set" => SeekFrom::Start(offset as u64),
                "cur" => SeekFrom::Current(offset),
                "end" => SeekFrom::End(offset),
                _ => {
                    return Err(format!("invalid option '{}'", whence_str)
                        .into_value(ctx)
                        .into())
                }
            };

            match file.inner() {
                Either::Left(left) => {
                    let mut file = left.borrow_mut();
                    if let Some(ref mut file) = *file {
                        match file.seek(seek_from) {
                            Ok(position) => {
                                stack.replace(ctx, position as i64);
                                Ok(CallbackReturn::Return)
                            }
                            Err(err) => {
                                stack.replace(
                                    ctx,
                                    (Value::Nil, err.to_string(), err.raw_os_error().unwrap_or(0)),
                                );
                                Ok(CallbackReturn::Return)
                            }
                        }
                    } else {
                        Err("attempt to use a closed file".into_value(ctx).into())
                    }
                }
                Either::Right(_) => {
                    stack.replace(ctx, (Value::Nil, "Illegal seek", 29));
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    file.set_field(
        ctx,
        "write",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Err("bad argument #1 to 'write' (file expected)"
                    .into_value(ctx)
                    .into());
            }

            let file: Value = stack.get(0);
            let file = if let Value::UserData(file) = file {
                if let Ok(file) = file.downcast_static::<IoFile>() {
                    file
                } else {
                    return Err("bad argument #1 to 'write' (file expected)"
                        .into_value(ctx)
                        .into());
                }
            } else {
                return Err("bad argument #1 to 'write' (file expected)"
                    .into_value(ctx)
                    .into());
            };
            let values = stack
                .into_iter()
                .skip(1)
                .enumerate()
                .map(|(n, value)| {
                    let Some(s) = value.into_string(ctx) else {
                        return Err(format!(
                            "bad argument #{} to 'write' (string expected, got {})",
                            n + 1,
                            value.type_name()
                        )
                        .into_value(ctx)
                        .into());
                    };
                    match s.to_str() {
                        Ok(s) => Ok(s),
                        Err(err) => Err(err.to_string().into_value(ctx).into()),
                    }
                })
                .collect::<Result<Vec<_>, Error<'_>>>()?;

            match file.inner() {
                Either::Left(left) => {
                    let mut left = left.borrow_mut();
                    if let Some(ref mut file) = *left {
                        for value in values {
                            file.write_all(value.as_bytes())?;
                        }
                    }
                    stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                    Ok(CallbackReturn::Return)
                }
                Either::Right(kind) => {
                    let mut output: Box<dyn Write> = match kind {
                        StdFileKind::Stdout => Box::new(io::stdout()),
                        StdFileKind::Stderr => Box::new(io::stderr()),
                        StdFileKind::Stdin => return Err("attempt to write to stdin"
                            .into_value(ctx)
                            .into()),
                    };
                    for value in values {
                        output.write_all(value.as_bytes())?;
                    }
                    stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                    Ok(CallbackReturn::Return)
                }
            }
        }),
    );

    ctx.set_global("io", io);
}
