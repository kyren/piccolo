use either::Either;
use gc_arena::Collect;
use std::{
    cell::RefCell,
    fs::{File, OpenOptions},
    io::{self, Read, Seek, SeekFrom, Write},
    pin::Pin,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    meta_ops::{self, MetaResult},
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, Sequence,
    SequencePoll, Stack, String, Table, UserData, Value,
};

#[derive(Collect, Clone)]
#[collect(require_static)]
pub struct FileWrapper(Either<Rc<RefCell<Option<File>>>, StdFileKind>);

impl FileWrapper {
    pub fn new(file: File) -> Self {
        Self(Either::Left(Rc::new(RefCell::new(Some(file)))))
    }
    pub fn stdin() -> Self {
        Self(Either::Right(StdFileKind::Stdin))
    }
    pub fn stdout() -> Self {
        Self(Either::Right(StdFileKind::Stdout))
    }
    pub fn stderr() -> Self {
        Self(Either::Right(StdFileKind::Stderr))
    }
    pub fn is_std(&self) -> bool {
        matches!(self.0, Either::Right(_))
    }
    pub fn close(&self) -> Result<(), io::Error> {
        if self.is_std() {
            Ok(())
        } else {
            let Either::Left(left) = &self.0 else {
                unreachable!()
            };

            let mut file_lock = left.borrow_mut();
            if let Some(file_handle) = file_lock.take() {
                file_handle.sync_all()?;
            }
            Ok(())
        }
    }
    pub fn flush(&self) -> Result<(), io::Error> {
        match &self.0 {
            Either::Left(left) => {
                let mut file_lock = left.borrow_mut();
                if let Some(ref mut file_handle) = *file_lock {
                    file_handle.flush()?;
                }
                Ok(())
            }
            Either::Right(kind) => {
                if matches!(kind, StdFileKind::Stdout | StdFileKind::Stderr) {
                    io::stdout().flush()
                } else {
                    Ok(())
                }
            }
        }
    }
    pub fn read_with_format<'gc>(
        &self,
        ctx: Context<'gc>,
        format: &str,
    ) -> Result<Option<Value<'gc>>, Error<'gc>> {
        fn read_from_stream<'gc, R: Read>(
            ctx: Context<'gc>,
            stream: &mut R,
            format: &str,
        ) -> Result<Option<Value<'gc>>, Error<'gc>> {
            match format {
                "*l" | "l" => {
                    let mut buf = Vec::new();
                    let mut byte = [0u8; 1];

                    loop {
                        match stream.read(&mut byte) {
                            Ok(0) => {
                                if buf.is_empty() {
                                    return Ok(None);
                                }
                                break;
                            }
                            Ok(_) => {
                                if byte[0] == b'\n' {
                                    break;
                                } else if byte[0] == b'\r' {
                                    let mut next_byte = [0u8; 1];
                                    match stream.read(&mut next_byte) {
                                        Ok(1) => {
                                            if next_byte[0] != b'\n' {
                                                // Wasn't \r\n, but we can't put it back if the stream doesn't support seek
                                                // We'll just consider this a part of the next line when reading continues
                                            }
                                        }
                                        _ => {}
                                    }
                                    break;
                                } else {
                                    buf.push(byte[0]);
                                }
                            }
                            Err(e) => return Err(e.to_string().into_value(ctx).into()),
                        }
                    }

                    Ok(Some(ctx.intern(&buf).into()))
                }

                "*L" | "L" => {
                    let mut buf = Vec::new();
                    let mut byte = [0u8; 1];

                    loop {
                        match stream.read(&mut byte) {
                            Ok(0) => {
                                if buf.is_empty() {
                                    return Ok(None);
                                }
                                break;
                            }
                            Ok(_) => {
                                buf.push(byte[0]);
                                if byte[0] == b'\n' {
                                    break;
                                } else if byte[0] == b'\r' {
                                    let mut next_byte = [0u8; 1];
                                    match stream.read(&mut next_byte) {
                                        Ok(1) => {
                                            if next_byte[0] == b'\n' {
                                                buf.push(next_byte[0]);
                                            } else {
                                                // Again, we can't put back what we read in a generic Read
                                                // This byte will be part of the next line
                                            }
                                            break;
                                        }
                                        _ => break,
                                    }
                                }
                            }
                            Err(e) => return Err(e.to_string().into_value(ctx).into()),
                        }
                    }

                    Ok(Some(ctx.intern(&buf).into()))
                }

                "*a" | "a" => {
                    let mut buf = Vec::new();
                    match stream.read_to_end(&mut buf) {
                        Ok(0) => Ok(Some(ctx.intern(&[]).into())),
                        Ok(_) => Ok(Some(ctx.intern(&buf).into())),
                        Err(e) => Err(e.to_string().into_value(ctx).into()),
                    }
                }

                "n" => {
                    let mut buf = Vec::new();
                    let mut byte = [0u8; 1];
                    let mut has_digit = false;

                    loop {
                        match stream.read(&mut byte) {
                            Ok(0) => return Ok(None),
                            Ok(_) => {
                                if !byte[0].is_ascii_whitespace() {
                                    buf.push(byte[0]);
                                    if byte[0].is_ascii_digit() {
                                        has_digit = true;
                                    }
                                    break;
                                }
                            }
                            Err(e) => return Err(e.to_string().into_value(ctx).into()),
                        }
                    }

                    loop {
                        match stream.read(&mut byte) {
                            Ok(0) => break,
                            Ok(_) => {
                                if byte[0].is_ascii_digit()
                                    || byte[0] == b'.'
                                    || byte[0] == b'-'
                                    || byte[0] == b'+'
                                    || byte[0] == b'e'
                                    || byte[0] == b'E'
                                {
                                    buf.push(byte[0]);
                                    if byte[0].is_ascii_digit() {
                                        has_digit = true;
                                    }
                                } else {
                                    break;
                                }
                            }
                            Err(e) => return Err(e.to_string().into_value(ctx).into()),
                        }
                    }

                    if !has_digit {
                        return Ok(None);
                    }

                    let s = std::string::String::from_utf8_lossy(&buf);
                    if let Ok(i) = s.parse::<i64>() {
                        Ok(Some(i.into()))
                    } else if let Ok(f) = s.parse::<f64>() {
                        Ok(Some(f.into()))
                    } else {
                        Ok(None)
                    }
                }

                _ => {
                    if let Ok(n) = format.parse::<usize>() {
                        if n == 0 {
                            let mut empty_buf: [u8; 0] = [];
                            match stream.read_exact(&mut empty_buf) {
                                Ok(_) => return Ok(Some(ctx.intern(&[]).into())),
                                Err(_) => return Ok(None),
                            }
                        }

                        let mut buf = vec![0u8; n];
                        match stream.read(&mut buf) {
                            Ok(0) => Ok(None),
                            Ok(bytes_read) => {
                                buf.truncate(bytes_read);
                                Ok(Some(ctx.intern(&buf).into()))
                            }
                            Err(e) => Err(e.to_string().into_value(ctx).into()),
                        }
                    } else {
                        Err(format!("invalid format '{}'", format)
                            .into_value(ctx)
                            .into())
                    }
                }
            }
        }

        match &self.0 {
            Either::Left(left) => {
                let mut file = left.borrow_mut();
                if let Some(mut file) = file.take() {
                    read_from_stream(ctx, &mut file, format)
                } else {
                    Err("attempt to use a closed file".into_value(ctx).into())
                }
            }
            Either::Right(kind) => match kind {
                StdFileKind::Stdin => {
                    let mut stdin = io::stdin();
                    read_from_stream(ctx, &mut stdin, format)
                }
                _ => Err("attempt to read from output file".into_value(ctx).into()),
            },
        }
    }
    pub fn is_some(&self) -> bool {
        match &self.0 {
            Either::Left(left) => left.borrow().is_some(),
            Either::Right(_) => true,
        }
    }
}

#[derive(Collect, Clone, Copy)]
#[collect(require_static)]
pub enum StdFileKind {
    Stdin,
    Stdout,
    Stderr,
}

#[derive(Collect, Clone)]
#[collect(no_drop)]
struct IoState {
    input: RefCell<FileWrapper>,
    output: RefCell<FileWrapper>,
}

fn get_io_state<'gc>(ctx: Context<'gc>) -> Result<UserData<'gc>, Error<'gc>> {
    let io: Table = ctx.globals().get(ctx, "io")?;
    let state: UserData = io.get(ctx, "__state")?;
    Ok(state)
}

impl IoState {
    pub fn replace_input(&self, input: FileWrapper) {
        self.input.replace(input);
    }
    pub fn replace_output(&self, output: FileWrapper) {
        self.output.replace(output);
    }
    pub fn input(&self) -> FileWrapper {
        self.input.borrow().clone()
    }
    pub fn output(&self) -> FileWrapper {
        self.output.borrow().clone()
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct CallWithValues<'gc> {
    callback: Callback<'gc>,
    values: Vec<Value<'gc>>,
}

impl<'gc> Sequence<'gc> for CallWithValues<'gc> {
    fn poll(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        for value in &self.values {
            stack.into_back(ctx, *value);
        }

        Ok(SequencePoll::Call {
            function: self.callback.into(),
            bottom: 0,
        })
    }
}

pub fn load_io<'gc>(ctx: Context<'gc>) {
    let io = Table::new(&ctx);
    let file = Table::new(&ctx);

    let io_state = IoState {
        input: RefCell::new(FileWrapper::stdin()),
        output: RefCell::new(FileWrapper::stdout()),
    };

    let state = UserData::new_static(&ctx, io_state);

    io.set_field(ctx, "__state", state);

    io.set_field(
        ctx,
        "stdin",
        UserData::new_static(&ctx, FileWrapper::stdin()),
    );
    io.set_field(
        ctx,
        "stdout",
        UserData::new_static(&ctx, FileWrapper::stdout()),
    );
    io.set_field(
        ctx,
        "stderr",
        UserData::new_static(&ctx, FileWrapper::stderr()),
    );

    io.set_field(
        ctx,
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
                    stack.replace(ctx, UserData::new_static(&ctx, FileWrapper::new(file)));
                    Ok(CallbackReturn::Return)
                }
                Err(err) => {
                    // Return nil, error message, error code (Lua-style error handling)
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
            struct InputOpenThen(IoState);

            impl<'gc> Sequence<'gc> for InputOpenThen {
                fn poll(
                    self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let file: Value = stack.consume(ctx)?;

                    if file.is_nil() {
                        return Ok(SequencePoll::Return);
                    }

                    match file {
                        Value::UserData(file) => {
                            if let Ok(file) = file.downcast_static::<FileWrapper>() {
                                self.0.replace_input(file.clone());
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

            let state = get_io_state(ctx)?;
            let io_state = state.downcast_static::<IoState>()?;

            if stack.is_empty() {
                stack.replace(ctx, UserData::new_static(&ctx, io_state.input()));
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
                        then: Some(BoxSequence::new(&ctx, InputOpenThen(io_state.clone()))),
                    })
                }
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<FileWrapper>() {
                        io_state.replace_input(file.clone());

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
            struct OutputOpenThen(IoState);

            impl<'gc> Sequence<'gc> for OutputOpenThen {
                fn poll(
                    self: Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let file: Value = stack.consume(ctx)?;

                    if file.is_nil() {
                        return Ok(SequencePoll::Return);
                    }

                    match file {
                        Value::UserData(file) => {
                            if let Ok(file) = file.downcast_static::<FileWrapper>() {
                                self.0.replace_output(file.clone());
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

            let state = get_io_state(ctx)?;
            let io_state = state.downcast_static::<IoState>()?;

            if stack.is_empty() {
                stack.replace(ctx, UserData::new_static(&ctx, io_state.output()));
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
                        then: Some(BoxSequence::new(&ctx, OutputOpenThen(io_state.clone()))),
                    });
                }
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<FileWrapper>() {
                        io_state.replace_output(file.clone());

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
                let state = get_io_state(ctx)?;
                let io_state = state.downcast_static::<IoState>()?;
                let output = io_state.output();

                if !output.is_std() {
                    if let Err(err) = output.close() {
                        stack.replace(
                            ctx,
                            (Value::Nil, err.to_string(), err.raw_os_error().unwrap_or(0)),
                        );
                        return Ok(CallbackReturn::Return);
                    }
                    io_state.replace_output(FileWrapper::stdout());
                    stack.replace(ctx, true);
                    return Ok(CallbackReturn::Return);
                }

                stack.replace(ctx, true);
                return Ok(CallbackReturn::Return);
            }

            let file: UserData = stack.consume(ctx)?;
            let file_wrapper = if let Ok(fw) = file.downcast_static::<FileWrapper>() {
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
            let state = get_io_state(ctx)?;
            let io_state = state.downcast_static::<IoState>()?;
            let output = io_state.output();

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
        Callback::from_fn(&ctx, |ctx, _, stack| {
            let state = get_io_state(ctx)?;
            let io_state = state.downcast_static::<IoState>()?;

            let input = io_state.input();

            let file: Table = ctx.globals().get(ctx, "file")?;
            let read: Callback = file.get(ctx, "read")?;

            let mut values = Vec::with_capacity(stack.len().wrapping_add(1));
            values.push(Value::UserData(UserData::new_static(&ctx, input)));

            stack.into_iter().for_each(|value| {
                values.push(value);
            });

            Ok(CallbackReturn::Sequence(BoxSequence::new(
                &ctx,
                CallWithValues {
                    callback: read,
                    values,
                },
            )))
        }),
    );

    io.set_field(
        ctx,
        "write",
        Callback::from_fn(&ctx, |ctx, _, stack| {
            let state = get_io_state(ctx)?;
            let io_state = state.downcast_static::<IoState>()?;

            let output = io_state.output();

            let file: Table = ctx.globals().get(ctx, "file")?;
            let write: Callback = file.get(ctx, "write")?;

            let mut values = Vec::with_capacity(stack.len().wrapping_add(1));
            values.push(Value::UserData(UserData::new_static(&ctx, output)));

            stack.into_iter().for_each(|value| {
                values.push(value);
            });

            Ok(CallbackReturn::Sequence(BoxSequence::new(
                &ctx,
                CallWithValues {
                    callback: write,
                    values,
                },
            )))
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

            let file: Table = ctx.globals().get(ctx, "file")?;
            let lines: Callback = file.get(ctx, "lines")?;

            if stack.is_empty() {
                let state = get_io_state(ctx)?;
                let io_state = state.downcast_static::<IoState>()?;

                let input = io_state.input();

                let mut values = Vec::with_capacity(2);
                values.push(Value::UserData(UserData::new_static(&ctx, input)));
                values.push(ctx.intern(b"l").into_value(ctx));

                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    CallWithValues {
                        callback: lines,
                        values,
                    },
                )))
            } else {
                let filename = stack.consume(ctx)?;
                if let Value::String(_) = filename {
                    let io: Table = ctx.globals().get(ctx, "io")?;
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
                    if let Ok(file) = file.downcast_static::<FileWrapper>() {
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

            stack.replace(ctx, UserData::new_static(&ctx, FileWrapper::new(tmpfile)));

            Ok(CallbackReturn::Return)
        }),
    );

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
            let file = if let Ok(file) = file.downcast_static::<FileWrapper>() {
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
            let file = if let Ok(file) = file.downcast_static::<FileWrapper>() {
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

            let file: UserData = stack.consume(ctx)?;
            let values = if stack.is_empty() {
                vec![ctx.intern(b"l").into_value(ctx)] // default format
            } else {
                stack.into_iter().collect::<Vec<_>>()
            };

            let formats = values
                .into_iter()
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
            let file = if let Ok(file) = file.downcast_static::<FileWrapper>() {
                file
            } else {
                return Err("bad argument #1 to 'read' (file expected)"
                    .into_value(ctx)
                    .into());
            };

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
                file: FileWrapper,
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

            let file: UserData = stack.consume(ctx)?;
            let file = if let Ok(file) = file.downcast_static::<FileWrapper>() {
                file
            } else {
                return Err("bad argument #1 to 'lines' (file expected)"
                    .into_value(ctx)
                    .into());
            };

            let values = if stack.is_empty() {
                vec![ctx.intern(b"l").into_value(ctx)] // default format
            } else {
                stack.into_iter().collect::<Vec<_>>()
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
            let file = if let Ok(file) = file.downcast_static::<FileWrapper>() {
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

            match &file.0 {
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

            let file: Value = stack.consume(ctx)?;
            let values = stack
                .into_iter()
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
            match file {
                Value::UserData(file) => {
                    if let Ok(file) = file.downcast_static::<FileWrapper>() {
                        match &file.0 {
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
                                    StdFileKind::Stdin => unreachable!(),
                                };
                                for value in values {
                                    output.write_all(value.as_bytes())?;
                                }
                                stack.replace(ctx, UserData::new_static(&ctx, file.clone()));
                                Ok(CallbackReturn::Return)
                            }
                        }
                    } else {
                        return Err("bad argument #1 to 'write' (file or string expected)"
                            .into_value(ctx)
                            .into());
                    }
                }
                Value::String(s) => {
                    let s = s.to_str()?;
                    let bytes = s.as_bytes();
                    let mut bytes = bytes.to_vec();
                    for value in values {
                        bytes.write_all(value.as_bytes())?;
                    }
                    stack.replace(ctx, ctx.intern(&bytes));
                    Ok(CallbackReturn::Return)
                }
                other => {
                    if let Some(s) = other.into_string(ctx) {
                        let bytes = s.as_bytes();
                        let mut bytes = bytes.to_vec();
                        for value in values {
                            bytes.write_all(value.as_bytes())?;
                        }
                        stack.replace(ctx, ctx.intern(&bytes));
                        Ok(CallbackReturn::Return)
                    } else {
                        return Err("bad argument #1 to 'write' (file or string expected)"
                            .into_value(ctx)
                            .into());
                    }
                }
            }
        }),
    );

    ctx.set_global("io", io);
    ctx.set_global("file", file);
}
