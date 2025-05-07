use std::{cell::RefCell, fs::File, io::{self, Read, Write}, rc::Rc};
use crate::{Context, Error, IntoValue, Value};

use super::std_file_kind::StdFileKind;
use either::Either;
use gc_arena::Collect;

#[derive(Collect, Clone)]
#[collect(require_static)]
pub struct IoFile(pub(super) Rc<Either<RefCell<Option<File>>, StdFileKind>>);

impl IoFile {
    pub fn new(file: File) -> Self {
        Self(Rc::new(Either::Left(RefCell::new(Some(file)))))
    }
    pub fn stdin() -> Self {
        Self(Rc::new(Either::Right(StdFileKind::Stdin)))
    }
    pub fn stdout() -> Self {
        Self(Rc::new(Either::Right(StdFileKind::Stdout)))
    }
    pub fn stderr() -> Self {
        Self(Rc::new(Either::Right(StdFileKind::Stderr)))
    }
    pub fn is_std(&self) -> bool {
        matches!(self.0.as_ref(), Either::Right(_))
    }
    pub fn close(&self) -> Result<(), io::Error> {
        if self.is_std() {
            Ok(())
        } else {
            let Either::Left(left) = self.0.as_ref() else {
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
        match self.0.as_ref() {
            Either::Left(left) =>    {
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

        match self.0.as_ref() {
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
        match self.0.as_ref() {
            Either::Left(left) => left.borrow().is_some(),
            Either::Right(_) => true,
        }
    }
}