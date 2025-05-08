use std::{cell::RefCell, fs::File, io::{self, Cursor, Read, Seek, Write}, rc::Rc};
use crate::{Context, Error, IntoValue, Value};

use super::std_file_kind::StdFileKind;
use either::Either;
use gc_arena::Collect;

fn read_from_any<'gc, W: Write + Read + Seek>(ctx: Context<'gc>, file: &mut W, format: &str, position: &mut usize) -> Result<Option<Value<'gc>>, Error<'gc>> {
    fn seek_read<'gc, W: Write + Read + Seek>(file: &mut W, position: &mut usize, buf: &mut [u8]) -> io::Result<usize> {
        file.seek(io::SeekFrom::Start(*position as u64))?;
        match file.read(buf) {
            Ok(n) => {
                *position += n;
                Ok(n)
            }
            Err(err) => Err(err)
        }
    }

    match format {
        "l" => {
            let mut buf = Vec::new();
            let mut byte = [0u8; 1];

            loop {
                match seek_read(file, position, &mut byte) {
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
                            match seek_read(file, position, &mut next_byte) {
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
                    Err(err) => return Err(err.to_string().into_value(ctx).into()),
                }
            }

            Ok(Some(ctx.intern(&buf).into()))
        }

        "L" => {
            let mut buf = Vec::new();
            let mut byte = [0u8; 1];

            loop {
                match seek_read(file, position, &mut byte) {
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
                            match seek_read(file, position, &mut next_byte) {
                                Ok(1) => {
                                    if next_byte[0] == b'\n' {
                                        *position += 1;
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
                    Err(err) => return Err(err.to_string().into_value(ctx).into()),
                }
            }

            Ok(Some(ctx.intern(&buf).into()))
        }

        "a" => {
            let mut buf = Vec::new();
            match file.read_to_end(&mut buf) {
                Ok(0) => Ok(Some(ctx.intern(&[]).into())),
                Ok(n) => {
                    *position += n;
                    Ok(Some(ctx.intern(&buf).into()))
                }
                Err(err) => Err(err.to_string().into_value(ctx).into()),
            }
        }

        "n" => {
            let mut buf = Vec::new();
            let mut byte = [0u8; 1];
            let mut has_digit = false;

            loop {
                match seek_read(file, position, &mut byte) {
                    Ok(0) => return Ok(None),
                    Ok(_) => {
                        if byte[0].is_ascii_whitespace() {
                            continue;
                        }
                        buf.push(byte[0]);
                        has_digit |= byte[0].is_ascii_digit();
                        break;
                    }
                    Err(e) => return Err(e.to_string().into_value(ctx).into()),
                }
            }

            loop {
                match seek_read(file, position, &mut byte) {
                    Ok(0) => break,
                    Ok(n) => {
                        *position += n;
                        if matches!(byte[0], b'0'..=b'9' | b'.' | b'-' | b'+' | b'e' | b'E')
                        {
                            buf.push(byte[0]);
                            if byte[0].is_ascii_digit() {
                                has_digit = true;
                            }
                        } else {
                            break;
                        }
                    }
                    Err(err) => return Err(err.to_string().into_value(ctx).into()),
                }
            }

            if !has_digit {
                return Ok(None);
            }

            let s = std::string::String::from_utf8_lossy(&buf);
            match s.parse::<i64>().ok().map(Value::from)
                .or_else(|| s.parse::<f64>().ok().map(Value::from)) 
            {
                Some(value) => Ok(Some(value)),
                None => Ok(None)
            }
        }

        _ => {
            if let Ok(n) = format.parse::<usize>() {
                if n == 0 {
                    let mut empty_buf: [u8; 0] = [];
                    file.seek(io::SeekFrom::Start(*position as u64))?;
                    match file.read_exact(&mut empty_buf) {
                        Ok(_) => return Ok(Some(ctx.intern(&[]).into())),
                        Err(_) => return Ok(None),
                    }
                }

                let mut buf = vec![0u8; n];
                match seek_read(file, position, &mut buf) {
                    Ok(0) => Ok(None),
                    Ok(bytes_read) => {
                        if bytes_read == 0 {
                            Ok(None)
                        } else if bytes_read < n {
                            Ok(None)
                        } else {
                            Ok(Some(ctx.intern(&buf).into()))
                        }
                    }
                    Err(err) => Err(err.to_string().into_value(ctx).into()),
                }
            } else {
                Err("invalid format".into_value(ctx).into())
            }
        }
    }
}

#[derive(Collect, Clone)]
#[collect(require_static)]
pub struct IoFile {
    inner: Rc<Either<RefCell<Option<File>>, StdFileKind>>,
    read_bytes: RefCell<usize>,
}

impl IoFile {
    pub fn new(file: File) -> Self {
        Self {
            inner: Rc::new(Either::Left(RefCell::new(Some(file)))),
            read_bytes: RefCell::new(0)
        }
    }
    pub fn stdin() -> Self {
        Self {
            inner: Rc::new(Either::Right(StdFileKind::Stdin)),
            read_bytes: RefCell::new(0)
        }
    }
    pub fn stdout() -> Self {
        Self {
            inner: Rc::new(Either::Right(StdFileKind::Stdout)),
            read_bytes: RefCell::new(0)
        }
    }
    pub fn stderr() -> Self {
        Self {
            inner: Rc::new(Either::Right(StdFileKind::Stderr)),
            read_bytes: RefCell::new(0)
        }
    }
    pub fn is_std(&self) -> bool {
        matches!(self.inner.as_ref(), Either::Right(_))
    }
    pub fn close(&self) -> Result<(), io::Error> {
        if self.is_std() {
            Ok(())
        } else {
            let Either::Left(left) = self.inner.as_ref() else {
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
        match self.inner() {
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
        match self.inner() {
            Either::Left(left) => {
                let mut file = left.borrow_mut();
                let file = match *file {
                    Some(ref mut file) => file,
                    None => return Err("attempt to use a closed file".into_value(ctx).into()),
                };
                let mut read_bytes = self.read_bytes.borrow_mut();
                read_from_any(ctx, file, format, &mut read_bytes)
            }
            Either::Right(kind) => match kind {
                StdFileKind::Stdin => {
                    let mut read_bytes = self.read_bytes.borrow_mut();
                    let mut stdin = io::stdin();
                    let mut buf = Vec::new();
                    stdin.read_exact(&mut buf)?;
                    let mut cursor = Cursor::new(buf);
                    read_from_any(ctx, &mut cursor, format, &mut read_bytes)
                }
                _ => Err("attempt to read from output file".into_value(ctx).into()),
            },
        }
    }
    pub fn is_some(&self) -> bool {
        match self.inner.as_ref() {
            Either::Left(left) => left.borrow().is_some(),
            Either::Right(_) => true,
        }
    }
    pub fn inner(&self) -> &Either<RefCell<Option<File>>, StdFileKind> {
        self.inner.as_ref()
    }
}