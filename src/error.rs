use std::{
    collections::HashMap, error::Error as StdError, fmt, string::String as StdString, sync::Arc,
    write,
};

use gc_arena::{Collect, Gc, Rootable};
use thiserror::Error;

use crate::{
    compiler::LineNumber, Callback, CallbackReturn, Context, FromValue, Function, IntoValue,
    MetaMethod, Singleton, Table, UserData, Value,
};

#[derive(Debug, Clone, Copy, Error)]
#[error("type error, expected {expected}, found {found}")]
pub struct TypeError {
    pub expected: &'static str,
    pub found: &'static str,
}

/// An error raised directly from Lua which contains a Lua value.
///
/// Any [`Value`] can be raised as an error and it will be contained here.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct LuaError<'gc> {
    pub value: Value<'gc>,
    pub backtrace: Option<Vec<BacktraceFrame>>,
}

impl<'gc> fmt::Display for LuaError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            pretty_print_error_with_backtrace(
                f,
                &self.value.display(),
                &self.backtrace.as_deref(),
                None,
            )
        } else {
            write!(f, "{}", self.value.display())
        }
    }
}

impl<'gc> From<Value<'gc>> for LuaError<'gc> {
    fn from(value: Value<'gc>) -> Self {
        LuaError {
            value,
            backtrace: None,
        }
    }
}

/// A [`LuaError`] that is not bound to the GC context.
///
/// All primitive values (nil, booleans, integers, numbers) are represented here exactly. Strings
/// are converted *lossily* into normal Rust strings. Tables, functions, threads, and userdata are
/// stored in their *raw pointer* form.
#[derive(Debug, Clone, Error)]
pub enum ExternLuaError {
    #[error("nil")]
    Nil,
    #[error("{0}")]
    Boolean(bool),
    #[error("{0}")]
    Integer(i64),
    #[error("{0}")]
    Number(f64),
    #[error("{0}")]
    String(StdString),
    #[error("<table {0:p}>")]
    Table(*const ()),
    #[error("<function {0:p}>")]
    Function(*const ()),
    #[error("<thread {0:p}>")]
    Thread(*const ()),
    #[error("<userdata {0:p}>")]
    UserData(*const ()),
}

impl<'gc> From<Value<'gc>> for ExternLuaError {
    fn from(error: Value<'gc>) -> Self {
        match error {
            Value::Nil => ExternLuaError::Nil,
            Value::Boolean(b) => ExternLuaError::Boolean(b),
            Value::Integer(i) => ExternLuaError::Integer(i),
            Value::Number(n) => ExternLuaError::Number(n),
            Value::String(s) => ExternLuaError::String(s.display_lossy().to_string()),
            Value::Table(t) => ExternLuaError::Table(Gc::as_ptr(t.into_inner()) as *const ()),
            Value::Function(Function::Callback(c)) => {
                ExternLuaError::Function(Gc::as_ptr(c.into_inner()) as *const ())
            }
            Value::Function(Function::Closure(c)) => {
                ExternLuaError::Function(Gc::as_ptr(c.into_inner()) as *const ())
            }
            Value::Thread(t) => ExternLuaError::Thread(Gc::as_ptr(t.into_inner()) as *const ()),
            Value::UserData(u) => ExternLuaError::UserData(Gc::as_ptr(u.into_inner()) as *const ()),
        }
    }
}

// SAFETY: The pointers in `ExternLuaError` are not actually dereferenced at all, they are purely
// informational.
unsafe impl Send for ExternLuaError {}
unsafe impl Sync for ExternLuaError {}

#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
pub enum BacktraceFrame {
    Lua {
        chunk_name: StdString,
        function_name: StdString,
        line_number: LineNumber,
        args: Vec<(StdString, StdString)>,
    },
    Callback {
        name: &'static str,
    },
    Sequence,
    Internal,
}

/// A shareable, dynamically typed wrapper around a normal Rust error.
///
/// Rust errors can be caught and re-raised through Lua which allows for unrestricted sharing, so
/// this type contains its error inside an `Arc` pointer to allow for this.
#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
pub struct RuntimeError {
    pub error: Arc<anyhow::Error>,
    pub backtrace: Option<Vec<BacktraceFrame>>,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            pretty_print_error_with_backtrace(f, &self.error, &self.backtrace.as_deref(), None)
        } else {
            write!(f, "{}", self.error)
        }
    }
}

impl<E: Into<anyhow::Error>> From<E> for RuntimeError {
    fn from(err: E) -> Self {
        Self::new(err)
    }
}

impl RuntimeError {
    pub fn new(err: impl Into<anyhow::Error>) -> Self {
        Self {
            error: Arc::new(err.into()),
            backtrace: None,
        }
    }

    pub fn root_cause(&self) -> &(dyn StdError + 'static) {
        self.error.root_cause()
    }

    pub fn is<E>(&self) -> bool
    where
        E: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.error.is::<E>()
    }

    pub fn downcast<E>(&self) -> Option<&E>
    where
        E: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.error.downcast_ref::<E>()
    }
}

impl AsRef<dyn StdError + 'static> for RuntimeError {
    fn as_ref(&self) -> &(dyn StdError + 'static) {
        (*self.error).as_ref()
    }
}

/// An error that can be raised from Lua code.
///
/// This can be either a [`LuaError`] containing a Lua [`Value`], or a [`RuntimeError`] containing a
/// Rust error.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Error<'gc> {
    Lua(LuaError<'gc>),
    Runtime(RuntimeError),
}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Error::Lua(err) => write!(f, "lua error: {:#}", err),
                Error::Runtime(err) => write!(f, "runtime error: {:#}", err),
            }
        } else {
            match self {
                Error::Lua(err) => write!(f, "lua error: {}", err),
                Error::Runtime(err) => write!(f, "runtime error: {}", err),
            }
        }
    }
}

impl<'gc> From<Value<'gc>> for Error<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self::from_value(value)
    }
}

impl<'gc> From<LuaError<'gc>> for Error<'gc> {
    fn from(error: LuaError<'gc>) -> Self {
        Self::Lua(error)
    }
}

impl<'gc> From<RuntimeError> for Error<'gc> {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'gc, E: Into<anyhow::Error>> From<E> for Error<'gc> {
    fn from(error: E) -> Self {
        Self::Runtime(RuntimeError::new(error))
    }
}

impl<'gc> Error<'gc> {
    /// Turn a Lua [`Value`] into an `Error`.
    ///
    /// If the provided value is a [`UserData`] object which holds a [`RuntimeError`], then this
    /// conversion will clone the held `RuntimeError` and properly return an [`Error::Runtime`]
    /// variant. This is how Rust errors are properly transported through Lua: a `RuntimeError`
    /// which is turned into a `Value` with [`Error::to_value`] will always turn back into a
    /// `RuntimeError` error with [`Error::from_value`].
    ///
    /// If the given value is *any other* kind of Lua value, then this will return a [`LuaError`]
    /// instead.
    pub fn from_value(value: Value<'gc>) -> Self {
        if let Value::UserData(ud) = value {
            if let Ok(err) = ud.downcast_static::<RuntimeError>() {
                return Error::Runtime(err.clone());
            }
        }

        Error::Lua(value.into())
    }

    /// Convert an `Error` into a Lua value.
    ///
    /// For Lua errors, this simply returns the original Lua [`Value`] directly.
    ///
    /// For Rust errors, this will return a [`UserData`] value which holds a [`RuntimeError`]. The
    /// `UserData` object will also have a `__tostring` metamethod which prints the error properly
    /// when printed from Lua.
    pub fn to_value(&self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Error::Lua(err) => err.value,
            Error::Runtime(err) => {
                #[derive(Copy, Clone, Collect)]
                #[collect(no_drop)]
                struct UDMeta<'gc>(Table<'gc>);

                impl<'gc> Singleton<'gc> for UDMeta<'gc> {
                    fn create(ctx: Context<'gc>) -> Self {
                        let table = Table::new(&ctx);
                        table
                            .set(
                                ctx,
                                MetaMethod::ToString,
                                Callback::from_fn(&ctx, |ctx, _, mut stack| {
                                    let ud = stack.consume::<UserData>(ctx)?;
                                    let error = ud.downcast_static::<RuntimeError>()?;
                                    stack.replace(ctx, error.error.to_string());
                                    Ok(CallbackReturn::Return)
                                }),
                            )
                            .unwrap();
                        Self(table)
                    }
                }

                let ud = UserData::new_static(&ctx, err.clone());
                ud.set_metatable(&ctx, Some(ctx.singleton::<Rootable![UDMeta<'_>]>().0));
                ud.into()
            }
        }
    }

    pub fn to_extern(&self) -> ExternError {
        self.clone().into_extern()
    }

    pub fn into_extern(self) -> ExternError {
        self.into()
    }
}

impl<'gc> IntoValue<'gc> for Error<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.to_value(ctx)
    }
}

impl<'gc> FromValue<'gc> for Error<'gc> {
    fn from_value(_: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(Error::from_value(value))
    }
}

/// An [`enum@Error`] that is not bound to the GC context.
#[derive(Debug, Clone)]
pub enum ExternError {
    Lua {
        error: ExternLuaError,
        backtrace: Option<Vec<BacktraceFrame>>,
    },
    Runtime(RuntimeError),
}

impl fmt::Display for ExternError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                ExternError::Lua { error, backtrace } => {
                    write!(f, "lua error: ")?;
                    pretty_print_error_with_backtrace(f, error, &backtrace.as_deref(), None)
                }
                ExternError::Runtime(err) => {
                    write!(f, "runtime error: {:#}", err)
                }
            }
        } else {
            match self {
                ExternError::Lua { error, .. } => write!(f, "lua error: {error}"),
                ExternError::Runtime(err) => write!(f, "runtime error: {err}"),
            }
        }
    }
}

impl StdError for ExternError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            ExternError::Lua { error, .. } => Some(error),
            ExternError::Runtime(err) => Some(err.as_ref()),
        }
    }
}

impl ExternError {
    pub fn root_cause(&self) -> &(dyn StdError + 'static) {
        match self {
            ExternError::Lua { error, .. } => error,
            ExternError::Runtime(err) => err.root_cause(),
        }
    }

    pub fn pretty_print(
        &self,
        writer: &mut impl fmt::Write,
        source_map: Option<&SourceMap>,
    ) -> Result<(), fmt::Error> {
        match self {
            ExternError::Lua { error, backtrace } => {
                pretty_print_error_with_backtrace(writer, error, &backtrace.as_deref(), source_map)
            }
            ExternError::Runtime(err) => pretty_print_error_with_backtrace(
                writer,
                &err.error,
                &err.backtrace.as_deref(),
                source_map,
            ),
        }
    }
}

impl From<ExternLuaError> for ExternError {
    fn from(error: ExternLuaError) -> Self {
        Self::Lua {
            error,
            backtrace: None,
        }
    }
}

impl From<RuntimeError> for ExternError {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'gc> From<Error<'gc>> for ExternError {
    fn from(err: Error<'gc>) -> Self {
        match err {
            Error::Lua(err) => ExternError::Lua {
                error: err.value.into(),
                backtrace: err.backtrace,
            },
            Error::Runtime(e) => e.into(),
        }
    }
}

pub type SourceMap = HashMap<StdString, StdString>;

pub(crate) fn pretty_print_error_with_backtrace(
    writer: &mut impl fmt::Write,
    error: &dyn fmt::Display,
    backtrace: &Option<&[BacktraceFrame]>,
    source_map: Option<&SourceMap>,
) -> Result<(), fmt::Error> {
    write!(writer, "{}", error)?;
    if let Some(backtrace) = backtrace {
        write!(writer, "\nstack traceback:")?;

        if backtrace.is_empty() {
            write!(writer, " <no frames>")?;
        } else {
            for frame in backtrace.iter() {
                match frame {
                    BacktraceFrame::Lua {
                        chunk_name,
                        function_name,
                        line_number,
                        args,
                    } => {
                        write!(
                            writer,
                            "\n  {}:{} in {}",
                            chunk_name, line_number, function_name,
                        )?;
                        if let Some(source) = source_map.and_then(|sm| sm.get(chunk_name)) {
                            if let Some(line) = source.lines().nth(line_number.0 as usize) {
                                write!(writer, ": `{}`", line.trim())?;
                            }
                        }
                        if !args.is_empty() {
                            write!(writer, "\n    arguments:")?;
                            for (name, value) in args {
                                write!(writer, "\n      {}: {}", name, value)?;
                            }
                        }
                    }
                    BacktraceFrame::Callback { name } => {
                        write!(writer, "\n  <callback: {}>", name)?;
                    }
                    BacktraceFrame::Sequence => {
                        write!(writer, "\n  <sequence>")?;
                    }
                    BacktraceFrame::Internal => {
                        write!(writer, "\n  <???>")?;
                    }
                }
            }
            writeln!(writer)?;
        }
    }
    Ok(())
}
