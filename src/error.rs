use std::{error::Error as StdError, fmt, string::String as StdString, sync::Arc};

use gc_arena::{Collect, Rootable};
use thiserror::Error;

use crate::{
    AnyCallback, AnyUserData, CallbackReturn, Context, Singleton, Table, UserDataError, Value,
};

#[derive(Debug, Clone, Copy, Error)]
#[error("type error, expected {expected}, found {found}")]
pub struct TypeError {
    pub expected: &'static str,
    pub found: &'static str,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct LuaError<'gc>(pub Value<'gc>);

impl<'gc> fmt::Display for LuaError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'gc> From<Value<'gc>> for LuaError<'gc> {
    fn from(error: Value<'gc>) -> Self {
        LuaError(error)
    }
}

impl<'gc> LuaError<'gc> {
    pub fn to_static(self) -> StaticLuaError {
        self.into()
    }
}

#[derive(Debug, Clone, Error)]
#[error("{0}")]
pub struct StaticLuaError(StdString);

impl<'gc> From<LuaError<'gc>> for StaticLuaError {
    fn from(error: LuaError<'gc>) -> Self {
        Self(error.to_string())
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
pub struct RuntimeError(pub Arc<anyhow::Error>);

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<E: Into<anyhow::Error>> From<E> for RuntimeError {
    fn from(err: E) -> Self {
        Self(Arc::new(err.into()))
    }
}

impl RuntimeError {
    pub fn root_cause(&self) -> &(dyn StdError + 'static) {
        self.0.root_cause()
    }

    pub fn is<E>(&self) -> bool
    where
        E: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.0.is::<E>()
    }

    pub fn downcast<E>(&self) -> Option<&E>
    where
        E: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.0.downcast_ref::<E>()
    }
}

impl AsRef<dyn StdError + 'static> for RuntimeError {
    fn as_ref(&self) -> &(dyn StdError + 'static) {
        (*self.0).as_ref()
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Error<'gc> {
    Lua(LuaError<'gc>),
    Runtime(RuntimeError),
}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lua(err) => write!(f, "lua error: {}", err),
            Error::Runtime(err) => write!(f, "runtime error: {}", err),
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

impl<'gc, E> From<E> for Error<'gc>
where
    E: Into<anyhow::Error>,
{
    fn from(error: E) -> Self {
        Self::Runtime(RuntimeError::from(error))
    }
}

impl<'gc> Error<'gc> {
    pub fn from_value(value: Value<'gc>) -> Self {
        if let Value::UserData(ud) = value {
            match ud.read_static::<RuntimeError>() {
                Ok(err) => return Error::Runtime(err.clone()),
                Err(UserDataError::BorrowError) => {
                    #[derive(Debug, Error)]
                    #[error("RustError borrowed mutably, cannot lift out of Lua value")]
                    struct LiftRustError;

                    return LiftRustError.into();
                }
                Err(UserDataError::WrongType) => {}
            }
        }

        Error::Lua(value.into())
    }

    pub fn to_value(&self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Error::Lua(err) => err.0,
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
                                "__tostring",
                                AnyCallback::from_fn(&ctx, |ctx, stack| {
                                    let ud = stack.consume::<AnyUserData>(ctx)?;
                                    let error = ud.read_static::<RuntimeError>()?;
                                    stack.replace(ctx, error.to_string());
                                    Ok(CallbackReturn::Return)
                                }),
                            )
                            .unwrap();
                        Self(table)
                    }
                }

                let ud = AnyUserData::new_static(&ctx, err.clone());
                ud.set_metatable(
                    &ctx,
                    Some(ctx.state.registry.singleton::<Rootable![UDMeta<'_>]>(ctx).0),
                );
                ud.into()
            }
        }
    }

    pub fn to_static(&self) -> StaticError {
        self.clone().into_static()
    }

    pub fn into_static(self) -> StaticError {
        self.into()
    }
}

#[derive(Debug, Clone)]
pub enum StaticError {
    Lua(StaticLuaError),
    Runtime(RuntimeError),
}

impl fmt::Display for StaticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StaticError::Lua(err) => write!(f, "lua error: {err}"),
            StaticError::Runtime(err) => write!(f, "runtime error: {err}"),
        }
    }
}

impl StdError for StaticError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            StaticError::Lua(err) => Some(err),
            StaticError::Runtime(err) => Some(err.as_ref()),
        }
    }
}

impl From<StaticLuaError> for StaticError {
    fn from(error: StaticLuaError) -> Self {
        Self::Lua(error)
    }
}

impl From<RuntimeError> for StaticError {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'gc> From<Error<'gc>> for StaticError {
    fn from(err: Error<'gc>) -> Self {
        match err {
            Error::Lua(err) => err.to_static().into(),
            Error::Runtime(e) => e.into(),
        }
    }
}
