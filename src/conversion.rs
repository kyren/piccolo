use std::{array, iter, ops, string::String as StdString};

use crate::{
    Callback, Closure, Context, Function, String, Table, Thread, TypeError, UserData, Value,
};

pub trait IntoValue<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc>;
}

macro_rules! impl_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> IntoValue<'gc> for $i {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    self.into()
                }
            }
        )*
    };
}
impl_into!(
    bool,
    i64,
    f64,
    String<'gc>,
    Table<'gc>,
    Function<'gc>,
    Closure<'gc>,
    Callback<'gc>,
    Thread<'gc>,
    Value<'gc>,
    UserData<'gc>,
);

macro_rules! impl_int_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> IntoValue<'gc> for $i {
                fn into_value(self, _: Context<'gc>) -> Value<'gc> {
                    Value::Integer(self.into())
                }
            }
        )*
    };
}
impl_int_into!(i8, u8, i16, u16, i32, u32);

impl<'gc> IntoValue<'gc> for f32 {
    fn into_value(self, _: Context<'gc>) -> Value<'gc> {
        Value::Number(self.into())
    }
}

macro_rules! impl_copy_into {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'a, 'gc> IntoValue<'gc> for &'a $i {
                fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
                    (*self).into_value(ctx)
                }
            }
        )*
    };
}
impl_copy_into!(
    bool,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    f32,
    f64,
    String<'gc>,
    Table<'gc>,
    Function<'gc>,
    Closure<'gc>,
    Callback<'gc>,
    Thread<'gc>,
    Value<'gc>,
    UserData<'gc>,
);

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::String(ctx.intern_static(self.as_bytes()))
    }
}

impl<'gc> IntoValue<'gc> for StdString {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::String(ctx.intern(self.as_bytes()))
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Option<T> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Some(t) => t.into_value(ctx),
            None => Value::Nil,
        }
    }
}

impl<'a, 'gc, T> IntoValue<'gc> for &'a Option<T>
where
    &'a T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Some(t) => t.into_value(ctx),
            None => Value::Nil,
        }
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Vec<T> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        let table = Table::new(&ctx);
        for (i, v) in self.into_iter().enumerate() {
            table.set(ctx, i64::try_from(i).unwrap() + 1, v).unwrap();
        }
        table.into()
    }
}

impl<'gc, 'a, T> IntoValue<'gc> for &'a [T]
where
    &'a T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        let table = Table::new(&ctx);
        for (i, v) in self.iter().enumerate() {
            table.set(ctx, i64::try_from(i).unwrap() + 1, v).unwrap();
        }
        table.into()
    }
}

impl<'gc, T, const N: usize> IntoValue<'gc> for [T; N]
where
    T: IntoValue<'gc>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        let table = Table::new(&ctx);
        for (i, v) in self.into_iter().enumerate() {
            table.set(ctx, i64::try_from(i).unwrap() + 1, v).unwrap();
        }
        table.into()
    }
}

pub trait FromValue<'gc>: Sized {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError>;
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn from_value(_: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(value)
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Option<T> {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(if value.is_nil() {
            None
        } else {
            Some(T::from_value(ctx, value)?)
        })
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Vec<T> {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        if let Value::Table(table) = value {
            (1..=table.length())
                .into_iter()
                .map(|i| table.get(ctx, i))
                .collect()
        } else {
            Err(TypeError {
                expected: "sequence",
                found: value.type_name(),
            })
        }
    }
}

impl<'gc, T: FromValue<'gc>, const N: usize> FromValue<'gc> for [T; N] {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        if let Value::Table(table) = value {
            let mut res: [Option<T>; N] = array::from_fn(|_| None);
            for i in 0..N {
                res[i] = Some(table.get(ctx, i64::try_from(i).unwrap() + 1)?);
            }
            Ok(res.map(|r| r.unwrap()))
        } else {
            Err(TypeError {
                expected: "sequence",
                found: value.type_name(),
            })
        }
    }
}

macro_rules! impl_int_from {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $i {
                #[allow(irrefutable_let_patterns)]
                fn from_value(
                    _: Context<'gc>,
                    value: Value<'gc>,
                ) -> Result<Self, TypeError> {
                    if let Some(i) = value.to_integer() {
                        #[allow(irrefutable_let_patterns)]
                        if let Ok(i) = <$i>::try_from(i) {
                            Ok(i)
                        } else {
                            Err(TypeError {
                                expected: stringify!($i),
                                found: "integer out of range",
                            })
                        }
                    } else {
                        Err(TypeError {
                            expected: stringify!($i),
                            found: value.type_name(),
                        })
                    }
                }
            }
        )*
    };
}
impl_int_from!(i64, u64, i32, u32, i16, u16, i8, u8);

macro_rules! impl_float_from {
    ($($f:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $f {
                fn from_value(
                    _: Context<'gc>,
                    value: Value<'gc>,
                ) -> Result<Self, TypeError> {
                    if let Some(n) = value.to_number() {
                        Ok(n as $f)
                    } else {
                        Err(TypeError {
                            expected: stringify!($f),
                            found: value.type_name(),
                        })
                    }
                }
            }
        )*
    };
}
impl_float_from!(f32, f64);

macro_rules! impl_from {
    ($([$e:ident $t:ty]),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $t {
                fn from_value(
                    _: Context<'gc>,
                    value: Value<'gc>,
                ) -> Result<Self, TypeError> {
                    match value {
                        Value::$e(a) => Ok(a),
                        _ => {
                            Err(TypeError {
                                expected: stringify!($e),
                                found: value.type_name(),
                            })
                        }
                    }
                }
            }
        )*
    };
}
impl_from! {
    [Boolean bool],
    [Table Table<'gc>],
    [Function Function<'gc>],
    [Thread Thread<'gc>],
    [UserData UserData<'gc>],
}

impl<'gc> FromValue<'gc> for Closure<'gc> {
    fn from_value(_: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        match value {
            Value::Function(Function::Closure(c)) => Ok(c),
            Value::Function(Function::Callback(_)) => Err(TypeError {
                expected: "Closure",
                found: "Callback",
            }),
            _ => Err(TypeError {
                expected: "Closure",
                found: value.type_name(),
            }),
        }
    }
}

impl<'gc> FromValue<'gc> for Callback<'gc> {
    fn from_value(_: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        match value {
            Value::Function(Function::Callback(c)) => Ok(c),
            Value::Function(Function::Closure(_)) => Err(TypeError {
                expected: "Callback",
                found: "Closure",
            }),
            _ => Err(TypeError {
                expected: "Callback",
                found: value.type_name(),
            }),
        }
    }
}

impl<'gc> FromValue<'gc> for String<'gc> {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        value.into_string(ctx).ok_or_else(|| TypeError {
            expected: "string",
            found: value.type_name(),
        })
    }
}

impl<'gc> FromValue<'gc> for StdString {
    fn from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, TypeError> {
        let str = String::from_value(ctx, value)?;
        let str = str.to_str().map_err(|_| TypeError {
            expected: "UTF-8 String",
            found: "non-UTF-8 String",
        })?;
        Ok(str.to_owned())
    }
}

pub trait IntoMultiValue<'gc> {
    fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>>;
}

impl<'gc, T: IntoValue<'gc>> IntoMultiValue<'gc> for T {
    fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        iter::once(self.into_value(ctx))
    }
}

pub trait FromMultiValue<'gc>: Sized {
    fn from_multi_value(
        ctx: Context<'gc>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError>;
}

impl<'gc, T: FromValue<'gc>> FromMultiValue<'gc> for T {
    fn from_multi_value(
        ctx: Context<'gc>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        T::from_value(ctx, values.next().unwrap_or(Value::Nil))
    }
}

impl<'gc, T: IntoMultiValue<'gc>, E: IntoValue<'gc>> IntoMultiValue<'gc> for Result<T, E> {
    fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        enum ResultIter<'gc, I> {
            Ok(I),
            Err(iter::Once<Value<'gc>>),
        }

        impl<'gc, I> Iterator for ResultIter<'gc, I>
        where
            I: Iterator<Item = Value<'gc>>,
        {
            type Item = Value<'gc>;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    ResultIter::Ok(i) => i.next(),
                    ResultIter::Err(i) => i.next(),
                }
            }
        }

        match self {
            Ok(v) => iter::once(true.into()).chain(ResultIter::Ok(v.into_multi_value(ctx))),
            Err(e) => {
                iter::once(false.into()).chain(ResultIter::Err(iter::once(e.into_value(ctx))))
            }
        }
    }
}

/// A marker newtype that converts to / from *multiple* Lua values.
///
/// A `Vec<T>` has [`IntoValue`] / [`FromValue`] implementations that conver to / from a [`Table`],
/// while a `Variadic<Vec<T>>` has [`IntoMultiValue`] and [`FromMultiValue`] implementations that
/// convert to / from multiple Lua values at once.
///
/// Use this to provide a variable number of arguments to a function, or to collect multiple return
/// values into a single container.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Variadic<T>(pub T);

impl<T> ops::Deref for Variadic<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ops::DerefMut for Variadic<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: IntoIterator> IntoIterator for Variadic<T> {
    type Item = T::Item;
    type IntoIter = T::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Variadic<T>
where
    &'a T: IntoIterator,
{
    type Item = <&'a T as IntoIterator>::Item;
    type IntoIter = <&'a T as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<I, T: FromIterator<I>> FromIterator<I> for Variadic<T> {
    fn from_iter<It: IntoIterator<Item = I>>(iter: It) -> Self {
        Self(T::from_iter(iter))
    }
}

impl<'gc, T: IntoIterator> IntoMultiValue<'gc> for Variadic<T>
where
    T::Item: IntoValue<'gc>,
{
    fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        self.0.into_iter().map(move |v| v.into_value(ctx))
    }
}

impl<'a, 'gc, T> IntoMultiValue<'gc> for &'a Variadic<T>
where
    &'a T: IntoIterator,
    <&'a T as IntoIterator>::Item: IntoValue<'gc>,
{
    fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        self.0.into_iter().map(move |v| v.into_value(ctx))
    }
}

impl<'gc, I: FromValue<'gc>> FromMultiValue<'gc> for Variadic<Vec<I>> {
    fn from_multi_value(
        ctx: Context<'gc>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        values.map(|v| I::from_value(ctx, v)).collect()
    }
}

impl<'gc, I: FromValue<'gc>, const N: usize> FromMultiValue<'gc> for Variadic<[I; N]> {
    fn from_multi_value(
        ctx: Context<'gc>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        let mut res: [Option<I>; N] = array::from_fn(|_| None);
        for i in 0..N {
            res[i] = Some(I::from_value(ctx, values.next().unwrap_or(Value::Nil))?);
        }

        Ok(Self(res.map(|v| v.unwrap())))
    }
}

macro_rules! impl_tuple {
    ($($name:ident),* $(,)?) => (
        impl<'gc, $($name,)*> IntoMultiValue<'gc> for ($($name,)*)
        where
            $($name: IntoMultiValue<'gc>,)*
        {
            #[allow(unused_variables)]
            #[allow(unused_mut)]
            #[allow(non_snake_case)]
            fn into_multi_value(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
                let ($($name,)*) = self;
                let i = iter::empty();
                $(
                    let i = i.chain($name.into_multi_value(ctx));
                )*
                i
            }
        }

        impl<'gc, $($name,)*> FromMultiValue<'gc> for ($($name,)*)
            where $($name: FromMultiValue<'gc>,)*
        {
            #[allow(unused_variables)]
            #[allow(unused_mut)]
            #[allow(non_snake_case)]
            fn from_multi_value(
                ctx: Context<'gc>,
                mut values: impl Iterator<Item = Value<'gc>>,
            ) -> Result<Self, TypeError> {
                $(let $name = FromMultiValue::from_multi_value(ctx, &mut values)?;)*
                Ok(($($name,)*))
            }
        }
    );
}

macro_rules! smaller_tuples_too {
    ($m: ident, $ty: ident) => {
        $m!{}
        $m!{$ty}
    };

    ($m: ident, $ty: ident, $($tt: ident),*) => {
        smaller_tuples_too!{$m, $($tt),*}
        $m!{$ty, $($tt),*}
    };
}

smaller_tuples_too!(impl_tuple, P, O, N, M, L, K, J, I, H, G, F, E, D, C, B, A);
