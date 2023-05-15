use std::{array, iter, ops, string::String as StdString, vec};

use gc_arena::MutationContext;

use crate::{AnyCallback, AnyUserData, Closure, Function, String, Table, Thread, TypeError, Value};

pub trait IntoValue<'gc> {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc>;
}

impl<'gc, T> IntoValue<'gc> for T
where
    T: Into<Value<'gc>>,
{
    fn into_value(self, _mc: MutationContext<'gc, '_>) -> Value<'gc> {
        self.into()
    }
}

impl<'gc> IntoValue<'gc> for &'static str {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        Value::String(String::from_static(mc, self.as_bytes()))
    }
}

impl<'gc> IntoValue<'gc> for StdString {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        Value::String(String::from_slice(mc, self.as_bytes()))
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Option<T> {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        match self {
            Some(t) => t.into_value(mc),
            None => Value::Nil,
        }
    }
}

impl<'gc, T: IntoValue<'gc>> IntoValue<'gc> for Vec<T> {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        let table = Table::new(mc);
        for (i, v) in self.into_iter().enumerate() {
            table.set(mc, i64::try_from(i).unwrap() + 1, v).unwrap();
        }
        table.into()
    }
}

impl<'gc, 'a, T: IntoValue<'gc> + Copy> IntoValue<'gc> for &'a Vec<T> {
    fn into_value(self, mc: MutationContext<'gc, '_>) -> Value<'gc> {
        let table = Table::new(mc);
        for (i, v) in self.iter().copied().enumerate() {
            table.set(mc, i64::try_from(i).unwrap() + 1, v).unwrap();
        }
        table.into()
    }
}

pub trait FromValue<'gc>: Sized {
    fn from_value(mc: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError>;
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn from_value(_: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(value)
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Option<T> {
    fn from_value(mc: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(if value.is_nil() {
            None
        } else {
            Some(T::from_value(mc, value)?)
        })
    }
}

impl<'gc, T: FromValue<'gc>> FromValue<'gc> for Vec<T> {
    fn from_value(mc: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
        if let Value::Table(table) = value {
            (1..=table.length())
                .into_iter()
                .map(|i| T::from_value(mc, table.get(mc, i)))
                .collect()
        } else {
            Err(TypeError {
                expected: "sequence table",
                found: value.type_name(),
            })
        }
    }
}

macro_rules! impl_int_from {
    ($($i:ty),* $(,)?) => {
        $(
            impl<'gc> FromValue<'gc> for $i {
                fn from_value(
                    _: MutationContext<'gc, '_>,
                    value: Value<'gc>,
                ) -> Result<Self, TypeError> {
                    if let Some(i) = value.to_integer() {
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
                    _: MutationContext<'gc, '_>,
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
                    _: MutationContext<'gc, '_>,
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
    [String String<'gc>],
    [Table Table<'gc>],
    [Function Function<'gc>],
    [Thread Thread<'gc>],
    [UserData AnyUserData<'gc>],
}

impl<'gc> FromValue<'gc> for Closure<'gc> {
    fn from_value(_: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
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

impl<'gc> FromValue<'gc> for AnyCallback<'gc> {
    fn from_value(_: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
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

pub trait IntoMultiValue<'gc> {
    type Iter: Iterator<Item = Value<'gc>>;

    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> Self::Iter;
}

impl<'gc, T: IntoValue<'gc>> IntoMultiValue<'gc> for T {
    type Iter = iter::Once<Value<'gc>>;

    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> Self::Iter {
        iter::once(self.into_value(mc))
    }
}

impl<'gc, T: IntoValue<'gc>, const N: usize> IntoMultiValue<'gc> for [T; N] {
    type Iter = <[Value<'gc>; N] as IntoIterator>::IntoIter;

    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> Self::Iter {
        let vals = self.map(|v| v.into_value(mc));
        vals.into_iter()
    }
}

pub trait FromMultiValue<'gc>: Sized {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError>;
}

impl<'gc, T: FromValue<'gc>> FromMultiValue<'gc> for T {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        T::from_value(mc, values.next().unwrap_or(Value::Nil))
    }
}

impl<'gc, T: FromValue<'gc>, const N: usize> FromMultiValue<'gc> for [T; N] {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        let mut values = values.fuse();
        let mut res: [Option<T>; N] = array::from_fn(|_| None);
        for i in 0..N {
            res[i] = Some(T::from_value(mc, values.next().unwrap_or(Value::Nil))?);
        }

        Ok(res.map(|v| v.unwrap()))
    }
}

pub struct Variadic<T>(pub Vec<T>);

impl<T> ops::Deref for Variadic<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ops::DerefMut for Variadic<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> IntoIterator for Variadic<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Variadic<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<T> FromIterator<T> for Variadic<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'gc, T: IntoValue<'gc>> IntoMultiValue<'gc> for Variadic<T> {
    type Iter = vec::IntoIter<Value<'gc>>;

    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> Self::Iter {
        self.into_iter()
            .map(|t| t.into_value(mc))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

impl<'gc, T: FromValue<'gc>> FromMultiValue<'gc> for Variadic<T> {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, TypeError> {
        values.map(|v| T::from_value(mc, v)).collect()
    }
}

macro_rules! impl_tuple {
    () => (
        impl<'gc> IntoMultiValue<'gc> for () {
            type Iter = iter::Empty<Value<'gc>>;

            fn into_multi_value(self, _: MutationContext<'gc, '_>) -> Self::Iter {
                iter::empty()
            }
        }

        impl<'gc> FromMultiValue<'gc> for () {
            fn from_multi_value(
                _: MutationContext<'gc, '_>,
                _: impl Iterator<Item = Value<'gc>>,
            ) -> Result<Self, TypeError> {
                Ok(())
            }
        }
    );

    ($last:ident $(,$name:ident)* $(,)?) => (
        impl<'gc, $($name,)* $last> IntoMultiValue<'gc> for ($($name,)* $last,)
        where
            $($name: IntoValue<'gc>,)*
            $last: IntoMultiValue<'gc>,
        {
            type Iter = vec::IntoIter<Value<'gc>>;

            #[allow(non_snake_case)]
            fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> Self::Iter {
                let ($($name,)* $last,) = self;
                let mut results = Vec::new();
                $(results.push($name.into_value(mc));)*
                results.extend($last.into_multi_value(mc));
                results.into_iter()
            }
        }

        impl<'gc, $($name,)* $last> FromMultiValue<'gc> for ($($name,)* $last,)
            where $($name: FromValue<'gc>,)*
                  $last: FromMultiValue<'gc>
        {
            #[allow(unused_mut)]
            #[allow(non_snake_case)]
            fn from_multi_value(
                mc: MutationContext<'gc, '_>,
                mut values: impl Iterator<Item = Value<'gc>>,
            ) -> Result<Self, TypeError> {
                $(let $name = FromValue::from_value(mc, values.next().unwrap_or(Value::Nil))?;)*
                let $last = FromMultiValue::from_multi_value(mc, values)?;
                Ok(($($name,)* $last,))
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
