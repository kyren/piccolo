use std::{
    array, iter,
    ops::{Deref, DerefMut},
};

use gc_arena::MutationContext;
use smallvec::SmallVec;

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

pub trait FromValue<'gc>: Sized {
    fn from_value(mc: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError>;
}

impl<'gc, T: From<Value<'gc>>> FromValue<'gc> for T {
    fn from_value(_: MutationContext<'gc, '_>, value: Value<'gc>) -> Result<Self, TypeError> {
        Ok(T::from(value))
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

type SmallArray<T> = [T; 4];

#[derive(Default)]
pub struct MultiValue<'gc>(SmallVec<SmallArray<Value<'gc>>>);

impl<'gc> FromIterator<Value<'gc>> for MultiValue<'gc> {
    fn from_iter<I: IntoIterator<Item = Value<'gc>>>(iter: I) -> Self {
        let mut v = SmallVec::from_iter(iter);
        v.reverse();
        MultiValue(v)
    }
}

impl<'gc> IntoIterator for MultiValue<'gc> {
    type Item = Value<'gc>;
    type IntoIter = iter::Rev<<SmallVec<SmallArray<Value<'gc>>> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}

impl<'a, 'gc> IntoIterator for &'a MultiValue<'gc> {
    type Item = &'a Value<'gc>;
    type IntoIter = iter::Rev<<&'a [Value<'gc>] as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.as_slice().into_iter().rev()
    }
}

pub trait IntoMultiValue<'gc> {
    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> MultiValue<'gc>;
}

impl<'gc, T: IntoValue<'gc>> IntoMultiValue<'gc> for T {
    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> MultiValue<'gc> {
        MultiValue::from_iter([self.into_value(mc)])
    }
}

impl<'gc, T: IntoValue<'gc>, const N: usize> IntoMultiValue<'gc> for [T; N] {
    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> MultiValue<'gc> {
        MultiValue::from_iter(self.map(|v| v.into_value(mc)))
    }
}

impl<'gc> IntoMultiValue<'gc> for MultiValue<'gc> {
    fn into_multi_value(self, _: MutationContext<'gc, '_>) -> MultiValue<'gc> {
        self
    }
}

pub trait FromMultiValue<'gc>: Sized {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: MultiValue<'gc>,
    ) -> Result<Self, TypeError>;
}

impl<'gc, T: FromValue<'gc>> FromMultiValue<'gc> for T {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        mut values: MultiValue<'gc>,
    ) -> Result<Self, TypeError> {
        T::from_value(mc, values.0.pop().unwrap_or(Value::Nil))
    }
}

impl<'gc, T: FromValue<'gc>, const N: usize> FromMultiValue<'gc> for [T; N] {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: MultiValue<'gc>,
    ) -> Result<Self, TypeError> {
        let mut res: [Option<T>; N] = array::from_fn(|_| None);
        for (i, v) in values.into_iter().enumerate() {
            if i >= N {
                break;
            }
            res[i] = Some(T::from_value(mc, v)?);
        }

        for v in &res {
            if v.is_none() {
                return Err(TypeError {
                    expected: "N elements",
                    found: "less than N elements",
                });
            }
        }

        Ok(res.map(|v| v.unwrap()))
    }
}

impl<'gc> FromMultiValue<'gc> for MultiValue<'gc> {
    fn from_multi_value(
        _: MutationContext<'gc, '_>,
        values: MultiValue<'gc>,
    ) -> Result<Self, TypeError> {
        Ok(values)
    }
}

#[derive(Clone)]
pub struct Variadic<T>(SmallVec<SmallArray<T>>);

impl<T> Variadic<T> {
    /// Creates an empty `Variadic` wrapper containing no values.
    pub fn new() -> Variadic<T> {
        Variadic(SmallVec::new())
    }
}

impl<T> Default for Variadic<T> {
    fn default() -> Variadic<T> {
        Variadic::new()
    }
}

impl<T> FromIterator<T> for Variadic<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Variadic(SmallVec::from_iter(iter))
    }
}

impl<T> IntoIterator for Variadic<T> {
    type Item = T;
    type IntoIter = <SmallVec<SmallArray<T>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> Deref for Variadic<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Variadic<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'gc, T: IntoValue<'gc>> IntoMultiValue<'gc> for Variadic<T> {
    fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> MultiValue<'gc> {
        self.into_iter().map(|t| t.into_value(mc)).collect()
    }
}

impl<'gc, T: FromValue<'gc>> FromMultiValue<'gc> for Variadic<T> {
    fn from_multi_value(
        mc: MutationContext<'gc, '_>,
        values: MultiValue<'gc>,
    ) -> Result<Self, TypeError> {
        values.into_iter().map(|v| T::from_value(mc, v)).collect()
    }
}

macro_rules! impl_tuple {
    () => (
        impl<'gc> IntoMultiValue<'gc> for () {
            fn into_multi_value(self, _: MutationContext<'gc, '_>) -> MultiValue<'gc> {
                MultiValue::default()
            }
        }

        impl<'gc> FromMultiValue<'gc> for () {
            fn from_multi_value(
                _: MutationContext<'gc, '_>,
                _: MultiValue<'gc>,
            ) -> Result<Self, TypeError> {
                Ok(())
            }
        }
    );

    ($last:ident $(,$name:ident)* $(,)?) => (
        impl<'gc, $($name,)* $last> IntoMultiValue<'gc> for ($($name,)* $last,)
            where $($name: IntoValue<'gc>,)*
                  $last: IntoMultiValue<'gc>
        {
            #[allow(unused_mut)]
            #[allow(non_snake_case)]
            fn into_multi_value(self, mc: MutationContext<'gc, '_>) -> MultiValue<'gc> {
                let ($($name,)* $last,) = self;

                let mut results = $last.into_multi_value(mc);
                push_reverse!(results, $($name.into_value(mc),)*);
                results
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
                mut values: MultiValue<'gc>,
            ) -> Result<Self, TypeError> {
                $(let $name = values.0.pop().unwrap_or(Value::Nil);)*
                let $last = FromMultiValue::from_multi_value(mc, values)?;
                Ok(($(FromValue::from_value(mc, $name)?,)* $last,))
            }
        }
    );
}

macro_rules! push_reverse {
    ($multi_value:expr, $first:expr, $($rest:expr,)*) => (
        push_reverse!($multi_value, $($rest,)*);
        $multi_value.0.push($first);
    );

    ($multi_value:expr, $first:expr) => (
        $multi_value.0.push($first);
    );

    ($multi_value:expr,) => ();
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
