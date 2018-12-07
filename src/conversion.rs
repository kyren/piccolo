use failure::{err_msg, Error};
use num_traits::cast;

use crate::value::Value;

pub trait FromLua: Sized {
    fn from_lua<'gc>(value: Value<'gc>) -> Result<Self, Error>;
}

macro_rules! convert_int {
    ($x:ty) => {
        impl FromLua for $x {
            fn from_lua<'gc>(value: Value<'gc>) -> Result<Self, Error> {
                let v = match value {
                    Value::Integer(i) => cast(i),
                    Value::Number(n) => cast(n),
                    _ => None,
                };

                v.ok_or_else(|| err_msg("cannot cast to int"))
            }
        }
    };
}

convert_int!(i8);
convert_int!(u8);
convert_int!(i16);
convert_int!(u16);
convert_int!(i32);
convert_int!(u32);
convert_int!(i64);
convert_int!(u64);
convert_int!(isize);
convert_int!(usize);
