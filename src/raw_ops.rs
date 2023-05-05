use gc_arena::MutationContext;

use crate::{
    lexer::{read_float, read_hex_float},
    String, Value,
};

/// Lua `nil` and `false` are false, anything else is true.
pub fn to_bool(v: Value) -> bool {
    match v {
        Value::Nil => false,
        Value::Boolean(false) => false,
        _ => true,
    }
}

/// Interprets Numbers, Integers, and Strings as a Number, if possible.
pub fn to_number(v: Value) -> Option<f64> {
    match v {
        Value::Integer(a) => Some(a as f64),
        Value::Number(a) => Some(a),
        Value::String(a) => {
            if let Some(f) = read_hex_float(&a) {
                Some(f)
            } else {
                read_float(&a)
            }
        }
        _ => None,
    }
}

/// Interprets Numbers, Integers, and Strings as an Integer, if possible.
pub fn to_integer(v: Value) -> Option<i64> {
    match v {
        Value::Integer(a) => Some(a),
        Value::Number(a) => {
            if ((a as i64) as f64) == a {
                Some(a as i64)
            } else {
                None
            }
        }
        Value::String(a) => match if let Some(f) = read_hex_float(&a) {
            Some(f)
        } else {
            read_float(&a)
        } {
            Some(f) => {
                if ((f as i64) as f64) == f {
                    Some(f as i64)
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Interprets Numbers, Integers, and Strings as a String, if possible.
pub fn to_string<'gc>(mc: MutationContext<'gc, '_>, v: Value<'gc>) -> Option<String<'gc>> {
    match v {
        Value::Integer(a) => Some(String::concat(mc, &[Value::Integer(a)]).unwrap()),
        Value::Number(a) => Some(String::concat(mc, &[Value::Number(a)]).unwrap()),
        Value::String(a) => Some(a),
        _ => None,
    }
}

pub fn not<'gc>(v: Value<'gc>) -> Value<'gc> {
    Value::Boolean(!to_bool(v))
}

// Mathematical operators

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_add(b)))
    } else {
        Some(Value::Number(to_number(lhs)? + to_number(rhs)?))
    }
}

pub fn subtract<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_sub(b)))
    } else {
        Some(Value::Number(to_number(lhs)? - to_number(rhs)?))
    }
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_mul(b)))
    } else {
        Some(Value::Number(to_number(lhs)? * to_number(rhs)?))
    }
}

/// This operation always returns a Number, even when called with Integer arguments.
pub fn float_divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Number(to_number(lhs)? / to_number(rhs)?))
}

/// This operation returns an Integer only if both arguments are Integers. Rounding is towards
/// negative infinity.
pub fn floor_divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        if b == 0 {
            None
        } else {
            Some(Value::Integer(a.wrapping_div(b)))
        }
    } else {
        Some(Value::Number((to_number(lhs)? / to_number(rhs)?).floor()))
    }
}

/// Computes the Lua modulus (`%`) operator. This is unlike Rust's `%` operator which computes
/// the remainder.
pub fn modulo<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        if b == 0 {
            None
        } else {
            Some(Value::Integer(((a % b) + b) % b))
        }
    } else {
        let (a, b) = (to_number(lhs)?, to_number(rhs)?);
        Some(Value::Number(((a % b) + b) % b))
    }
}

/// This operation always returns a Number, even when called with Integer arguments.
pub fn exponentiate<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Number(to_number(lhs)?.powf(to_number(rhs)?)))
}

pub fn negate<'gc>(lhs: Value<'gc>) -> Option<Value<'gc>> {
    match lhs {
        Value::Integer(a) => Some(Value::Integer(a.wrapping_neg())),
        Value::Number(a) => Some(Value::Number(-a)),
        _ => None,
    }
}

// Bitwise operators

pub fn bitwise_not<'gc>(v: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(!to_integer(v)?))
}

pub fn bitwise_and<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(to_integer(lhs)? & to_integer(rhs)?))
}

pub fn bitwise_or<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(to_integer(lhs)? | to_integer(rhs)?))
}

pub fn bitwise_xor<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(to_integer(lhs)? ^ to_integer(rhs)?))
}

pub fn shift_left<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(to_integer(lhs)? << to_integer(rhs)?))
}

pub fn shift_right<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(
        (to_integer(lhs)? as u64 >> to_integer(rhs)? as u64) as i64,
    ))
}

// Comparison operators

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(a < b)
    } else if let (Value::String(a), Value::String(b)) = (lhs, rhs) {
        Some(a.as_bytes() < b.as_bytes())
    } else {
        Some(to_number(lhs)? < to_number(rhs)?)
    }
}

pub fn less_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(a <= b)
    } else if let (Value::String(a), Value::String(b)) = (lhs, rhs) {
        Some(a.as_bytes() <= b.as_bytes())
    } else {
        Some(to_number(lhs)? <= to_number(rhs)?)
    }
}
