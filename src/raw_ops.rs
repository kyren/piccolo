use crate::Value;

// Mathematical operators

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_add(b)))
    } else {
        Some(Value::Number(lhs.to_number()? + rhs.to_number()?))
    }
}

pub fn subtract<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_sub(b)))
    } else {
        Some(Value::Number(lhs.to_number()? - rhs.to_number()?))
    }
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(Value::Integer(a.wrapping_mul(b)))
    } else {
        Some(Value::Number(lhs.to_number()? * rhs.to_number()?))
    }
}

/// This operation always returns a Number, even when called with Integer arguments.
pub fn float_divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Number(lhs.to_number()? / rhs.to_number()?))
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
        Some(Value::Number((lhs.to_number()? / rhs.to_number()?).floor()))
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
        let (a, b) = (lhs.to_number()?, rhs.to_number()?);
        Some(Value::Number(((a % b) + b) % b))
    }
}

/// This operation always returns a Number, even when called with Integer arguments.
pub fn exponentiate<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Number(lhs.to_number()?.powf(rhs.to_number()?)))
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
    Some(Value::Integer(!v.to_integer()?))
}

pub fn bitwise_and<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(lhs.to_integer()? & rhs.to_integer()?))
}

pub fn bitwise_or<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(lhs.to_integer()? | rhs.to_integer()?))
}

pub fn bitwise_xor<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(lhs.to_integer()? ^ rhs.to_integer()?))
}

pub fn shift_left<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(lhs.to_integer()? << rhs.to_integer()?))
}

pub fn shift_right<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(Value::Integer(
        (lhs.to_integer()? as u64 >> rhs.to_integer()? as u64) as i64,
    ))
}

// Comparison operators

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(a < b)
    } else if let (Value::String(a), Value::String(b)) = (lhs, rhs) {
        Some(a.as_bytes() < b.as_bytes())
    } else {
        Some(lhs.to_number()? < rhs.to_number()?)
    }
}

pub fn less_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    if let (Value::Integer(a), Value::Integer(b)) = (lhs, rhs) {
        Some(a <= b)
    } else if let (Value::String(a), Value::String(b)) = (lhs, rhs) {
        Some(a.as_bytes() <= b.as_bytes())
    } else {
        Some(lhs.to_number()? <= rhs.to_number()?)
    }
}
