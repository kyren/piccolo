use crate::Value;

// TODO: This module should be entirely replaced by `meta_ops` as they are added.

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.add(&rhs.to_constant()?)?.into())
}

pub fn subtract<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.subtract(&rhs.to_constant()?)?.into())
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.multiply(&rhs.to_constant()?)?.into())
}

pub fn float_divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.float_divide(&rhs.to_constant()?)?.into())
}

pub fn floor_divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.floor_divide(&rhs.to_constant()?)?.into())
}

pub fn modulo<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.modulo(&rhs.to_constant()?)?.into())
}

pub fn exponentiate<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.exponentiate(&rhs.to_constant()?)?.into())
}

pub fn negate<'gc>(lhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.negate()?.into())
}

pub fn bitwise_not<'gc>(v: Value<'gc>) -> Option<Value<'gc>> {
    Some(v.to_constant()?.bitwise_not()?.into())
}

pub fn bitwise_and<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.bitwise_and(&rhs.to_constant()?)?.into())
}

pub fn bitwise_or<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.bitwise_or(&rhs.to_constant()?)?.into())
}

pub fn bitwise_xor<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.bitwise_xor(&rhs.to_constant()?)?.into())
}

pub fn shift_left<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.shift_left(&rhs.to_constant()?)?.into())
}

pub fn shift_right<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    Some(lhs.to_constant()?.shift_right(&rhs.to_constant()?)?.into())
}

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    Some(lhs.to_constant()?.less_than(&rhs.to_constant()?)?.into())
}

pub fn less_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    Some(lhs.to_constant()?.less_equal(&rhs.to_constant()?)?.into())
}

pub fn equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> bool {
    match (lhs, rhs) {
        (Value::Nil, Value::Nil) => true,
        (Value::Nil, _) => false,

        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Boolean(_), _) => false,

        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Integer(a), Value::Number(b)) => a as f64 == b,
        (Value::Integer(_), _) => false,

        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Number(a), Value::Integer(b)) => b as f64 == a,
        (Value::Number(_), _) => false,

        (Value::String(a), Value::String(b)) => a == b,
        (Value::String(_), _) => false,

        (Value::Table(a), Value::Table(b)) => a == b,
        (Value::Table(_), _) => false,

        (Value::Function(a), Value::Function(b)) => a == b,
        (Value::Function(_), _) => false,

        (Value::Thread(a), Value::Thread(b)) => a == b,
        (Value::Thread(_), _) => false,

        (Value::UserData(a), Value::UserData(b)) => a == b,
        (Value::UserData(_), _) => false,
    }
}
