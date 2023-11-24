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
