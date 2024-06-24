use crate::Value;

// TODO: This module should be entirely replaced by `meta_ops` as they are added.

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    Some(lhs.to_constant()?.less_than(&rhs.to_constant()?)?.into())
}

pub fn less_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<bool> {
    Some(lhs.to_constant()?.less_equal(&rhs.to_constant()?)?.into())
}
