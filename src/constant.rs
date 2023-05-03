use std::hash::{Hash, Hasher};

use gc_arena::Collect;

use crate::{String, Value};

/// Immutable value which implements Hash and Eq, where values are equal only when they are bit for
/// bit identical.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Constant<'gc> {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    String(String<'gc>),
}

impl<'gc> Constant<'gc> {
    pub fn from_value(value: Value<'gc>) -> Option<Constant<'gc>> {
        match value {
            Value::Nil => Some(Constant::Nil),
            Value::Boolean(b) => Some(Constant::Boolean(b)),
            Value::Integer(i) => Some(Constant::Integer(i)),
            Value::Number(n) => Some(Constant::Number(n)),
            Value::String(s) => Some(Constant::String(s)),
            _ => None,
        }
    }

    pub fn to_value(self) -> Value<'gc> {
        match self {
            Constant::Nil => Value::Nil,
            Constant::Boolean(b) => Value::Boolean(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Number(n) => Value::Number(n),
            Constant::String(s) => Value::String(s),
        }
    }
}

impl<'gc> PartialEq for Constant<'gc> {
    fn eq(&self, other: &Constant<'gc>) -> bool {
        match (self, other) {
            (Constant::Nil, Constant::Nil) => true,
            (Constant::Nil, _) => false,

            (Constant::Boolean(a), Constant::Boolean(b)) => a == b,
            (Constant::Boolean(_), _) => false,

            (Constant::Integer(a), Constant::Integer(b)) => a == b,
            (Constant::Integer(_), _) => false,

            (Constant::Number(a), Constant::Number(b)) => a.to_bits() == b.to_bits(),
            (Constant::Number(_), _) => false,

            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::String(_), _) => false,
        }
    }
}

impl<'gc> Eq for Constant<'gc> {}

impl<'gc> Hash for Constant<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Nil => {
                Hash::hash(&0, state);
            }
            Constant::Boolean(b) => {
                Hash::hash(&1, state);
                b.hash(state);
            }
            Constant::Integer(i) => {
                Hash::hash(&2, state);
                i.hash(state);
            }
            Constant::Number(n) => {
                Hash::hash(&3, state);
                n.to_bits().hash(state);
            }
            Constant::String(s) => {
                Hash::hash(&4, state);
                s.hash(state);
            }
        }
    }
}
