use gc_arena::MutationContext;

use crate::{String, Value};

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
