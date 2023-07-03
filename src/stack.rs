use std::{
    collections::{vec_deque, VecDeque},
    iter, ops,
};

use gc_arena::Collect;

use crate::{Context, FromMultiValue, FromValue, IntoMultiValue, IntoValue, TypeError, Value};

#[derive(Clone, Default, Collect)]
#[collect(no_drop)]
pub struct Stack<'gc>(VecDeque<Value<'gc>>);

impl<'gc> ops::Deref for Stack<'gc> {
    type Target = VecDeque<Value<'gc>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc> ops::DerefMut for Stack<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'gc> Stack<'gc> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, i: usize) -> Value<'gc> {
        self.0.get(i).copied().unwrap_or_default()
    }

    pub fn into_back(&mut self, ctx: Context<'gc>, v: impl IntoMultiValue<'gc>) {
        for v in v.into_multi_value(ctx) {
            self.0.push_back(v.into_value(ctx));
        }
    }

    pub fn into_front(&mut self, ctx: Context<'gc>, v: impl IntoMultiValue<'gc>) {
        let mut c = 0;
        for v in v.into_multi_value(ctx) {
            c += 1;
            self.0.push_back(v.into_value(ctx));
        }
        self.0.rotate_right(c);
    }

    pub fn from_back<V: FromValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<V, TypeError> {
        V::from_value(ctx, self.0.pop_back().unwrap_or_default())
    }

    pub fn from_front<V: FromValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<V, TypeError> {
        V::from_value(ctx, self.pop_front().unwrap_or_default())
    }

    pub fn replace(&mut self, ctx: Context<'gc>, v: impl IntoMultiValue<'gc>) {
        self.0.clear();
        self.0.extend(v.into_multi_value(ctx));
    }

    pub fn consume<V: FromMultiValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<V, TypeError> {
        V::from_multi_value(ctx, self.0.drain(..))
    }
}

impl<'a, 'gc: 'a> IntoIterator for &'a Stack<'gc> {
    type Item = Value<'gc>;
    type IntoIter = iter::Copied<vec_deque::Iter<'a, Value<'gc>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}
