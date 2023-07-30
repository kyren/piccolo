use std::{
    iter,
    ops::{Index, IndexMut, RangeBounds},
    slice::{self, SliceIndex},
};

use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Mutation};

use crate::{Context, FromMultiValue, FromValue, IntoMultiValue, IntoValue, TypeError, Value};

#[derive(Clone, Collect)]
#[collect(no_drop)]
pub struct Stack<'gc>(vec::Vec<Value<'gc>, MetricsAlloc<'gc>>);

impl<'gc> Stack<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self(vec::Vec::new_in(MetricsAlloc::new(mc)))
    }

    pub fn get(&self, i: usize) -> Value<'gc> {
        self.0.get(i).copied().unwrap_or_default()
    }

    pub fn push_back(&mut self, value: Value<'gc>) {
        self.0.push(value);
    }

    pub fn push_front(&mut self, value: Value<'gc>) {
        self.0.insert(0, value);
    }

    pub fn pop_back(&mut self) -> Value<'gc> {
        self.0.pop().unwrap_or_default()
    }

    pub fn pop_front(&mut self) -> Value<'gc> {
        if self.0.is_empty() {
            Value::Nil
        } else {
            self.0.remove(0)
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn resize(&mut self, size: usize) {
        self.0.resize(size, Value::Nil);
    }

    pub fn drain<R: RangeBounds<usize>>(
        &mut self,
        range: R,
    ) -> vec::Drain<Value<'gc>, MetricsAlloc<'gc>> {
        self.0.drain(range)
    }

    pub fn into_back(&mut self, ctx: Context<'gc>, v: impl IntoMultiValue<'gc>) {
        for v in v.into_multi_value(ctx) {
            self.0.push(v.into_value(ctx));
        }
    }

    pub fn into_front(&mut self, ctx: Context<'gc>, v: impl IntoMultiValue<'gc>) {
        let mut c = 0;
        for v in v.into_multi_value(ctx) {
            c += 1;
            self.0.push(v.into_value(ctx));
        }
        self.0.rotate_right(c);
    }

    pub fn from_back<V: FromValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<V, TypeError> {
        V::from_value(ctx, self.0.pop().unwrap_or_default())
    }

    pub fn from_front<V: FromValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<V, TypeError> {
        if self.0.is_empty() {
            V::from_value(ctx, Value::Nil)
        } else {
            V::from_value(ctx, self.0.remove(0))
        }
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
    type IntoIter = iter::Copied<slice::Iter<'a, Value<'gc>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}

impl<'gc> Extend<Value<'gc>> for Stack<'gc> {
    fn extend<T: IntoIterator<Item = Value<'gc>>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl<'a, 'gc: 'a> Extend<&'a Value<'gc>> for Stack<'gc> {
    fn extend<T: IntoIterator<Item = &'a Value<'gc>>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl<'gc, I: SliceIndex<[Value<'gc>]>> Index<I> for Stack<'gc> {
    type Output = <Vec<Value<'gc>> as Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<'gc> IndexMut<usize> for Stack<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
