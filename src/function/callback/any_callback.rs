use gc_arena::{Collect, Gc, MutationContext};

use crate::{Callback, CallbackMode, Error, Value};

/// Represents a callback as a single pointer with an inline VTable header, or a bare fn pointer.
#[derive(Clone, Copy)]
pub enum AnyCallback<'gc> {
    Dyn(DynCallback<'gc>),
    FnPtr(CallbackFn<'gc>),
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct DynCallback<'gc>(Gc<'gc, Header<'gc>>);

type CallbackFn<'gc> = fn(
    MutationContext<'gc, '_>,
    stack: &mut Vec<Value<'gc>>,
) -> Result<CallbackMode<'gc>, Error<'gc>>;

// SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but function
// pointers can't hold any data. It would be nice if function pointers could have higher rank
// `for<'gc>` lifetimes.
unsafe impl<'gc> Collect for AnyCallback<'gc> {
    fn trace(&self, cc: gc_arena::CollectionContext) {
        match self {
            AnyCallback::Dyn(d) => d.trace(cc),
            AnyCallback::FnPtr(_) => {}
        }
    }
}

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        MutationContext<'gc, '_>,
        &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>,
}

#[repr(C)]
struct HeaderCallback<'gc, C> {
    header: Header<'gc>,
    callback: C,
}

// SAFETY: We can't implement `Collect` automatically again, because of the function pointer
// lifetimes.
unsafe impl<'gc, C: Collect> Collect for HeaderCallback<'gc, C> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        C::needs_trace()
    }

    fn trace(&self, cc: gc_arena::CollectionContext) {
        self.callback.trace(cc)
    }
}

impl<'gc> AnyCallback<'gc> {
    pub fn new<C: Callback<'gc> + 'gc>(mc: MutationContext<'gc, '_>, callback: C) -> Self {
        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: Header {
                    call: |ptr, mc, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(mc, stack)
                    },
                },
                callback,
            },
        );

        Self::Dyn(DynCallback(unsafe { Gc::cast::<Header>(hc) }))
    }

    pub fn from_fn_ptr<C: Callback<'gc> + 'gc>(f: CallbackFn<'gc>) -> Self {
        Self::FnPtr(f)
    }

    pub fn as_ptr(self) -> *const () {
        match self {
            AnyCallback::Dyn(d) => Gc::as_ptr(d.0) as *const (),
            AnyCallback::FnPtr(f) => f as *const (),
        }
    }

    pub fn call(
        self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        match self {
            AnyCallback::Dyn(d) => unsafe { (d.0.call)(Gc::as_ptr(d.0) as *const (), mc, stack) },
            AnyCallback::FnPtr(f) => f(mc, stack),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::CallbackReturn;

    use gc_arena::rootless_arena;

    use super::*;

    #[test]
    fn test_dyn_callback() {
        rootless_arena(|mc| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct CB(i64);

            impl<'gc> Callback<'gc> for CB {
                fn call(
                    &self,
                    _: MutationContext<'gc, '_>,
                    stack: &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                    stack.push(Value::Integer(self.0));
                    Ok(CallbackReturn::Return.into())
                }
            }

            let dyn_callback = AnyCallback::new(mc, CB(17));

            let mut stack = Vec::new();
            assert!(dyn_callback.call(mc, &mut stack).is_ok());
            assert!(stack[0] == Value::Integer(17));
        })
    }
}
