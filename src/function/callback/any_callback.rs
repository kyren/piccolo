use gc_arena::{Collect, Gc, MutationContext};

use crate::{Callback, CallbackMode, Error, Value};

/// Represents a callback as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop, bound = "")]
pub struct AnyCallback<'gc>(Gc<'gc, Header<'gc>>);

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        MutationContext<'gc, '_>,
        &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>,
}

impl<'gc> AnyCallback<'gc> {
    pub fn new<C: Callback<'gc> + 'gc>(mc: MutationContext<'gc, '_>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: Header<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but function
        // pointers can't hold any data. It would be nice if function pointers could have higher rank
        // `for<'gc>` lifetimes.
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

        Self(unsafe { Gc::cast::<Header>(hc) })
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn call(
        self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0) as *const (), mc, stack) }
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
            assert!(matches!(stack[0], Value::Integer(17)));
        })
    }
}
