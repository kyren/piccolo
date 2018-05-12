use std::{usize, f64};

pub struct ArenaParameters {
    pub(crate) pause_factor: f64,
    pub(crate) timing_factor: f64,
    pub(crate) min_sleep: usize,
}

impl Default for ArenaParameters {
    fn default() -> ArenaParameters {
        const PAUSE_FACTOR: f64 = 0.5;
        const TIMING_FACTOR: f64 = 1.5;
        const MIN_SLEEP: usize = 4096;

        ArenaParameters {
            pause_factor: PAUSE_FACTOR,
            timing_factor: TIMING_FACTOR,
            min_sleep: MIN_SLEEP,
        }
    }
}

#[macro_export]
macro_rules! make_arena {
    ($arena:ident, $root:ident) => {
        struct $arena {
            context: $crate::Context,
            root: $root<'static>,
        }
        make_arena!(@methods $arena, $root)
    };

    (pub $arena:ident, $root:ident) => {
        pub struct $arena {
            context: $crate::Context,
            root: $root<'static>,
        }
        make_arena!(@methods $arena, $root)
    };

    (@methods $arena:ident, $root:ident) => {
        impl $arena
        where
            for<'gc> $root<'gc>: $crate::Collect,
        {
            #[allow(unused)]
            pub fn new<F>(arena_parameters: $crate::ArenaParameters, f: F) -> $arena
            where
                F: for<'gc> FnOnce($crate::MutationContext<'gc>) -> $root<'gc>,
            {
                unsafe {
                    let context = $crate::Context::new(arena_parameters);
                    let root: $root<'static> = ::std::mem::transmute(f(context.mutation_context()));
                    $arena { context, root }
                }
            }

            #[allow(unused)]
            pub fn mutate<F, R>(&self, f: F) -> R
            where
                F: for<'gc> FnOnce($crate::MutationContext<'gc>, &$root<'gc>) -> R,
            {
                unsafe {
                    f(
                        self.context.mutation_context(),
                        ::std::mem::transmute(&self.root),
                    )
                }
            }

            #[allow(unused)]
            pub fn allocation_debt(&self) -> f64 {
                self.context.allocation_debt()
            }

            #[allow(unused)]
            pub fn collect_debt(&self) {
                unsafe {
                    self.context
                        .do_collection(&self.root, self.context.allocation_debt());
                }
            }

            #[allow(unused)]
            pub fn collect_all(&self) {
                unsafe {
                    self.context.do_collection(&self.root, ::std::f64::INFINITY);
                }
            }
        }
    };
}
