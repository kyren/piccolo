#[macro_export]
macro_rules! make_sequencable_arena {
    ($module:ident, $root:ident) => {
        mod $module {
            use std::any::Any;
            use std::marker::PhantomData;

            use gc_arena::{make_arena, ArenaParameters, Collect, GcCell, MutationContext};
            use gc_sequence::{Sequence, SequenceExt};

            use super::$root;

            #[derive(Collect)]
            #[collect(empty_drop)]
            struct InnerRoot<'gc> {
                root: $root<'gc>,
                current_sequence: GcCell<
                    'gc,
                    Option<Box<dyn Sequence<'gc, Output = Box<dyn Any + 'static>> + 'gc>>,
                >,
            }

            make_arena!(InnerArena, InnerRoot);

            pub(super) struct Arena(InnerArena);

            impl Arena {
                #[allow(unused)]
                pub fn new<F>(arena_parameters: ArenaParameters, f: F) -> Arena
                where
                    F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> $root<'gc>,
                {
                    Arena(InnerArena::new(arena_parameters, move |mc| InnerRoot {
                        root: f(mc),
                        current_sequence: GcCell::allocate(mc, None),
                    }))
                }

                #[allow(unused)]
                pub fn try_new<F, E>(arena_parameters: ArenaParameters, f: F) -> Result<Arena, E>
                where
                    F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> Result<$root<'gc>, E>,
                {
                    Ok(Arena(InnerArena::try_new(arena_parameters, move |mc| {
                        Ok(InnerRoot {
                            root: f(mc)?,
                            current_sequence: GcCell::allocate(mc, None),
                        })
                    })?))
                }

                #[allow(unused)]
                pub fn mutate<F, R>(&mut self, f: F) -> R
                where
                    F: for<'gc> FnOnce(MutationContext<'gc, '_>, &$root<'gc>) -> R,
                {
                    self.0.mutate(move |mc, root| f(mc, &root.root))
                }

                #[allow(unused)]
                pub fn sequence<F, O>(mut self, f: F) -> Sequencer<O>
                where
                    O: 'static,
                    F: for<'gc> FnOnce(&$root<'gc>) -> Box<dyn Sequence<'gc, Output = O> + 'gc>,
                {
                    self.0.mutate(move |mc, root| {
                        *root.current_sequence.write(mc) =
                            Some(f(&root.root).map(|r| -> Box<Any> { Box::new(r) }).boxed());
                    });
                    Sequencer(self.0, PhantomData)
                }

                #[allow(unused)]
                #[inline]
                pub fn total_allocated(&self) -> usize {
                    self.0.total_allocated()
                }

                #[allow(unused)]
                #[inline]
                pub fn allocation_debt(&self) -> f64 {
                    self.0.allocation_debt()
                }

                #[allow(unused)]
                #[inline]
                pub fn collect_debt(&mut self) {
                    self.0.collect_debt()
                }

                #[allow(unused)]
                pub fn collect_all(&mut self) {
                    self.0.collect_all()
                }
            }

            pub(super) struct Sequencer<O>(InnerArena, PhantomData<O>);

            impl<O> Sequencer<O>
            where
                O: 'static,
            {
                #[allow(unused)]
                pub fn step(mut self) -> Result<(Arena, O), Sequencer<O>> {
                    let r = self.0.mutate(move |mc, root| {
                        root.current_sequence.write(mc).as_mut().unwrap().step(mc)
                    });

                    if let Some(r) = r {
                        self.0.mutate(|mc, root| {
                            *root.current_sequence.write(mc) = None;
                        });
                        Ok((
                            Arena(self.0),
                            *Box::<dyn Any + 'static>::downcast(r).unwrap(),
                        ))
                    } else {
                        Err(self)
                    }
                }

                #[allow(unused)]
                #[inline]
                pub fn total_allocated(&self) -> usize {
                    self.0.total_allocated()
                }

                #[allow(unused)]
                #[inline]
                pub fn allocation_debt(&self) -> f64 {
                    self.0.allocation_debt()
                }

                #[allow(unused)]
                #[inline]
                pub fn collect_debt(&mut self) {
                    self.0.collect_debt()
                }

                #[allow(unused)]
                pub fn collect_all(&mut self) {
                    self.0.collect_all()
                }
            }
        }
    };
}
