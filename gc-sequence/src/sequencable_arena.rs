#[macro_export]
macro_rules! make_sequencable_arena {
    ($module:ident, $root:ident) => {
        mod $module {
            use std::any::Any;
            use std::marker::PhantomData;

            use gc_arena::{make_arena, ArenaParameters, Collect, GcCell, MutationContext};
            use gc_sequence::{Sequence, SequenceExt};

            type DynSequence<'gc> =
                dyn Sequence<'gc, Item = Box<dyn Any + 'static>, Error = Box<dyn Any + 'static>>
                    + 'gc;

            use super::$root;

            #[derive(Collect)]
            #[collect(empty_drop)]
            struct InnerRoot<'gc> {
                root: $root<'gc>,
                current_sequence: GcCell<'gc, Option<Box<DynSequence<'gc>>>>,
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
                pub fn sequence<F, I, E>(mut self, f: F) -> Sequencer<I, E>
                where
                    I: 'static,
                    E: 'static,
                    F: for<'gc> FnOnce(
                        &$root<'gc>,
                    )
                        -> Box<Sequence<'gc, Item = I, Error = E> + 'gc>,
                {
                    self.0.mutate(move |mc, root| {
                        *root.current_sequence.write(mc) = Some(
                            f(&root.root)
                                .map_ok(|r| -> Box<Any> { Box::new(r) })
                                .map_err(|e| -> Box<Any> { Box::new(e) })
                                .boxed(),
                        );
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

            pub(super) struct Sequencer<I, E>(InnerArena, PhantomData<(I, E)>)
            where
                I: 'static,
                E: 'static;

            impl<I, E> Sequencer<I, E>
            where
                I: 'static,
                E: 'static,
            {
                #[allow(unused)]
                pub fn step(mut self) -> Result<(Arena, Result<I, E>), Sequencer<I, E>> {
                    let r = self.0.mutate(move |mc, root| {
                        root.current_sequence.write(mc).as_mut().unwrap().step(mc)
                    });

                    if let Some(r) = r {
                        self.0.mutate(|mc, root| {
                            *root.current_sequence.write(mc) = None;
                        });

                        let r = match r {
                            Ok(r) => Ok(*Box::<Any + 'static>::downcast(r).unwrap()),
                            Err(e) => Err(*Box::<Any + 'static>::downcast(e).unwrap()),
                        };

                        Ok((Arena(self.0), r))
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
