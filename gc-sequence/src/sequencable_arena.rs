/// Creates a set of types for running "sequences" on `gc_arena` "arena" types.
///
/// Takes two parameters, the first is the name of a module that will be created, the second is the
/// name of a type that can be made into an "arena" type with `gc_arena::make_arena`.
///
/// The module will contain two accessible types, `module::Arena` `module::Sequencer`.  The `Arena`
/// type is the same as what would be produced by `gc_arena::make_arena`, but has a single extra
/// method `Arena::sequence`.  `Arena::sequence` can be used to produce a `module::Sequencer`, which
/// can then be stepped until a result is produced.
///
/// ```
/// # use gc_arena::{ArenaParameters, Collect, Gc};
/// # use gc_sequence::{make_sequencable_arena, SequenceExt, SequenceResultExt};
///
/// #[derive(Collect)]
/// #[collect(empty_drop)]
/// struct TestRoot<'gc> {
///     test: Gc<'gc, i32>,
/// }
///
/// make_sequencable_arena!(test_sequencer, TestRoot);
/// use test_sequencer::Arena as TestArena;
///
/// fn main() {
///     let arena = TestArena::new(ArenaParameters::default(), |mc| TestRoot {
///         test: Gc::allocate(mc, 42),
///     });
///
///     let mut sequence = arena.sequence(|root| {
///         gc_sequence::from_fn_with(root.test, |_, test| {
///             *test + 10
///         })
///         .then(|_, r| r - 20)
///         .then(|_, r| r - 30)
///         .then(|_, r| r + 2)
///         .boxed()
///     });
///
///     loop {
///         match sequence.step() {
///             Ok((_, output)) => {
///                 assert_eq!(output, 4);
///                 return;
///             }
///             Err(s) => sequence = s,
///         }
///     }
/// }
/// ```
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
                /// Create a new arena with the given garbage collector tuning parameters.
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

                /// Allows for creating an arena with a constructor that can fail.
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

                /// Provides access to a garbage collected arena, during which no garbage collection
                /// may take place.
                #[allow(unused)]
                pub fn mutate<F, R>(&mut self, f: F) -> R
                where
                    F: for<'gc> FnOnce(MutationContext<'gc, '_>, &$root<'gc>) -> R,
                {
                    self.0.mutate(move |mc, root| f(mc, &root.root))
                }

                /// Access a garbage collected arena with a `Sequence`, allowing garbage collection
                /// to take place in between sequence steps.
                ///
                /// Consumes this arena type, but the arena will be returned when the `Sequencer` is
                /// finished.
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

                /// Returns total currently used memory
                #[allow(unused)]
                #[inline]
                pub fn total_allocated(&self) -> usize {
                    self.0.total_allocated()
                }

                /// Returns the current "allocation debt", measured in bytes.  Allocation debt rises
                /// as allocation takes place based on the `ArenaParameters` set for this arena.
                #[allow(unused)]
                #[inline]
                pub fn allocation_debt(&self) -> f64 {
                    self.0.allocation_debt()
                }

                /// Runs the incremental garbage collector until the allocation debt is <= 0.0.
                /// There is no minimum unit of work enforced here, so it may be faster to only call
                /// this method when the allocation debt is above some threshold.
                #[allow(unused)]
                #[inline]
                pub fn collect_debt(&mut self) {
                    self.0.collect_debt()
                }

                /// Run the current garbage collection cycle to completion, stopping once the
                /// garbage collector has entered the sleeping phase.
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
                /// Steps the current sequence.  Returns `Ok((arena, result))` if the sequence is
                /// complete, and `Err(self)` otherwise.
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

                /// *Abort* this sequence, returning the inner arena type.
                pub fn abort(mut self) -> Arena {
                    self.0.mutate(|mc, root| {
                        *root.current_sequence.write(mc) = None;
                    });
                    Arena(self.0)
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
