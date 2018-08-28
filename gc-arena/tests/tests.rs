extern crate rand;

#[macro_use]
extern crate gc_arena;

use std::collections::HashMap;
use std::rc::Rc;

use gc_arena::{ArenaParameters, Collect, Gc, GcCell};

#[test]
fn simple_allocation() {
    #[derive(Collect)]
    #[collect(empty_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, i32>,
    }

    make_arena!(TestArena, TestRoot);

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| TestRoot {
        test: Gc::allocate(mc, 42),
    });

    arena.mutate(|_mc, root| {
        assert_eq!(*((*root).test), 42);
    });
}

#[test]
fn repeated_allocation_deallocation() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(empty_drop)]
    struct TestRoot<'gc>(GcCell<'gc, HashMap<i32, Gc<'gc, (i32, RefCounter)>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, HashMap::new()))
    });

    for _ in 0..200 {
        arena.mutate(|mc, root| {
            let mut map = root.0.write(mc);
            for _ in 0..100 {
                let i = rand::random::<i32>() % 10000;
                if let Some(old) = map.insert(i, Gc::allocate(mc, (i, r.clone()))) {
                    assert_eq!(old.0, i);
                }
            }

            for _ in 0..100 {
                let i = rand::random::<i32>() % 10000;
                if let Some(old) = map.remove(&i) {
                    assert_eq!(old.0, i);
                }
            }
        });

        arena.collect_debt();
    }

    arena.collect_all();
    arena.collect_all();

    let live_size = arena.mutate(|_, root| root.0.read().len());

    println!("{}", live_size);
    assert_eq!(Rc::strong_count(&r.0), live_size + 1);
}

#[test]
fn all_dropped() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(empty_drop)]
    struct TestRoot<'gc>(GcCell<'gc, Vec<Gc<'gc, RefCounter>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, Vec::new()))
    });

    arena.mutate(|mc, root| {
        let mut v = root.0.write(mc);
        for _ in 0..100 {
            v.push(Gc::allocate(mc, r.clone()));
        }
    });
    drop(arena);
    assert_eq!(Rc::strong_count(&r.0), 1);
}

#[test]
fn all_garbage_collected() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(empty_drop)]
    struct TestRoot<'gc>(GcCell<'gc, Vec<Gc<'gc, RefCounter>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, Vec::new()))
    });

    arena.mutate(|mc, root| {
        let mut v = root.0.write(mc);
        for _ in 0..100 {
            v.push(Gc::allocate(mc, r.clone()));
        }
    });
    arena.mutate(|mc, root| {
        root.0.write(mc).clear();
    });
    arena.collect_all();
    arena.collect_all();
    assert_eq!(Rc::strong_count(&r.0), 1);
}

#[test]
fn derive_collect() {
    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    struct Test1<'gc> {
        a: i32,
        b: Gc<'gc, i32>,
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    struct Test2 {
        a: i32,
        b: i32,
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    enum Test3<'gc> {
        B(Gc<'gc, i32>),
        A(i32),
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    enum Test4 {
        A(i32),
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    struct Test5(Gc<'static, i32>);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(empty_drop)]
    struct Test6(i32);

    assert_eq!(Test1::needs_trace(), true);
    assert_eq!(Test2::needs_trace(), false);
    assert_eq!(Test3::needs_trace(), true);
    assert_eq!(Test4::needs_trace(), false);
    assert_eq!(Test5::needs_trace(), true);
    assert_eq!(Test6::needs_trace(), false);
}
