use std::rc::Rc;

use gc::{GcContext, GcObject, GcParameters, GcTracer};

#[test]
fn test_simple_allocate() {
    struct Simple(i64, Rc<()>);
    impl GcObject for Simple {}

    let rc = Rc::new(());
    {
        let gc_context = GcContext::new(GcParameters::default());
        let r = gc_context.allocate_root(Simple(42, rc.clone()));
        assert_eq!(r.as_ref().0, 42);
    }
    assert_eq!(Rc::strong_count(&rc), 1);
}

#[test]
fn test_repeated_allocations() {
    struct Simple(i64);
    impl GcObject for Simple {}

    let gc_context = GcContext::new(GcParameters::default());
    let mut v = Vec::new();
    for i in 0..20000 {
        if i < 5000 && i % 10 == 0 {
            v.push(gc_context.allocate_root(Simple(42)));
        } else {
            gc_context.allocate(Simple(i));
        }
    }
}

#[test]
fn test_locked_tracing() {
    struct Blocking(bool);
    impl GcObject for Blocking {
        unsafe fn trace<'a>(&self, _: &GcTracer<'a, Self>) -> bool {
            self.0
        }
    }

    let gc_context = GcContext::new(GcParameters::default());
    let _lock = gc_context.allocate_root(Blocking(false));
    for _ in 0..1000 {
        gc_context.allocate(Blocking(true));
    }
}
