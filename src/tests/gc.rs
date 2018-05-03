use gc::{GcContext, GcObject, GcParameters};

#[test]
fn test_simple_allocate() {
    struct Simple(i64);
    impl GcObject for Simple {}

    let gc_context = GcContext::new(GcParameters::default());
    let r = gc_context.allocate_root(Simple(42));
    assert_eq!(r.as_ref().0, 42);
}
