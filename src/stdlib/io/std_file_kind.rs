use gc_arena::Collect;

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub enum StdFileKind {
    Stdin,
    Stdout,
    Stderr,
}
