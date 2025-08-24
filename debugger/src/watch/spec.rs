#[derive(Debug, Clone)]
pub enum WatchSpec {
    Register(usize),
    Global(String),
}
