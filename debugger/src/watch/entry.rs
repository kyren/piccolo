use super::{WatchMode, WatchSpec};

#[derive(Debug, Clone)]
pub struct WatchEntry {
    pub spec: WatchSpec,
    pub last: Option<String>,
    pub mode: WatchMode,
    pub last_line_seen: Option<(String, usize)>,
}
