use super::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StopReason {
    Breakpoint {
        location: Location,
        breakpoint_ids: Vec<usize>,
    },
    Step(Location),
    Watchpoint(String),
    Suspended,
}

impl StopReason {
    pub const fn reason(&self) -> &'static str {
        match self {
            StopReason::Breakpoint { .. } => "breakpoint",
            StopReason::Step { .. } => "step",
            StopReason::Watchpoint { .. } => "watchpoint",
            StopReason::Suspended => "suspended",
        }
    }
}

impl std::fmt::Display for StopReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StopReason::Breakpoint {
                location,
                breakpoint_ids,
            } => write!(
                f,
                "StopReason::Breakpoint {{ location: {:?}, breakpoint_ids: {:?} }}",
                location, breakpoint_ids
            ),
            StopReason::Step(location) => {
                write!(f, "StopReason::Step {{ location: {:?} }}", location)
            }
            StopReason::Watchpoint(watchpoint) => write!(
                f,
                "StopReason::Watchpoint {{ watchpoint: {:?} }}",
                watchpoint
            ),
            StopReason::Suspended => write!(f, "StopReason::Suspended"),
        }
    }
}
