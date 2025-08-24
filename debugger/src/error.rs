#[derive(Debug, Clone, Copy)]
pub enum Error {
    SenderDead,
    ReceiverDead,
    UnexpectedResponse,
    ExecutorFinished,
    ExecutorAlreadyRunning,
    SessionNotFound,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SenderDead => write!(f, "the sender is dead"),
            Error::ReceiverDead => write!(f, "the receiver is dead"),
            Error::UnexpectedResponse => write!(f, "the response is unexpected"),
            Error::ExecutorFinished => write!(f, "the executor has finished"),
            Error::ExecutorAlreadyRunning => write!(f, "the executor is already running"),
            Error::SessionNotFound => write!(f, "the session is not found"),
        }
    }
}
