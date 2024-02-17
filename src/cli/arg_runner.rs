use std::fmt::Display;

pub enum CliMessage {
    Message(String),
    Warning(String),
    Error(String),
}

impl Display for CliMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliMessage::Message(m) => write!(f, "{}", m),
            CliMessage::Warning(w) => write!(f, "[WARNING]: {}", w),
            CliMessage::Error(e) => write!(f, "[ERROR]: {}", e),
        }
    }
}


pub trait ArgRunner {
    fn run(&self) -> CliMessage;
}
