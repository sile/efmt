use std::backtrace::{Backtrace, BacktraceStatus};

pub struct Error {
    pub reason: String,
    pub backtrace: Backtrace,
}

impl Error {
    #[track_caller]
    pub fn new<T: Into<String>>(reason: T) -> Self {
        Self {
            reason: reason.into(),
            backtrace: Backtrace::capture(),
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reason)?;
        if self.backtrace.status() == BacktraceStatus::Captured {
            write!(f, "\n\nBacktrace:\n{}", self.backtrace)?;
        }
        Ok(())
    }
}

impl<E: std::error::Error> From<E> for Error {
    #[track_caller]
    fn from(e: E) -> Self {
        Self::new(e.to_string())
    }
}
