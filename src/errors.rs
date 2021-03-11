pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! err_wasmin {
    ($e:expr) => {{
        use crate::errors::Error;
        Err(Error::Wasmin($e))
    }};
}

#[macro_export]
macro_rules! err_io {
    ($e:expr) => {{
        use crate::errors::Error;
        Err(Error::IO($e))
    }};
}

#[macro_export]
macro_rules! werr_syntax {
    ($cause:expr, $start:expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        let end = $start.clone();
        WasminError::SyntaxError {
            cause: $cause.to_owned(),
            pos: ErrorPosition { start: $start, end },
        }
    }};
    ($cause:expr, $start:expr, $end: expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        WasminError::SyntaxError {
            cause: $cause.to_owned(),
            pos: ErrorPosition {
                start: $start,
                end: $end,
            },
        }
    }};
}

#[macro_export]
macro_rules! werr_type {
    ($cause:expr, $start:expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        let end = $start.clone();
        WasminError::TypeError {
            cause: $cause.to_owned(),
            pos: ErrorPosition { start: $start, end },
        }
    }};
    ($cause:expr, $start:expr, $end: expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        WasminError::TypeError {
            cause: $cause.to_owned(),
            pos: ErrorPosition {
                start: $start,
                end: $end,
            },
        }
    }};
}

#[macro_export]
macro_rules! werr_unsupported_feature {
    ($cause:expr, $start:expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        let end = $start.clone();
        WasminError::UnsupportedFeatureError {
            cause: $cause.to_owned(),
            pos: ErrorPosition { start: $start, end },
        }
    }};
    ($cause:expr, $start:expr, $end: expr) => {{
        use crate::errors::{ErrorPosition, WasminError};
        WasminError::UnsupportedFeatureError {
            cause: $cause.to_owned(),
            pos: ErrorPosition {
                start: $start,
                end: $end,
            },
        }
    }};
}

#[derive(std::fmt::Debug, PartialEq, Clone, Hash, Eq)]
pub struct ErrorPosition {
    pub start: (usize, usize),
    pub end: (usize, usize),
}

/// Top-level error type in the wasmin crate.
#[derive(std::fmt::Debug)]
pub enum Error {
    /// IO Error.
    IO(std::io::Error),
    /// Wasmin program error.
    Wasmin(WasminError),
    /// WASM validation error.
    Validation(String),
}

/// WasminError enumerates all non-IO errors returned by this library.
#[derive(std::fmt::Debug, PartialEq, Clone, Hash, Eq)]
pub enum WasminError {
    /// Wasmin program contains a syntax error.
    SyntaxError { cause: String, pos: ErrorPosition },

    /// Wasmin program contains a type error.
    TypeError { cause: String, pos: ErrorPosition },

    /// Wasmin program is using an unsupported feature.
    UnsupportedFeatureError { cause: String, pos: ErrorPosition },
}

impl WasminError {
    pub fn pos(&self) -> &ErrorPosition {
        match self {
            WasminError::SyntaxError { pos, .. } => pos,
            WasminError::TypeError { pos, .. } => pos,
            WasminError::UnsupportedFeatureError { pos, .. } => pos,
        }
    }

    pub fn cause(&self) -> &str {
        match self {
            WasminError::SyntaxError { cause, .. } => cause,
            WasminError::TypeError { cause, .. } => cause,
            WasminError::UnsupportedFeatureError { cause, .. } => cause,
        }
    }

    pub fn relevant_text(&self, text: &str) -> String {
        let pos = self.pos();
        let line = text.lines().skip(pos.start.0).next().unwrap();
        let index = format!("[{}, {}]", pos.start.0, pos.start.1);
        let pointer_line = " ".repeat(pos.start.1 + index.len() + 2);
        format!("{} {}\n{}^\n", index, line, pointer_line)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IO(e) => e.source(),
            Error::Validation(_) | Error::Wasmin(_) => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::IO(e) => {
                write!(f, "I/O error: {}", e)
            }
            Error::Validation(e) => {
                write!(f, "WASM validation error: {}", e)
            }
            Error::Wasmin(e) => {
                write!(f, "{}", e)
            }
        }
    }
}

impl std::error::Error for WasminError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl std::fmt::Display for WasminError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            WasminError::SyntaxError { cause, .. } => {
                write!(f, "syntax error: {}", cause)
            }
            WasminError::TypeError { cause, .. } => {
                write!(f, "type error: {}", cause)
            }
            WasminError::UnsupportedFeatureError { cause, .. } => {
                write!(f, "unsupported feature error: {}", cause)
            }
        }
    }
}
