use crate::parse::model::Position;

pub type Result<T> = std::result::Result<T, Error>;

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
    SyntaxError { cause: String, pos: Position },

    /// Wasmin program contains a type error.
    TypeError { cause: String, pos: Position },

    DuplicateDeclaration { cause: String, pos: Position, other: Position },

    /// Wasmin program is using an unsupported feature.
    UnsupportedFeatureError { cause: String, pos: Position },
}

impl WasminError {
    pub fn cause(&self) -> &str {
        match self {
            WasminError::SyntaxError { cause, .. } => cause,
            WasminError::TypeError { cause, .. } => cause,
            WasminError::DuplicateDeclaration { cause, .. } => cause,
            WasminError::UnsupportedFeatureError { cause, .. } => cause,
        }
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
            WasminError::DuplicateDeclaration { cause, .. } => {
                write!(f, "duplicate declaration: {}", cause)
            }
            WasminError::UnsupportedFeatureError { cause, .. } => {
                write!(f, "unsupported feature error: {}", cause)
            }
        }
    }
}
