#[derive(std::fmt::Debug)]
struct ErrorPosition {
    start: (usize, usize),
    end: (usize, usize),
}

/// WasminError enumerates all possible errors returned by this library.
#[derive(std::fmt::Debug)]
enum WasminError {
    /// Wasmin program contains a syntax error.
    SyntaxError {
        cause: String,
        pos: ErrorPosition,
    },

    /// Wasmin program contains a type error.
    TypeError {
        cause: String,
        pos: ErrorPosition,
    },

    /// Wraps an `std::io::Error` that occurred while reading sources.
    ReadError(std::io::Error),

    /// Wraps an `std::io::Error` that occurred while writing output.
    WriteError(std::io::Error),
}

impl std::error::Error for WasminError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            WasminError::SyntaxError { .. } => None,
            WasminError::TypeError { .. } => None,
            WasminError::ReadError(ref e) => Some(e),
            WasminError::WriteError(ref e) => Some(e),
        }
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
            WasminError::ReadError(ref e) => {
                write!(f, "read error: ")?;
                e.fmt(f)
            }
            WasminError::WriteError(ref e) => {
                write!(f, "write error: ")?;
                e.fmt(f)
            }
        }
    }
}
