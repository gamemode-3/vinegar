use thiserror::Error;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("{0}\nInvalid character: \'{1}\'")]
    InvalidCharacterError(String, String),

    #[error("{0}\nunexpected end of file. {1}")]
    UnexpectedEndOfFileError(String, String),

    #[error("{0}\nunexpected end of statement. {1}")]
    UnexpectedEndOfStatementError(String, String),

    #[error("{0}\nunexpected indent.")]
    UnexpectedIndentError(String),

    #[error("{0}\nexpected an expression.")]
    ExpectedExpressionError(String),

    #[error("{0}\n{1}")]
    MissingTokenError(String, String),
}

#[derive(Debug, Error)]
pub enum FileInterpreterError {
    #[error("{0}")]
    InterpreterError(InterpreterError),

    #[error("{0}")]
    IOError(std::io::Error),
}

#[derive(Clone)]
pub struct DebugInfo {
    pub source_name: String,
}

impl DebugInfo {
    pub fn new(source_name: String) -> DebugInfo {
        Self {
            source_name: source_name,
        }
    }
}
