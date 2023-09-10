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

    #[error("{0}\n{1}")]
    ExpectedIndentError(String, String),

    #[error("{0}\nunexpected token: {1}. {2}")]
    UnexpectedTokenError(String, String, String),

    #[error("{0}\nexpected an expression.")]
    ExpectedExpressionError(String),

    #[error("{0}\n{1}")]
    MissingTokenError(String, String),

    #[error("{0}\ninvalid literal: {1}. {2}")]
    InvalidLiteralError(String, String, String),

    #[error("{0}\nunexpected literal: {1}. {2}")]
    UnexpectedLiteralError(String, String, String),

    #[error("{0}\n{1}")]
    IncompatibleTypesError(String, String),

    #[error("{0}\nunknown identifier: \"{1}\"")]
    UnknownIdentifier(String, String),

    #[error("{0}\n{1}")]
    VinegarError(String, VinegarError),
}

#[derive(Debug, Error)]
pub enum VinegarError {
    #[error("{0} is of type {1}, which is not callable.")]
    NotCallableError(String, String),

    #[error("{0}")]
    InvalidArgumentsError(String),

    #[error("{0}")]
    TypeError(String),

    #[error("attribute not found: {0}")]
    AttributeNotFound(String),
}

#[derive(Debug, Error)]
pub enum FileInterpreterError {
    #[error("{0}")]
    InterpreterError(InterpreterError),

    #[error("{0}")]
    IOError(std::io::Error),
}

impl From<InterpreterError> for FileInterpreterError {
    fn from(value: InterpreterError) -> Self {
        Self::InterpreterError(value)
    }
}

impl From<std::io::Error> for FileInterpreterError {
    fn from(value: std::io::Error) -> Self {
        Self::IOError(value)
    }
}

#[derive(Clone)]
pub struct DebugInfo {
    pub source: Option<String>,
    pub source_name: String,
}

impl DebugInfo {
    pub fn new(source: Option<String>, source_name: String) -> DebugInfo {
        Self {
            source: source,
            source_name: source_name,
        }
    }
}
