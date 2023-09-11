use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}\nLexerError:\n{1}")]
    LexerError(String, LexerError),

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

    #[error("invalid literal: {0}. {1}")]
    InvalidLiteralError(String, String, String),

    #[error("{0}\n{1}")]
    MissingTokenError(String, String),

    #[error("{0}\nunexpected literal: {1}. {2}")]
    UnexpectedLiteralError(String, String, String),

    #[error("{0}\n{1}")]
    IncompatibleTypesError(String, String),

    #[error("{0}\nunknown identifier: \"{1}\"")]
    UnknownIdentifier(String, String),

    #[error("{0}\n{1}")]
    VinegarError(String, VinegarError),

    #[error("{0}\n{1} \"{2}\" has no attribute \"{3}\"")]
    AttributeNotFound(String, String, String, String),
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("invalid literal: {0}. {1}")]
    InvalidLiteralError(String, String),

    #[error("unexpected end of file. {0}")]
    UnexpectedEndOfFileError(String),

    #[error("Invalid character: \'{0}\'")]
    InvalidCharacterError(String),
}

#[derive(Debug, Error)]
pub enum VinegarError {
    #[error("{0} is of type {1}, which is not callable.")]
    NotCallableError(String, String),

    #[error("{0}")]
    InvalidArgumentsError(String),

    #[error("{0}")]
    TypeError(String),

    #[error("{0} has no attribute \"{1}\"")]
    AttributeNotFound(String, String),
}

#[derive(Debug, Error)]
pub enum FileOrOtherError {
    #[error("{0}")]
    OtherError(Error),

    #[error("{0}")]
    IOError(std::io::Error),
}

impl From<Error> for FileOrOtherError {
    fn from(value: Error) -> Self {
        Self::OtherError(value)
    }
}

impl From<std::io::Error> for FileOrOtherError {
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
