use colored::Colorize;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}\nLexerError:\n{1}")]
    LexerError(String, LexerError),

    #[error("{0}\nParserError:\n{1}")]
    ParserError(String, ParserError),

    #[error("{0}\n{1}")]
    VinegarError(String, VinegarError),
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
pub enum ParserError {
    #[error("unexpected token: {0}. {1}")]
    UnexpectedTokenError(String, String),

    #[error("unexpected end of file. {0}")]
    UnexpectedEndOfFileError(String),

    #[error("unexpected indent.")]
    UnexpectedIndentError(),

    #[error("{0}")]
    ExpectedIndentError(String),

    #[error("{0}")]
    MissingTokenError(String),

    #[error("unexpected end of statement. {0}")]
    UnexpectedEndOfStatementError(String),

    #[error("expected an expression.")]
    ExpectedExpressionError(),

    #[error("invalid literal: {0}. {1}")]
    InvalidLiteralError(String, String),

    #[error("unexpected literal: {0}. {1}")]
    UnexpectedLiteralError(String, String),
}

#[derive(Debug, Error)]
pub enum VinegarError {
    #[error("{0} is of type {1}, which is not callable.")]
    NotCallableError(String, String),

    #[error("{0}")]
    InvalidArgumentsError(String),

    #[error("{0}")]
    TypeError(String),

    #[error("{0} \"{1}\" has no attribute \"{2}\"")]
    VarAttributeNotFound(String, String, String),

    #[error("\"{0}\" has no attribute \"{1}\"")]
    AttributeNotFound(String, String),

    #[error("{0}")]
    IncompatibleTypesError(String),

    #[error("unknown identifier: \"{0}\"")]
    UnknownIdentifier(String),
}

pub trait OrError<T> {
    fn or_error(self, s: String) -> Result<T, Error>;
}

impl<T> OrError<T> for Result<T, VinegarError> {
    fn or_error(self, s: String) -> Result<T, Error> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(Error::VinegarError(s, err)),
        }
    }
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

pub fn get_error_prefix(debug_info: &DebugInfo, range: &Range<usize>) -> String {
    if let None = debug_info.source {
        return "".to_string();
    }

    let mut line = 0;
    let mut column = 0;
    let mut column_start = 0;
    let s: &String = debug_info
        .source
        .as_ref()
        .expect("suddenly there was no string");
    for (i, char) in s.chars().take(range.start).enumerate() {
        column += 1;
        if char == '\n' {
            line += 1;
            column = 0;
            column_start = i + 1;
        }
    }
    let mut column_end = range.start + 1;
    let mut chars = s.chars();
    chars.nth(range.start);
    loop {
        match chars.next() {
            None => break,
            Some('\n') => {
                break;
            }
            Some(_) => {
                column_end += 1;
            }
        }
    }

    let file_info = format!("in {},", debug_info.source_name,);

    if range.start >= range.end {
        return format!("{} unknown position:", file_info);
    }

    let indent = 4;

    let mut rv = format!("{} line {}, column {}:", file_info, line + 1, column + 1,);
    rv = format!(
        "{}\n\n{}{}",
        rv,
        " ".repeat(indent),
        s[column_start..column_end].to_string()
    );

    let squiggle_start = range.start - column_start + indent;
    let squiggle_len = range.end - range.start;

    rv = format!(
        "{}\n{}{}",
        rv,
        " ".repeat(squiggle_start),
        "~".repeat(squiggle_len).bold().bright_red()
    );

    rv = format!("{}\n", rv);
    rv
}
