use thiserror::Error;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("{0}\nInvalid character: \'{1}\'")]
    InvalidCharacterError(String, String),

    #[error("{0}\nunexpected end of file.{1}")]
    UnexpectedEOFError(String, String),
}

pub fn debug_char_repr(c: char) -> String {
    c.escape_debug().to_string().replace("\\\"", "\"")
}
