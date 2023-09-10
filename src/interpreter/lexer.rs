extern crate lazy_static;
use super::debug::FileInterpreterError;
use super::{debug::DebugInfo, debug::InterpreterError};
use crate::{debug, file_handler};
use lazy_static::lazy_static;
use std::char;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use unescaper::unescape;

lazy_static! {
    static ref SPACE_CHARS: HashMap<char, usize> = {
        let mut set = HashMap::new();
        set.insert(' ', 1);
        set.insert('\t', 4);
        set
    };
}

lazy_static! {
    static ref WORD_CHARS: HashSet<char> = {
        let mut set = HashSet::new();
        set.insert('_');
        set.insert('#');
        set
    };
}

lazy_static! {
    static ref NUM_CHARS: HashSet<char> = {
        let mut set = HashSet::new();
        set.insert('e');
        set
    };
}

#[derive(Debug)]
pub struct TokenNewLine {
    pub indent: usize,
}

#[derive(Debug)]
pub enum Token {
    Word(String),
    NewLine(TokenNewLine),
    StringLiteral(String),
    ParenOpen,
    ParenClose,
    Colon,
    Period,
    Equals,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Pipe,
}

impl Token {
    pub fn simple_string(&self) -> String {
        match self {
            Token::Word(s) => format!("\"{}\"", s),
            Token::NewLine(n) => format!("<newline {}>", n.indent),
            Token::StringLiteral(s) => format!("<string_literal \"{}\">", s),
            Token::ParenOpen => ("\'(\'").into(),
            Token::ParenClose => ("\')\'").into(),
            Token::Colon => ("\':\'").into(),
            Token::Period => ("\'.\'").into(),
            Token::Equals => ("\'=\'").into(),
            Token::Comma => ("\',\'").into(),
            Token::Plus => ("\'+\'").into(),
            Token::Minus => ("\'-\'").into(),
            Token::Star => ("\'*\'").into(),
            Token::Slash => ("\'/\'").into(),
            Token::Pipe => ("\'|\'").into(),
        }
    }
}

#[derive(Debug)]
pub struct DebugToken {
    pub token: Token,
    pub char_range: Range<usize>,
}

trait VinegarChar {
    fn is_word_char_alphanum(&self) -> bool;
    fn is_word_char_num(&self) -> bool;
}

impl VinegarChar for char {
    fn is_word_char_alphanum(&self) -> bool {
        self.is_alphanumeric() || WORD_CHARS.contains(self)
    }

    fn is_word_char_num(&self) -> bool {
        self.is_numeric() || NUM_CHARS.contains(self)
    }
}

pub trait ContainsRange {
    fn contains_range(&self, other: &Self) -> bool;
}

impl<T> ContainsRange for Range<T>
where
    T: PartialOrd,
{
    fn contains_range(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

trait SliceIfContained<T> {
    fn slice_if_contained(&self, range: Range<usize>) -> Option<&[T]>;
}

impl<T> SliceIfContained<T> for Vec<T> {
    fn slice_if_contained(&self, range: Range<usize>) -> Option<&[T]> {
        if (0..self.len()).contains_range(&range) {
            return Some(&self[range]);
        }
        return None;
    }
}

pub struct Lexer {
    debug_info: DebugInfo,
    string: Vec<char>,
    pointer: usize,
}

impl Lexer {
    pub fn lex_string(
        string: String,
        debug_info: Option<DebugInfo>,
    ) -> Result<Vec<DebugToken>, InterpreterError> {
        let mut instance = Self {
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new(Some(string.clone()), "(unknown source)".to_string()),
            },
            string: string.chars().collect(),
            pointer: 0,
        };
        instance.get_tokens()
    }

    pub fn lex_file(path: String) -> Result<Vec<DebugToken>, FileInterpreterError> {
        let file_content = match file_handler::get_file_contents(&path) {
            Ok(contents) => contents,
            Err(err) => return Err(FileInterpreterError::IOError(err)),
        };
        let mut instance = Self {
            debug_info: DebugInfo::new(Some(file_content.clone()), format!("\"{}\"", path)),
            string: file_content.chars().collect(),
            pointer: 0,
        };
        match instance.get_tokens() {
            Ok(tokens) => Ok(tokens),
            Err(err) => Err(FileInterpreterError::InterpreterError(err)),
        }
    }

    fn get_tokens(&mut self) -> Result<Vec<DebugToken>, InterpreterError> {
        let mut all_tokens: Vec<DebugToken> = Vec::new();
        all_tokens.push(self.next_newline(self.pointer)?);
        if self.pointer > 0 {
            self.pointer -= 1
        }

        while let Some(&current) = self.peek() {
            if current == ' ' {
                self.next();
                continue;
            }
            if let Some(token) = self.next_token(current)? {
                all_tokens.push(token);
            }
        }

        Ok(all_tokens)
    }

    fn next_token(&mut self, first: char) -> Result<Option<DebugToken>, InterpreterError> {
        let start_pointer = self.pointer;
        if first.is_word_char_num() {
            Ok(Some(self.next_number(start_pointer)?))
        } else if first.is_word_char_alphanum() {
            self.next_word(start_pointer)
        } else if first == '\n' {
            self.next();
            Ok(Some(self.next_newline(start_pointer)?))
        } else if first == '"' || first == '\'' {
            Ok(Some(self.next_explicit_string(first, start_pointer)?))
        } else {
            Ok(Some(self.next_single_char(first, start_pointer)?))
        }
    }

    fn next_word(&mut self, start_pointer: usize) -> Result<Option<DebugToken>, InterpreterError> {
        let mut word = String::new();

        if self.peek() == Some(&'#') {
            match self.peek_n(1) {
                Some(' ') => {
                    loop {
                        let peek = self.peek();
                        if peek == Some(&'\n') || peek == None {
                            break;
                        }
                        self.next();
                    }
                    return Ok(None);
                }
                Some('#') => {
                    if self.peek_n(2) == Some(&'(') {
                        while !(self
                            .string
                            .slice_if_contained(self.pointer..self.pointer + 3)
                            == Some(&[')', '#', '#']))
                        {
                            self.next();
                        }
                        self.next();
                        self.next();
                        self.next();
                        return Ok(None);
                    }
                }
                _ => (),
            }
        }
        while let Some(&character) = self.peek() {
            if !character.is_word_char_alphanum() {
                break;
            }
            self.next();
            word.push(character);
        }
        return Ok(Some(DebugToken {
            token: Token::Word(word),
            char_range: start_pointer..self.pointer,
        }));
    }

    fn next_number(&mut self, start_pointer: usize) -> Result<DebugToken, InterpreterError> {
        let mut word = String::new();

        while let Some(&character) = self.peek() {
            if !character.is_word_char_num() {
                break;
            }
            self.next();
            word.push(character);
        }
        return Ok(DebugToken {
            token: Token::Word(word),
            char_range: start_pointer..self.pointer,
        });
    }

    fn next_newline(&mut self, start_pointer: usize) -> Result<DebugToken, InterpreterError> {
        let mut indent = 0;
        while let Some(next_char) = self.peek() {
            indent += match SPACE_CHARS.get(next_char) {
                Some(i) => i,
                None => break,
            };
            self.next();
        }
        return Ok(DebugToken {
            token: Token::NewLine(TokenNewLine { indent }),
            char_range: start_pointer..self.pointer,
        });
    }

    fn next_explicit_string(
        &mut self,
        first: char,
        start_pointer: usize,
    ) -> Result<DebugToken, InterpreterError> {
        self.next();

        let quote_type = first;

        let mut string = String::new();

        let mut next = None;
        let mut escaping_quotes = false;
        while let Some(&character) = self.next() {
            if character == quote_type {
                if !escaping_quotes {
                    next = Some(character);
                    break;
                }
                escaping_quotes = false;
            }
            if character == '\\' {
                escaping_quotes = !escaping_quotes;
            } else {
                escaping_quotes = false;
            }
            string.push(character);
        }

        let unescaped_string = match unescape(&string) {
            Ok(s) => s,
            Err(_) => {
                return Err(InterpreterError::InvalidLiteralError(
                    self.get_error_prefix(),
                    format!("\"{}\"", string),
                    "".into(),
                ))
            }
        };

        return match next {
            Some(_) => Ok(DebugToken {
                token: Token::StringLiteral(unescaped_string),
                char_range: start_pointer..self.pointer,
            }),
            None => Err(InterpreterError::UnexpectedEndOfFileError(
                self.get_error_prefix(),
                format!("expected '{}'.", debug::char_repr(quote_type),),
            )),
        };
    }

    fn next_single_char(
        &mut self,
        first: char,
        start_pointer: usize,
    ) -> Result<DebugToken, InterpreterError> {
        self.next();
        let token = match first {
            '(' => Some(Token::ParenOpen),
            ')' => Some(Token::ParenClose),
            '=' => Some(Token::Equals),
            ':' => Some(Token::Colon),
            '.' => Some(Token::Period),
            ',' => Some(Token::Comma),
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(Token::Slash),
            '|' => Some(Token::Pipe),
            _ => None,
        };
        match token {
            Some(t) => Ok(DebugToken {
                token: t,
                char_range: start_pointer..self.pointer,
            }),
            None => Err(InterpreterError::InvalidCharacterError(
                self.get_error_prefix(),
                debug::char_repr(first),
            )),
        }
    }

    fn peek(&self) -> Option<&char> {
        self.string.get(self.pointer)
    }

    fn peek_n(&self, n: usize) -> Option<&char> {
        self.string.get(self.pointer + n)
    }

    fn next(&mut self) -> Option<&char> {
        let rv = self.string.get(self.pointer);
        self.pointer += 1;
        rv
    }

    fn get_error_prefix(&self) -> String {
        let mut line = 0;
        let mut column = 0;
        for &char in self.string.iter().take(self.pointer) {
            column += 1;
            if char == '\n' {
                line += 1;
                column = 0;
            }
        }

        format!(
            "in {}, line {}, column {}:",
            self.debug_info.source_name,
            line + 1,
            column
        )
    }
}
