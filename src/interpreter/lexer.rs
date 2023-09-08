extern crate lazy_static;
use super::debug::FileInterpreterError;
use super::{debug::DebugInfo, debug::InterpreterError};
use crate::{debug, file_handler};
use lazy_static::lazy_static;
use std::char;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

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
        set
    };
}

#[derive(Debug)]
pub struct TokenWord {
    pub word: String,
}

#[derive(Debug)]
pub struct TokenNewLine {
    pub indent: usize,
}

#[derive(Debug)]
pub struct TokenExplicitString {
    pub string: String,
}

#[derive(Debug)]
pub struct TokenChar {
    pub character: char,
}

#[derive(Debug)]
pub enum Token {
    Word(TokenWord),
    NewLine(TokenNewLine),
    ExplicitString(TokenExplicitString),
    ParenOpen(TokenChar),
    ParenClose(TokenChar),
    Colon(TokenChar),
    Equals(TokenChar),
    Comma(TokenChar),
    Plus(TokenChar),
    Minus(TokenChar),
    Star(TokenChar),
    Slash(TokenChar),
    Pipe(TokenChar),
    Hashtag(TokenChar),
}

#[derive(Debug)]
pub struct DebugToken {
    pub token: Token,
    pub char_range: Range<usize>,
}

trait VinegarChar {
    fn is_word_char(&self) -> bool;
}

impl VinegarChar for char {
    fn is_word_char(&self) -> bool {
        self.is_alphanumeric() || WORD_CHARS.contains(self)
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
                None => DebugInfo::new("(unknown source)".to_string()),
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
            debug_info: DebugInfo::new(format!("\"{}\"", path)),
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
            all_tokens.push(self.next_token(current)?);
        }

        Ok(all_tokens)
    }

    fn next_token(&mut self, first: char) -> Result<DebugToken, InterpreterError> {
        let start_pointer = self.pointer;

        if first.is_word_char() {
            self.next_word(start_pointer)
        } else if first == '\n' {
            self.next_newline(start_pointer)
        } else if first == '"' || first == '\'' {
            self.next_explicit_string(first, start_pointer)
        } else {
            self.next_single_char(first, start_pointer)
        }
    }

    fn next_word(
        &mut self,
        start_pointer: usize,
    ) -> Result<DebugToken, InterpreterError> {
        let mut word = String::new();

        while let Some(&character) = self.next() {
            if !character.is_word_char() {
                break;
            }
            word.push(character);
        }
        return Ok(DebugToken {
            token: Token::Word(TokenWord { word: word }),
            char_range: start_pointer..self.pointer,
        });
    }

    fn next_newline(&mut self, start_pointer: usize) -> Result<DebugToken, InterpreterError> {
        let mut indent = 0;
        let mut current = match self.next() {
            Some(c) => c,
            None => {
                return Ok(DebugToken {
                    token: Token::NewLine(TokenNewLine { indent: 0 }),
                    char_range: start_pointer..self.pointer,
                });
            }
        };
        loop {
            indent += match SPACE_CHARS.get(&current) {
                Some(i) => i,
                None => break,
            };
            if let Some(next_char) = self.next() {
                current = next_char;
            } else {
                break;
            }
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
            }
            string.push(character);
        }

        return match next {
            Some(_) => {
                self.next();
                Ok(DebugToken {
                    token: Token::ExplicitString(TokenExplicitString { string: string }),
                    char_range: start_pointer..self.pointer,
                })
            }
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
        let char_token = TokenChar { character: first };
        let token = match first {
            '(' => Some(Token::ParenOpen(char_token)),
            ')' => Some(Token::ParenClose(char_token)),
            '=' => Some(Token::Equals(char_token)),
            ':' => Some(Token::Colon(char_token)),
            ',' => Some(Token::Comma(char_token)),
            '+' => Some(Token::Plus(char_token)),
            '-' => Some(Token::Minus(char_token)),
            '*' => Some(Token::Star(char_token)),
            '/' => Some(Token::Slash(char_token)),
            '|' => Some(Token::Pipe(char_token)),
            '#' => Some(Token::Hashtag(char_token)),
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

        format!("in {}, line {}, column {}:", self.debug_info.source_name, line + 1, column)
    }
}
