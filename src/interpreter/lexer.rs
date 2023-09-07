extern crate lazy_static;
use crate::interpreter::error_handling::{InterpreterError};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ops::Range;


use super::error_handling::debug_char_repr;

lazy_static! {
    static ref SPACE_CHARS: HashMap<char, usize> = {
        let mut set = HashMap::new();
        set.insert(' ', 1);
        set.insert('\t', 4);
        set
    };
}

#[derive(Debug)]
pub struct TokenWord {
    word: String,
    char_range: Range<usize>,
}

#[derive(Debug)]
pub struct TokenSpace {
    pub size: usize,
    pub char_range: Range<usize>,
}

#[derive(Debug)]
pub struct TokenExplicitString {
    pub string: String,
    pub char_range: Range<usize>,
}

#[derive(Debug)]
pub struct TokenSingleChar {
    pub char: char,
    pub char_range: Range<usize>,
}

#[derive(Debug)]
pub enum Token {
    Word(TokenWord),
    Space(TokenSpace),
    NewLine(TokenSingleChar),
    ExplicitString(TokenExplicitString),
    Colon(TokenSingleChar),
    Equals(TokenSingleChar),
    Comma(TokenSingleChar),
    Plus(TokenSingleChar),
    Minus(TokenSingleChar),
    Star(TokenSingleChar),
    Slash(TokenSingleChar),
}

pub struct Lexer {
    string: Vec<char>,
    pointer: usize,
}

impl Lexer {
    pub fn lex(string: String) -> Result<Vec<Token>, InterpreterError> {
        let mut instance = Self {
            string: string.chars().collect(),
            pointer: 0,
        };
        instance.get_tokens()
    }

    fn get_tokens(&mut self) -> Result<Vec<Token>, InterpreterError> {
        let mut all_tokens = Vec::new();
        self.next();
        while let Some(&current) = self.prev() {
            match self.next_token(current) {
                Ok(value) => all_tokens.push(value),
                Err(err) => return Err(err),
            }
        }

        Ok(all_tokens)
    }

    fn next_token(&mut self, first: char) -> Result<Token, InterpreterError> {
        let start_pointer = self.pointer;

        if first.is_alphanumeric() {
            let mut word = String::new();
            word.push(first);

            while let Some(&character) = self.next() {
                if !character.is_alphanumeric() {
                    break;
                }
                word.push(character);
            }
            return Ok(Token::Word(TokenWord {
                word: word,
                char_range: start_pointer..self.pointer,
            }));
        } else if SPACE_CHARS.contains_key(&first) {
            self.next();
            return Ok(Token::Space(TokenSpace {
                size: SPACE_CHARS
                    .get(&first)
                    .map(|&x| x)
                    .expect("key magically vanished"),
                char_range: start_pointer..self.pointer,
            }));
        } else if first == '"' || first == '\'' {
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
                    Ok(Token::ExplicitString(TokenExplicitString {
                        string: string,
                        char_range: start_pointer..self.pointer,
                    }))
                }
                None => Err(InterpreterError::UnexpectedEOFError(
                    self.get_error_prefix(),
                    format!(" expected '{}'", debug_char_repr(quote_type),),
                )),
            };
        } else {
            self.next();
            let token = TokenSingleChar {
                char: first,
                char_range: start_pointer..self.pointer,
            };
            return match first {
                '\n' => Ok(Token::NewLine(token)),
                '=' => Ok(Token::Equals(token)),
                ':' => Ok(Token::Colon(token)),
                ',' => Ok(Token::Comma(token)),
                '+' => Ok(Token::Plus(token)),
                '-' => Ok(Token::Minus(token)),
                '*' => Ok(Token::Star(token)),
                '/' => Ok(Token::Slash(token)),
                _ => Err(InterpreterError::InvalidCharacterError(
                    self.get_error_prefix(),
                    debug_char_repr(first),
                )),
            };
        }
    }

    fn prev(&self) -> Option<&char> {
        self.string.get(self.pointer - 1)
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

        format!("in line {}, column {}:", line + 1, column)
    }
}
