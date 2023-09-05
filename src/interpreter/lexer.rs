use std::{iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TokenizerError {
    #[error("{0}\nInvalid character: {1}")]
    InvalidCharacterError(String, char),
}

#[derive(Debug)]
struct TokenWord {
    word: String,
}

#[derive(Debug)]
pub enum Token {
    Word(TokenWord),
}

pub struct Lexer<'a> {
    characters: Peekable<Chars<'a>>,
    string: &'a str,
    pointer: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a str) -> Self {
        Lexer {
            characters: string.chars().peekable(),
            string: string,
            pointer: 0,
        }
    }

    pub fn get_tokens(&mut self) -> Result<Vec<Token>, TokenizerError> {
        let mut all_tokens = Vec::new();
        let mut next = self.peek().map(|&x| x);
        while let Some(current) = next {
            match self.next_token(current) {
                Ok((value, last)) => {
                    next = last;
                    all_tokens.push(value)
                }
                Err(err) => {
                    return Err(err)
                }
            }
        }

        Ok(all_tokens)
    }

    fn next_token(&mut self, first: char) -> Result<(Token, Option<char>), TokenizerError> {
        if first.is_alphabetic() {
            let mut word = String::new();
            let mut next = None;
            while let Some(character) = self.next() {
                println!("{}", character);
                if !character.is_alphanumeric() {
                    next = Some(character);
                    break;
                }
                word.push(character);
            }
            return Ok(
                (Token::Word(TokenWord { word: word }), next)
            );
        }

        Err(TokenizerError::InvalidCharacterError(
            self.get_error_prefix(),
            first,
        ))
    }

    fn peek(&mut self) -> Option<&char> {
        self.characters.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.pointer += 1;
        self.characters.next()
    }

    fn get_error_prefix(&self) -> String {
        let mut line = 0;
        let mut column = 0;
        for char in self.string.chars().take(self.pointer) {
            column += 1;
            if char == '\n' {
                line += 1;
                column = 0;
            }
        }

        format!("in line {}, column {}:", line, column)
    }
}
