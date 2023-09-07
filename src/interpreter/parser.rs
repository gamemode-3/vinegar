use crate::interpreter::error_handling::InterpreterError;
use crate::interpreter::lexer::{
    Lexer, Token,
};



pub struct CodeBody {

}

pub struct Parser {
    tokens: Vec<Token>,
    pointer: usize,
}

impl Parser {
    pub fn parse_tokens(tokens: Vec<Token>) -> Result<CodeBody, InterpreterError> {
        let mut instance: Parser = Self {
            tokens,
            pointer: 0,
        };
        let result = instance.get_runnable();
        result
    }

    pub fn parse_string(s: String) -> Result<CodeBody, InterpreterError> {
        let tokens = Lexer::lex(s)?;
        let result = Self::parse_tokens(tokens);
        result
    }

    pub fn get_runnable(&mut self) -> Result<CodeBody, InterpreterError> {
        self.next_code_body()
    }

    fn next_code_body(&mut self) -> Result<CodeBody, InterpreterError> {
        let indent: usize = match self.next() {
            Some(Token::Space(space)) => space.size,
            Some(_) => 0,
            None => return Ok(CodeBody {})
        };

        println!("{}", indent);

        unimplemented!()
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.pointer - 1)
    }

    fn next(&mut self) -> Option<&Token> {
        let rv = self.tokens.get(self.pointer);
        self.pointer += 1;
        rv
    }
}
