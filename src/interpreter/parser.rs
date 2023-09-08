use crate::file_handler;
use crate::interpreter::debug::InterpreterError;
use crate::interpreter::lexer::{DebugToken, Lexer, Token};
use std::rc::Rc;

use super::debug::{DebugInfo, FileInterpreterError};

#[derive(Debug)]
pub struct CodeBody {
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct LiteralString {
    // WORD(value)
    value: String,
}

#[derive(Debug)]
pub struct LiteralInt {
    // WORD(value)
    value: i64,
}

#[derive(Debug)]
pub struct LiteralFloat {
    // WORD(value)
    value: f64,
}

#[derive(Debug)]
pub enum Literal {
    String(LiteralString),
    Int(LiteralInt),
    Float(LiteralFloat),
}

#[derive(Debug)]
pub struct BinOpAdd {
    // <Expression>(a) PLUS <Expression>(b)
    a: Expression,
    b: Expression,
}

#[derive(Debug)]
pub struct BinOpSubtract {
    // <Expression>(a) MINUS <Expression>(b)
    a: Expression,
    b: Expression,
}

#[derive(Debug)]
pub struct BinOpMultiply {
    // <Expression>(a) STAR <Expression>(b)
    a: Expression,
    b: Expression,
}

#[derive(Debug)]
pub struct BinOpDivide {
    // <Expression>(a) SLASH <Expression>(b)
    a: Expression,
    b: Expression,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add(BinOpAdd),
    Subtract(BinOpSubtract),
    Multiply(BinOpMultiply),
    Divide(BinOpDivide),
}

#[derive(Debug)]
pub struct FunctionCall {}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    BinaryOperator(Rc<BinaryOperator>),
    FunctionCall(Rc<FunctionCall>),
}

impl From<LiteralString> for Expression {
    fn from(value: LiteralString) -> Self {
        Expression::Literal(Literal::String(value))
    }
}

impl From<LiteralInt> for Expression {
    fn from(value: LiteralInt) -> Self {
        Expression::Literal(Literal::Int(value))
    }
}

impl From<LiteralFloat> for Expression {
    fn from(value: LiteralFloat) -> Self {
        Expression::Literal(Literal::Float(value))
    }
}

impl From<BinOpAdd> for Expression {
    fn from(value: BinOpAdd) -> Self {
        Expression::BinaryOperator(Rc::new(BinaryOperator::Add(value)))
    }
}

impl From<BinOpSubtract> for Expression {
    fn from(value: BinOpSubtract) -> Self {
        Expression::BinaryOperator(Rc::new(BinaryOperator::Subtract(value)))
    }
}

impl From<BinOpMultiply> for Expression {
    fn from(value: BinOpMultiply) -> Self {
        Expression::BinaryOperator(Rc::new(BinaryOperator::Multiply(value)))
    }
}

impl From<BinOpDivide> for Expression {
    fn from(value: BinOpDivide) -> Self {
        Expression::BinaryOperator(Rc::new(BinaryOperator::Divide(value)))
    }
}

impl From<FunctionCall> for Expression {
    fn from(value: FunctionCall) -> Self {
        Expression::FunctionCall(Rc::new(value))
    }
}

#[derive(Debug)]
pub struct Assignment {
    // WORD(name) EQUALS <Expression>(value)
    name: String,
    value: Expression,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Assignment(Assignment),
}

impl From<Expression> for Statement {
    fn from(value: Expression) -> Self {
        Statement::Expression(value)
    }
}

impl From<Assignment> for Statement {
    fn from(value: Assignment) -> Self {
        Statement::Assignment(value)
    }
}

pub struct Parser {
    debug_info: DebugInfo,
    string: Option<String>,
    tokens: Vec<DebugToken>,
    pointer: usize,
}

impl Parser {
    pub fn parse_tokens(
        tokens: Vec<DebugToken>,
        string: Option<String>,
        debug_info: Option<DebugInfo>,
    ) -> Result<CodeBody, InterpreterError> {
        let mut instance: Parser = Self {
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new("(unknown source)".to_string()),
            },
            string: string,
            tokens: tokens,
            pointer: 0,
        };
        let result = instance.get_runnable();
        result
    }

    pub fn parse_string(
        s: String,
        debug_info: Option<DebugInfo>,
    ) -> Result<CodeBody, InterpreterError> {
        let tokens = Lexer::lex_string(s.clone(), debug_info.clone())?;
        let result: Result<CodeBody, InterpreterError> =
            Self::parse_tokens(tokens, Some(s), debug_info);
        result
    }

    pub fn parse_file(path: String) -> Result<CodeBody, FileInterpreterError> {
        let file_content = match file_handler::get_file_contents(&path) {
            Ok(contents) => contents,
            Err(err) => return Err(FileInterpreterError::IOError(err)),
        };
        let tokens: Vec<DebugToken> = Lexer::lex_file(path.clone())?;
        let result = match Self::parse_tokens(
            tokens,
            Some(file_content),
            Some(DebugInfo::new(format!("\"{}\"", path))),
        ) {
            Ok(v) => Ok(v),
            Err(err) => Err(FileInterpreterError::InterpreterError(err)),
        };
        result
    }

    pub fn get_runnable(&mut self) -> Result<CodeBody, InterpreterError> {
        self.next_code_body()
    }

    fn next_code_body(&mut self) -> Result<CodeBody, InterpreterError> {
        let indent: usize = match self.next_token() {
            Some(Token::NewLine(s)) => s.indent,
            Some(_) => 0,
            None => return Ok(CodeBody { statements: vec![] }),
        };

        let mut statements = vec![];

        loop {
            let this_statement = self.next_statement()?;
            if let None = this_statement {
                break;
            }
            statements.push(this_statement.expect("idk how but of something became nothing"));
            match self.next_token() {
                Some(Token::NewLine(n)) => {
                    if n.indent < indent {
                        break;
                    };
                    if n.indent > indent {};
                }
                Some(_) => {
                    return Err(InterpreterError::UnexpectedEndOfStatementError(
                        self.get_error_prefix(),
                        "expected the beginning of a new line.".to_string(),
                    ))
                }
                None => break,
            }
        }

        Ok(CodeBody {
            statements: statements,
        })
    }

    fn next_statement(&mut self) -> Result<Option<Statement>, InterpreterError> {
        let next_token = self.peek_token();
        if let Some(Token::Word(first_word)) = next_token {
            let word = first_word.word.clone();
            if let Some(Token::Equals(_)) = self.peek_n_token(1) {
                return Ok(Some(Statement::Assignment(
                    self.next_assignment(word)?,
                )));
            }
        };
        Ok(None)
    }

    fn next_assignment(&mut self, identifier: String) -> Result<Assignment, InterpreterError> {
        self.next();
        self.next();
        let expression = match self.next_expression()? {
            Some(e) => e,
            None => {
                return Err(InterpreterError::ExpectedExpressionError(
                    self.get_error_prefix(),
                ))
            }
        };
        return Ok(Assignment {
            name: identifier,
            value: expression,
        });
    }

    fn next_expression(&mut self) -> Result<Option<Expression>, InterpreterError> {
        match self.next_token() {
            Some(Token::Word(w)) => {
                let word = w.word.clone();
                Ok(Some(Expression::from(LiteralString { value: word })))
            }
            Some(Token::Hashtag(_)) => {
                let value = match self.next_token() {
                    Some(Token::Word(w)) => w.word.clone(),
                    _ => {
                        return Err(InterpreterError::MissingTokenError(
                            self.get_error_prefix(),
                            "expected hexadecimal literal after hashtag".into(),
                        ))
                    }
                };
                println!("ex{}", format!("{:?}", self.prev_token()));
                let int_value = match i64::from_str_radix(&value, 16) {
                    Ok(int_value) => int_value,
                    Err(_) => return Err(InterpreterError::MissingTokenError(
                            self.get_error_prefix(),
                            "expected hexadecimal literal after hashtag".into(),
                        ))
                };
                Ok(Some(Expression::from(LiteralInt { value: int_value })))
            }
            Some(Token::ExplicitString(w)) => {
                let string = w.string.clone();
                Ok(Some(Expression::from(LiteralString { value: string })))
            }
            _ => {
                println!("{}", format!("{:?}", self.prev_token()));
                return Err(InterpreterError::UnexpectedEndOfStatementError(
                    self.get_error_prefix(),
                    "".into(),
                ));
            }
        }
    }

    fn prev(&self) -> Option<&DebugToken> {
        self.tokens.get(self.pointer - 1)
    }

    fn peek(&self) -> Option<&DebugToken> {
        self.tokens.get(self.pointer)
    }

    fn peek_n(&self, n: usize) -> Option<&DebugToken> {
        self.tokens.get(self.pointer + n)
    }

    fn next(&mut self) -> Option<&DebugToken> {
        let rv = self.tokens.get(self.pointer);
        self.pointer += 1;
        rv
    }

    fn prev_token(&self) -> Option<&Token> {
        self.get_token(self.pointer - 1)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.get_token(self.pointer)
    }

    fn peek_n_token(&self, n: usize) -> Option<&Token> {
        self.get_token(self.pointer + n)
    }

    fn next_token(&mut self) -> Option<&Token> {
        let rv = match self.tokens.get(self.pointer) {
            Some(token) => Some(&token.token),
            None => None,
        };
        self.pointer += 1;
        rv
    }

    fn get_token(&self, index: usize) -> Option<&Token> {
        match self.tokens.get(index) {
            Some(token) => Some(&token.token),
            None => None,
        }
    }

    fn get_error_prefix(&self) -> String {
        if let None = self.string {
            return "".to_string();
        }

        let mut line = 0;
        let mut column = 0;
        let s: &String = &self.string.as_ref().expect("suddenly there was no string");
        let iter: Box<dyn Iterator<Item = char>> = match self.prev() {
            Some(t) => Box::new(s.chars().take(t.char_range.start)),
            None => Box::new(s.chars()),
        };
        for char in iter {
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
