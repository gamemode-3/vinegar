use super::debug::{get_error_prefix, DebugInfo, Error, FileOrOtherError, ParserError};
use super::iter::{CanConcatenate, SingleItemIterator};
use super::lexer::{DebugToken, Lexer, Token};
use super::string_literal_map::{MapStringLiterals, StringLiteralMap};
use crate::file_handler;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;
use std::vec;

macro_rules! expect_token {
    ($self:expr, $token_source:expr, $($pattern:pat $(if $cond:expr)? => $result:expr),* $(,)?; $msg:expr) => {
        match $token_source {
            $(
                Some($pattern) $(if $cond)? => $result,
            )*
            Some(t) => {
                let token_str = t.simple_string();
                return Err(Error::ParserError(
                    $self.get_error_prefix(),
                    ParserError::UnexpectedTokenError(
                    token_str,
                    $msg.to_string(),
                )));
            }
            _ => {
                return Err(Error::ParserError(
                    $self.get_error_prefix(),
                    ParserError::UnexpectedEndOfFileError(
                    $msg.to_string(),
                )));
            }
        }
    };
}

macro_rules! expect_any_token {
    ($self:expr, $token_source:expr, $($pattern:pat $(if $cond:expr)? => $result:expr),* $(,)?; $msg:expr) => {
        match $token_source {
            $(
                Some($pattern) $(if $cond)? => $result,
            )*
            _ => {
                return Err(Error::ParserError(
                    $self.get_error_prefix(),
                    ParserError::UnexpectedEndOfFileError(
                    $msg.to_string(),
                )));
            }
        }
    };
}

macro_rules! expect_expression {
    ($self:expr) => {
        match $self.next_expression(0)? {
            Some(e) => e,
            None => {
                return Err(Error::ParserError(
                    $self.get_error_prefix(),
                    ParserError::ExpectedExpressionError(),
                ))
            }
        }
    };
}

macro_rules! expect_atomic_expression {
    ($self:expr) => {
        match $self.next_atomic_expression()? {
            Some(e) => e,
            None => {
                return Err(Error::ParserError(
                    $self.get_error_prefix(),
                    ParserError::ExpectedExpressionError(),
                ))
            }
        }
    };
}

lazy_static! {
    static ref LITERAL_PREFIXES: HashMap<String, u32> = {
        let mut set = HashMap::new();
        set.insert("bin_".into(), 2);
        set.insert("dec_".into(), 10);
        set.insert("hex_".into(), 16);
        set.insert("#".into(), 16);
        set
    };
}

#[derive(Debug, Clone)]
pub struct CodeBody {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(u64, Range<usize>),
    Int(i64, Range<usize>),
    Float(f64, Range<usize>),
    Bool(bool, Range<usize>),
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub op_type: UnaryOperatorType,
    pub op_range: Range<usize>,
    pub expr: Rc<Expression>,
}

impl UnaryOperator {
    pub fn new(op_type: UnaryOperatorType, op_range: Range<usize>, expr: Expression) -> Self {
        UnaryOperator {
            op_type,
            op_range,
            expr: Rc::new(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorType {
    Not,
    Minus,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub op_type: BinaryOperatorType,
    pub op_range: Range<usize>,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}

impl BinaryOperator {
    pub fn new(
        op_type: BinaryOperatorType,
        op_range: Range<usize>,
        left: Expression,
        right: Expression,
    ) -> Self {
        BinaryOperator {
            op_type,
            op_range,
            left: Rc::new(left),
            right: Rc::new(right),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorType {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
}

impl BinaryOperator {
    pub fn get_precedence(token: &BinaryOperatorType) -> usize {
        match token {
            BinaryOperatorType::Or => 0,
            BinaryOperatorType::And => 1,
            BinaryOperatorType::Add => 2,
            BinaryOperatorType::Sub => 2,
            BinaryOperatorType::Mul => 3,
            BinaryOperatorType::Div => 3,
        }
    }

    pub fn get_operator_type(token: &Token) -> Option<BinaryOperatorType> {
        match token {
            Token::Plus => Some(BinaryOperatorType::Add),
            Token::Minus => Some(BinaryOperatorType::Sub),
            Token::Star => Some(BinaryOperatorType::Mul),
            Token::Slash => Some(BinaryOperatorType::Div),
            Token::Word(w) => match w.as_str() {
                "and" => Some(BinaryOperatorType::And),
                "or" => Some(BinaryOperatorType::Or),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallArg {
    pub debug_name: Option<(String, Range<usize>)>,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub expr: Rc<Expression>,
    pub args: Vec<FunctionCallArg>,
    pub char_range: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum Indentifier {
    Final(String, Range<usize>),
    Member(String, Rc<Expression>, Range<usize>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Indentifier),
    UnaryOperator(UnaryOperator),
    BinaryOperator(BinaryOperator),
    FunctionCall(FunctionCall),
}

pub trait DebugString {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String;
}

impl DebugString for Expression {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        match self {
            Expression::Identifier(i) => i.debug_string(string_literals),
            Expression::Literal(l) => l.debug_string(string_literals),
            Expression::BinaryOperator(b) => b.debug_string(string_literals),
            Expression::FunctionCall(f) => f.debug_string(string_literals),
            Expression::UnaryOperator(u) => u.debug_string(string_literals),
        }
    }
}

impl DebugString for Indentifier {
    fn debug_string(&self, _: &StringLiteralMap) -> String {
        match self {
            Indentifier::Member(name, member, _) => {
                format!("{}.{:?}", name, member)
            }
            Indentifier::Final(name, _) => format!("{}", name),
        }
    }
}

impl DebugString for Literal {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        match self {
            Literal::Int(i, _) => format!("{}", i),
            Literal::Float(f, _) => format!("{}", f),
            Literal::Bool(b, _) => format!("{}", b),
            Literal::String(s, _) => format!("{:?}", string_literals.get(s)),
        }
    }
}

impl DebugString for UnaryOperator {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        let expr = match self.expr.as_ref() {
            Expression::BinaryOperator(b) => format!("({})", b.debug_string(string_literals)),
            _ => self.expr.debug_string(string_literals),
        };
        match self.op_type {
            UnaryOperatorType::Not => format!("not {}", expr),
            UnaryOperatorType::Minus => format!("-{}", expr),
        }
    }
}

impl DebugString for BinaryOperator {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        let left = match self.left.as_ref() {
            Expression::BinaryOperator(b) => {
                if BinaryOperator::get_precedence(&self.op_type)
                    > BinaryOperator::get_precedence(&b.op_type)
                {
                    b.debug_string(string_literals);
                }
                format!("({})", b.debug_string(string_literals))
            }
            _ => self.left.debug_string(string_literals),
        };
        let right = match self.right.as_ref() {
            Expression::BinaryOperator(b) => {
                if BinaryOperator::get_precedence(&self.op_type)
                    > BinaryOperator::get_precedence(&b.op_type)
                {
                    b.debug_string(string_literals);
                }
                format!("({})", b.debug_string(string_literals))
            }
            _ => self.right.debug_string(string_literals),
        };
        match self.op_type {
            BinaryOperatorType::Add => format!("{} + {}", left, right),
            BinaryOperatorType::Sub => format!("{} - {}", left, right),
            BinaryOperatorType::Mul => format!("{} * {}", left, right),
            BinaryOperatorType::Div => format!("{} / {}", left, right),
            BinaryOperatorType::And => format!("{} && {}", left, right),
            BinaryOperatorType::Or => format!("{} || {}", left, right),
        }
    }
}

impl DebugString for FunctionCall {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        let expr = match self.expr.as_ref() {
            Expression::BinaryOperator(b) => format!("({})", b.debug_string(string_literals)),
            Expression::UnaryOperator(u) => format!("({})", u.debug_string(string_literals)),
            _ => self.expr.debug_string(string_literals),
        };
        format!(
            "{}({})",
            expr,
            self.args
                .iter()
                .map(|a| a.debug_string(string_literals))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl DebugString for FunctionCallArg {
    fn debug_string(&self, string_literals: &StringLiteralMap) -> String {
        format!("{}", self.expr.debug_string(string_literals))
    }
}

pub trait GetCharRange {
    fn get_char_range(&self) -> Range<usize>;
}

impl GetCharRange for Expression {
    fn get_char_range(&self) -> Range<usize> {
        match self {
            Expression::Identifier(i) => return i.get_char_range(),
            Expression::Literal(l) => l.get_char_range(),
            Expression::BinaryOperator(b) => b.get_char_range(),
            Expression::FunctionCall(f) => f.get_char_range(),
            Expression::UnaryOperator(u) => u.get_char_range(),
        }
    }
}

impl GetCharRange for Indentifier {
    fn get_char_range(&self) -> Range<usize> {
        match self {
            Indentifier::Final(_, range) => range.clone(),
            Indentifier::Member(_, _, range) => range.clone(),
        }
    }
}

impl GetCharRange for Literal {
    fn get_char_range(&self) -> Range<usize> {
        match self {
            Literal::Int(_, range) => range.clone(),
            Literal::Float(_, range) => range.clone(),
            Literal::Bool(_, range) => range.clone(),
            Literal::String(_, range) => range.clone(),
        }
    }
}

impl GetCharRange for UnaryOperator {
    fn get_char_range(&self) -> Range<usize> {
        self.op_range.start..self.expr.get_char_range().end
    }
}

impl GetCharRange for BinaryOperator {
    fn get_char_range(&self) -> Range<usize> {
        self.left.get_char_range().start..self.right.get_char_range().end
    }
}

impl GetCharRange for FunctionCall {
    fn get_char_range(&self) -> Range<usize> {
        self.char_range.clone()
    }
}

impl From<Literal> for Expression {
    fn from(value: Literal) -> Self {
        Expression::Literal(value)
    }
}

impl From<UnaryOperator> for Expression {
    fn from(value: UnaryOperator) -> Self {
        Expression::UnaryOperator(value)
    }
}

impl From<BinaryOperator> for Expression {
    fn from(value: BinaryOperator) -> Self {
        Expression::BinaryOperator(value)
    }
}

impl From<FunctionCall> for Expression {
    fn from(value: FunctionCall) -> Self {
        Expression::FunctionCall(value)
    }
}

impl From<Indentifier> for Expression {
    fn from(value: Indentifier) -> Self {
        Expression::Identifier(value)
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    // WORD(name) EQUALS <Expression>(value)
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinitionArg {
    pub name: String,
    pub default: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<FunctionDefinitionArg>,
    pub body: CodeBody,
}

#[derive(Debug, Clone)]
pub enum ElseBody {
    If(Rc<If>),
    CodeBody(CodeBody),
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub if_body: CodeBody,
    pub else_body: Option<ElseBody>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Assignment(Assignment),
    FunctionDefinition(FunctionDefinition),
    If(If),
    Return(Return),
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

impl From<FunctionDefinition> for Statement {
    fn from(value: FunctionDefinition) -> Self {
        Statement::FunctionDefinition(value)
    }
}

impl From<If> for Statement {
    fn from(value: If) -> Self {
        Statement::If(value)
    }
}

impl From<Return> for Statement {
    fn from(value: Return) -> Self {
        Statement::Return(value)
    }
}

trait IsNumeric {
    fn is_numeric(&self) -> bool;
}

impl IsNumeric for str {
    fn is_numeric(&self) -> bool {
        for char in self.chars() {
            if !char.is_numeric() {
                return false;
            }
        }
        return true;
    }
}

trait CanBeInt {
    fn can_be_int(&self) -> bool;
}

impl CanBeInt for f64 {
    fn can_be_int(&self) -> bool {
        self.fract() == 0.0 && self >= &(i64::MIN as f64) && self <= &(i64::MAX as f64)
    }
}

pub struct Parser {
    debug_info: DebugInfo,
    tokens: Vec<DebugToken>,
    string_literals: StringLiteralMap,
    string_hasher: DefaultHasher,
    pointer: usize,
    paren_depth: usize,
}

pub struct ParserResult {
    pub tree_root: CodeBody,
    pub string_literals: StringLiteralMap,
    pub string_hasher: DefaultHasher,
}

impl Debug for ParserResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tree_string = format!("{:?}", self.tree_root);
        let pattern = r#"Literal\(String\((\d+),"#;
        let re = Regex::new(pattern).unwrap();
        let result = re
            .replace_all(&tree_string, |caps: &regex::Captures| {
                if let Some(number_match) = caps.get(1) {
                    if let Ok(number) = number_match.as_str().parse::<u64>() {
                        if let Some(value) = self.string_literals.get(&number) {
                            return format!(
                                "Literal(String(<\"{}\"> {},",
                                value.escape_default(),
                                number
                            );
                        }
                    }
                }
                // If no replacement found, return the original capture
                caps[0].to_string()
            })
            .to_string();
        write!(f, "{}", result)
    }
}

impl Parser {
    pub fn parse_tokens(
        tokens: Vec<DebugToken>,
        debug_info: Option<DebugInfo>,
    ) -> Result<ParserResult, Error> {
        let mut instance: Parser = Self {
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new(None, "(unknown source)".to_string()),
            },
            tokens: tokens,
            string_literals: HashMap::default(),
            string_hasher: DefaultHasher::new(),
            pointer: 0,
            paren_depth: 0,
        };
        Ok(ParserResult {
            tree_root: instance.get_tree()?,
            string_literals: instance.string_literals,
            string_hasher: instance.string_hasher,
        })
    }

    #[allow(dead_code)]
    pub fn parse_string(s: String, debug_info: Option<DebugInfo>) -> Result<ParserResult, Error> {
        let tokens = Lexer::lex_string(s.to_string(), debug_info.clone())?;
        let result = Self::parse_tokens(tokens, debug_info);
        result
    }

    pub fn parse_file(path: String) -> Result<ParserResult, FileOrOtherError> {
        let file_content = match file_handler::get_file_contents(&path) {
            Ok(contents) => contents,
            Err(err) => return Err(FileOrOtherError::IOError(err)),
        };
        let tokens: Vec<DebugToken> = Lexer::lex_file(path.to_string())?;
        let result = match Self::parse_tokens(
            tokens,
            Some(DebugInfo::new(Some(file_content), format!("\"{}\"", path))),
        ) {
            Ok(v) => Ok(v),
            Err(err) => Err(FileOrOtherError::OtherError(err)),
        };
        result
    }

    pub fn get_tree(&mut self) -> Result<CodeBody, Error> {
        match self.peek_token() {
            Some(Token::NewLine(n)) => {
                if n.indent != 0 {
                    return Err(Error::ParserError(
                        self.get_error_prefix(),
                        ParserError::UnexpectedIndentError(),
                    ));
                }
            }
            None => return Ok(CodeBody { statements: vec![] }),
            Some(t) => {
                let token_str = t.simple_string();
                return Err(Error::ParserError(
                    self.get_error_prefix(),
                    ParserError::UnexpectedTokenError(
                        token_str,
                        "the first token must be a newline token with 0 indent.".into(),
                    ),
                ));
            }
        }
        let indent: usize = match self.next_token() {
            Some(Token::NewLine(s)) => s.indent,
            Some(_) => 0,
            None => return Ok(CodeBody { statements: vec![] }),
        };
        self.next_code_body(indent)
    }

    fn next_code_body(&mut self, indent: usize) -> Result<CodeBody, Error> {
        let mut statements = vec![];

        loop {
            let this_statement = self.next_statement(indent)?;
            match this_statement {
                None => break,
                Some(statement) => statements.push(statement),
            }
            match self.peek_token() {
                Some(Token::NewLine(n)) => {
                    if n.indent < indent {
                        break;
                    };
                    if n.indent > indent {
                        self.move_n(2);
                        return Err(Error::ParserError(
                            self.get_error_prefix(),
                            ParserError::UnexpectedIndentError(),
                        ));
                    };
                }
                Some(t) => {
                    let token_str = t.simple_string();
                    self.move_n(1);
                    return Err(Error::ParserError(
                        self.get_error_prefix(),
                        ParserError::UnexpectedTokenError(
                            token_str,
                            "expected the beginning of a new line.".to_string(),
                        ),
                    ));
                }
                None => break,
            }
        }

        Ok(CodeBody {
            statements: statements,
        })
    }

    fn next_statement(&mut self, indent: usize) -> Result<Option<Statement>, Error> {
        while let Some(Token::NewLine(_)) = self.peek_token() {
            self.next();
        }
        let next_token_option = self.peek_token();
        let next_token = match next_token_option {
            None => return Ok(None),
            Some(t) => t,
        };

        if let Token::Word(first_word) = next_token {
            // check for function definition pattern
            if first_word == "def" {
                return self.next_function_definition(indent);
            }

            // check for if statement pattern
            if first_word == "if" {
                return self.next_if_statement(indent);
            }

            // check for return statement pattern
            if first_word == "return" {
                return self.next_return();
            }

            // check for assignment pattern "WORD EQUALS ..."
            let word = first_word.to_string();
            if let Some(Token::Equals) = self.peek_n_token(1) {
                return Ok(Some(Statement::from(self.next_assignment(word)?)));
            }
        }

        return match self.next_expression(0)? {
            Some(expr) => Ok(Some(Statement::from(expr))),
            _ => Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::UnexpectedEndOfStatementError("expected a statement.".into()),
            )),
        };
    }

    fn next_assignment(&mut self, identifier: String) -> Result<Assignment, Error> {
        self.next();
        self.next();
        let expression = expect_expression!(self);
        return Ok(Assignment {
            name: identifier,
            value: expression,
        });
    }

    fn next_function_definition(&mut self, indent: usize) -> Result<Option<Statement>, Error> {
        if let (Some(Token::Word(name)), Some(Token::ParenOpen)) =
            (self.peek_n_token(1), self.peek_n_token(2))
        {
            let name = name.to_string();
            self.move_n(2);
            self.paren_depth += 1;
            self.next();

            let mut args = vec![];

            loop {
                let identifier = match self.peek_token() {
                    Some(Token::Word(w)) => w,
                    _ => break,
                }
                .clone();
                self.next();
                match self.peek_token() {
                    Some(Token::Colon) => {
                        self.next();
                        let expr = expect_expression!(self);
                        args.push(FunctionDefinitionArg {
                            name: identifier,
                            default: Some(expr),
                        });
                    }
                    _ => args.push(FunctionDefinitionArg {
                        name: identifier.clone(),
                        default: None,
                    }),
                };
                match self.peek_token() {
                    Some(Token::Comma) => {
                        self.next();
                    }
                    _ => break,
                }
            }
            expect_token!(
                self, self.next_token(),
                Token::ParenClose => ();
                "expected closing parenthesis."
            );
            self.paren_depth -= 1;

            expect_token!(
                self, self.next_token(),
                Token::Colon => ();
                "expected colon."
            );

            let mut next_indent = expect_token!(
                self, self.next_token(),
                Token::NewLine(n) => n;
                "expected new line."
            )
            .indent;

            while let Some(Token::NewLine(n)) = self.peek_token() {
                next_indent = n.indent;
                self.next();
            }

            if next_indent <= indent {
                return Err(Error::ParserError(
                    self.get_error_prefix(),
                    ParserError::ExpectedIndentError(
                        "expected indented block for function declaration.".into(),
                    ),
                ));
            }
            let body = self.next_code_body(next_indent)?;

            return Ok(Some(Statement::from(FunctionDefinition {
                name,
                args,
                body,
            })));
        }
        self.next();
        return Err(Error::ParserError(
            self.get_error_prefix(),
            ParserError::MissingTokenError(
                "expected function definition after keyword \"def\"".into(),
            ),
        ));
    }

    fn next_if_statement(&mut self, indent: usize) -> Result<Option<Statement>, Error> {
        Ok(Some(Statement::from(self.next_if(indent)?)))
    }

    fn next_if(&mut self, indent: usize) -> Result<If, Error> {
        self.next();
        let condition = expect_expression!(self);
        expect_token!(
            self, self.next_token(),
            Token::Colon => ();
            "expected colon."
        );
        let mut next_indent = expect_token!(
            self, self.next_token(),
            Token::NewLine(n) => n.indent;
            "expected new line."
        );
        while let Some(Token::NewLine(n)) = self.peek_token() {
            next_indent = n.indent; // TODO remove this
        }
        if next_indent <= indent {
            return Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::ExpectedIndentError(
                    "expected indented block for if statement.".into(),
                ),
            ));
        }
        let if_body = self.next_code_body(next_indent)?;
        let mut else_body = None;
        let next_indent = match self.peek_token() {
            Some(Token::NewLine(n)) => n.indent,
            Some(t) => return Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::UnexpectedTokenError(t.simple_string(), "expected new line.".into()),
            )),
            None => 0,
        };
        if next_indent < indent {
            return Ok(If {
                condition,
                if_body,
                else_body,
            });
        }
        if next_indent > indent {
            return Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::UnexpectedIndentError(),
            ));
        }
        if let Some(Token::Word(w)) = self.peek_n_token(1) {
            if w == "else" {
                self.move_n(2);
                else_body = Some(self.next_else(indent)?);
            }
        }
        return Ok(If {
            condition,
            if_body,
            else_body,
        });
    }

    fn next_else(&mut self, indent: usize) -> Result<ElseBody, Error> {
        expect_token!(
            self, self.peek_token(),
            Token::Word(w) if w == "if" => {
                return Ok(ElseBody::If(Rc::new(self.next_if(indent)?)));
            },
            Token::Colon => {
                self.next();
                return Ok(ElseBody::CodeBody(self.next_code_body(indent)?));
            };
            "expected \"if\" or colon."
        );
    }

    fn next_return(&mut self) -> Result<Option<Statement>, Error> {
        self.next();
        let value = self.next_expression(0)?;
        Ok(Some(Statement::from(Return { value })))
    }

    fn next_expression(&mut self, min_prec: usize) -> Result<Option<Expression>, Error> {
        let mut a = match self.next_atomic_expression()? {
            Some(a) => a,
            None => return Ok(None),
        };

        loop {
            match self.peek_token() {
                Some(Token::ParenOpen) => {
                    if let Some(Token::ParenOpen) = self.peek_token() {
                        a = self.next_function_call(a)?;
                    }
                }
                Some(Token::Period) => {
                    a = self.next_member(a)?;
                }
                Some(Token::ParenClose) => {
                    if self.paren_depth > 0 {
                        return Ok(Some(a));
                    } else {
                        return Err(Error::ParserError(
                            self.get_error_prefix(),
                            ParserError::UnexpectedTokenError(
                                Token::ParenClose.simple_string(),
                                "".into(),
                            ),
                        ));
                    }
                }
                Some(Token::NewLine(_)) => return Ok(Some(a)),
                None => return Ok(Some(a)),
                Some(_) => break,
            };
        }
        // if the next token is a binary operator with a precedence higher or equal to min_prec, then:
        //     consume and save it
        //     continue by evaluating the next expression
        loop {
            let next = match self.peek() {
                Some(token) => token,
                None => break,
            };
            let (op_type, op_range) = match BinaryOperator::get_operator_type(&next.token) {
                Some(op_type) => (op_type, next.char_range.clone()),
                None => break,
            };
            let op_prec = BinaryOperator::get_precedence(&op_type);
            if op_prec < min_prec {
                break;
            }
            self.next();

            a = match self.next_expression(op_prec + 1)? {
                Some(b) => Expression::from(BinaryOperator::new(op_type, op_range, a, b)),
                None => {
                    return Err(Error::ParserError(
                        self.get_error_prefix(),
                        ParserError::UnexpectedEndOfStatementError(format!(
                            "expected expression after operator {}",
                            self.prev_token().unwrap().simple_string()
                        )),
                    ))
                }
            };
        }

        Ok(Some(a))
    }

    fn next_function_call(&mut self, a: Expression) -> Result<Expression, Error> {
        let mut keyword_args = vec![];
        let mut positional_args = vec![];

        let start_char_pos = a.get_char_range().start;

        self.paren_depth += 1;
        loop {
            self.next();
            expect_any_token!(self, self.peek_token(),
                Token::ParenClose => break,
                _ => ();
                "expected expression or closing parentheses."
            );

            match self.peek_n_token(1) {
                Some(Token::Colon) => {
                    let name_debug = expect_any_token!(
                        self, self.next(),
                        a => a;
                        "expected keyword argument name before colon."
                    );
                    let debug_range = name_debug.char_range.clone();

                    let name = match &name_debug.token {
                        Token::Word(name) => name,
                        t => {
                            let token_string = t.simple_string();
                            return Err(Error::ParserError(
                                self.get_error_prefix(),
                                ParserError::UnexpectedTokenError(
                                    token_string,
                                    "expected keyword argument name before colon.".to_string(),
                                ),
                            ));
                        }
                    }
                    .clone();
                    self.next();
                    let expr = expect_expression!(self);
                    keyword_args.push(FunctionCallArg {
                        debug_name: Some((name, debug_range)),
                        expr: expr,
                    });
                }
                _ => positional_args.push(FunctionCallArg {
                    debug_name: None,
                    expr: expect_expression!(self),
                }),
            };
            expect_token!(self, self.peek_token(),
                Token::Comma => {
                    continue;
                },
                Token::ParenClose => break;
                "expected comma or closing parentheses."
            )
        }
        self.paren_depth -= 1;
        let ending_paren = self.next().unwrap();
        let mut all_args = keyword_args;
        all_args.extend(positional_args);
        return Ok(Expression::from(FunctionCall {
            expr: Rc::new(a),
            args: all_args,
            char_range: start_char_pos..ending_paren.char_range.end,
        }));
    }

    fn next_member(&mut self, a: Expression) -> Result<Expression, Error> {
        self.next();
        let next = match self.peek() {
            Some(t) => t,
            None => {
                return Err(Error::ParserError(
                    self.get_error_prefix(),
                    ParserError::UnexpectedEndOfFileError("expected a member identifier.".into()),
                ))
            }
        };
        match &next.token {
            Token::Word(w) => {
                self.assert_not_literal(&w)?;
                let name = w.to_string();
                let range = next.char_range.clone();
                self.next();
                return Ok(Expression::Identifier(Indentifier::Member(
                    name,
                    Rc::new(a),
                    range,
                )));
            }
            _ => Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::UnexpectedTokenError(
                    next.token.simple_string(),
                    "expected a member identifier.".into(),
                ),
            )),
        }
    }

    fn next_atomic_expression(&mut self) -> Result<Option<Expression>, Error> {
        let next_debug = self.next();
        let (next_token, next_range) = match next_debug {
            Some(d) => (&d.token, d.char_range.clone()),
            None => return Ok(None),
        };
        match next_token {
            Token::Word(w) => {
                let word = w.to_string();
                return self.next_word(word, next_range);
            }
            Token::StringLiteral(w) => {
                let string = w.to_string();
                let hash = self
                    .string_literals
                    .add_lit(string, &mut self.string_hasher);
                Ok(Some(Expression::from(Literal::String(hash, next_range))))
            }
            Token::ParenOpen => {
                self.paren_depth += 1;
                let rv = self.next_expression(0);
                self.paren_depth -= 1;
                self.next();
                rv
            }
            Token::Minus => {
                let expr = expect_atomic_expression!(self);
                Ok(Some(Expression::from(UnaryOperator::new(
                    UnaryOperatorType::Minus,
                    next_range,
                    expr,
                ))))
            }
            _ => Ok(None),
        }
    }

    fn next_word(
        &mut self,
        word: String,
        range: Range<usize>,
    ) -> Result<Option<Expression>, Error> {
        if word == "not" {
            let expr = expect_atomic_expression!(self);
            return Ok(Some(Expression::from(UnaryOperator::new(
                UnaryOperatorType::Not,
                range,
                expr,
            ))));
        }

        if word == "true" {
            return Ok(Some(Expression::from(Literal::Bool(true, range))));
        }
        if word == "false" {
            return Ok(Some(Expression::from(Literal::Bool(false, range))));
        }

        if let Ok(value) = word.parse() {
            if let Some(Token::Period) = self.peek_token() {
                self.next();
                if let Some(Token::Word(frac)) = self.next_token() {
                    let mut full_str = word;
                    full_str.push('.');
                    full_str.push_str(frac);
                    return match full_str.parse() {
                        Ok(value) => Ok(Some(Expression::from(Literal::Float(value, range)))),
                        Err(_) => Err(Error::ParserError(
                            self.get_error_prefix(),
                            ParserError::InvalidLiteralError(
                                full_str,
                                "expected this to be a float literal.".into(),
                            ),
                        )),
                    };
                }
            }
            return Ok(Some(Expression::from(Literal::Int(value, range))));
        }

        if word.ends_with("e") && word[..word.len() - 1].is_numeric() {
            if let Some(Token::Word(w)) = self.peek_n_token(1) {
                let operator = match self.peek_n_token(0) {
                    Some(Token::Plus) => Some('+'),
                    Some(Token::Minus) => Some('-'),
                    _ => None,
                };
                match operator {
                    Some(operator) => {
                        let mut full_value = word.to_string();
                        full_value.push(operator);
                        full_value.push_str(&w);
                        if let Ok(value) = full_value.parse::<f64>() {
                            self.next();
                            self.next();
                            if value.can_be_int() {
                                return Ok(Some(Expression::from(Literal::Int(
                                    value as i64,
                                    range,
                                ))));
                            }
                            return Ok(Some(Expression::from(Literal::Float(value, range))));
                        }
                    }
                    None => (),
                }
            };
            return Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::InvalidLiteralError(
                    word,
                    "expected float literal due to numeric followed by \"e\". example: 10e-3"
                        .into(),
                ),
            ));
        }

        for k in LITERAL_PREFIXES.keys() {
            if !word.starts_with(k) {
                continue;
            }
            let rest_word = &word[k.len()..];
            if let Ok(value) = i64::from_str_radix(rest_word, LITERAL_PREFIXES[k]) {
                return Ok(Some(Expression::from(Literal::Int(value, range))));
            }
            return Err(Error::ParserError(
                self.get_error_prefix(),
                ParserError::InvalidLiteralError(
                    format!("{:?}", word),
                    format!(
                        "expected an int literal with base {} after prefix \"{}\"",
                        LITERAL_PREFIXES[k], k
                    )
                    .into(),
                ),
            ));
        }

        if word.starts_with("base_") {
            let mut remaining_word_iter: std::str::Split<'_, &str> = (&word[5..]).split("_");
            if let Some(base_str) = remaining_word_iter.next() {
                if let Ok(base) = base_str.parse() {
                    if let Some(literal) = remaining_word_iter.next() {
                        if let Ok(value) = i64::from_str_radix(literal, base) {
                            return Ok(Some(Expression::from(Literal::Int(value, range))));
                        }
                    }
                }
            }
            return Err(Error::ParserError(
                self.get_error_prefix(),ParserError::InvalidLiteralError(
                word,
                "expected a base n and an int literal with base n after prefix \"base_\". example: \"base_3_2102\"".into()
            )));
        }

        Ok(Some(Expression::from(Indentifier::Final(
            word,
            self.prev().unwrap().char_range.clone(),
        ))))
    }

    fn assert_not_literal(&self, identifier: &str) -> Result<(), Error> {
        if identifier
            .chars()
            .nth(0)
            .expect("zero-size identifier")
            .is_numeric()
        {
            return Err(Error::ParserError(
                self.get_error_prefix(),ParserError::UnexpectedLiteralError(
                identifier.into(),
                "a literal cannot be placed in member syntax (var.member). was this meant to be a float literal?".into()
            )));
        }
        for prefix in LITERAL_PREFIXES
            .keys()
            .concat(SingleItemIterator::new(&"base_".to_string()))
        {
            if identifier.starts_with(prefix) {
                return Err(Error::ParserError(
                    self.get_error_prefix(),
                    ParserError::UnexpectedLiteralError(
                        identifier.into(),
                        format!("a literal cannot be placed in member syntax (var.member). tokens starting with {} are interpreted as numeric literals.", prefix)
                    )
                ));
            }
        }
        Ok(())
    }

    fn skip_newline_back(&self, mut pointer: usize) -> usize {
        loop {
            let token = match self.tokens.get(pointer) {
                Some(t) => t,
                None => break,
            };
            match token.token {
                Token::NewLine(_) => {
                    if pointer > 0 {
                        pointer -= 1;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        pointer
    }

    fn skip_newline_forward(&self, mut pointer: usize) -> usize {
        loop {
            let token = match self.tokens.get(pointer) {
                Some(t) => t,
                None => break,
            };
            match token.token {
                Token::NewLine(_) => pointer += 1,
                _ => break,
            }
        }
        pointer
    }

    fn prev(&self) -> Option<&DebugToken> {
        let mut temp_pointer = self.pointer - 1;

        if self.paren_depth > 0 {
            temp_pointer = self.skip_newline_back(temp_pointer);
        }

        self.tokens.get(temp_pointer)
    }

    fn peek(&self) -> Option<&DebugToken> {
        let mut temp_pointer = self.pointer;
        if self.paren_depth > 0 {
            temp_pointer = self.skip_newline_back(temp_pointer);
        }
        self.tokens.get(temp_pointer)
    }

    fn _peek_n(&self, n: usize) -> Option<&DebugToken> {
        let mut temp_pointer = self.pointer;
        if self.paren_depth > 0 {
            for _ in 0..n {
                temp_pointer = self.skip_newline_forward(temp_pointer + 1);
            }
        } else {
            temp_pointer += n;
        }

        self.tokens.get(temp_pointer)
    }

    fn next(&mut self) -> Option<&DebugToken> {
        let rv = self.tokens.get(self.pointer);

        self.pointer += 1;
        if self.paren_depth > 0 {
            self.pointer = self.skip_newline_forward(self.pointer);
        }

        rv
    }

    fn move_n(&mut self, n: usize) -> () {
        if self.paren_depth > 0 {
            for _ in 0..n {
                self.pointer = self.skip_newline_forward(self.pointer + 1);
            }
        } else {
            self.pointer += n;
        }
    }

    fn prev_token(&self) -> Option<&Token> {
        self.get_token(self.pointer - 1)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.get_token(self.pointer)
    }

    fn peek_n_token(&self, n: usize) -> Option<&Token> {
        let mut temp_pointer = self.pointer;
        if self.paren_depth > 0 {
            for _ in 0..n {
                temp_pointer = self.skip_newline_forward(temp_pointer + 1);
            }
        } else {
            temp_pointer += n;
        }
        self.get_token(temp_pointer)
    }

    fn next_token(&mut self) -> Option<&Token> {
        let rv = match self.tokens.get(self.pointer) {
            Some(token) => Some(&token.token),
            None => None,
        };

        self.pointer += 1;
        if self.paren_depth > 0 {
            self.pointer = self.skip_newline_forward(self.pointer);
        }

        rv
    }

    fn get_token(&self, index: usize) -> Option<&Token> {
        match self.tokens.get(index) {
            Some(token) => Some(&token.token),
            None => None,
        }
    }

    fn _print_next_n(&mut self, n: usize) {
        println!("printing next {} tokens", n);
        for i in 0..n {
            println!("{:?}", self._peek_n(i))
        }
    }

    fn get_error_prefix(&self) -> String {
        let range = match self.prev() {
            Some(t) => &t.char_range,
            None => &(1..0),
        };

        get_error_prefix(&self.debug_info, range)
    }
}
