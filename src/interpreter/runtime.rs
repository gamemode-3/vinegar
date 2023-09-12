use std::cmp::Ordering;
use std::sync::{Arc, Mutex};
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    ops::Range,
    rc::Rc,
};

use crate::file_handler;

use super::debug::OrError;
use super::parser::{DebugString, GetCharRange, ParserResult, UnaryOperator, UnaryOperatorType};
use super::string_literal_map::StringLiteralMap;
use super::{
    debug::{DebugInfo, Error, FileOrOtherError, VinegarError},
    lexer::Lexer,
    parser::{
        Assignment, BinaryOperator, CodeBody, Expression, FunctionCall, FunctionDefinition,
        Indentifier, Literal, Parser, Statement,
    },
    string_literal_map::MapStringLiterals,
    vinegar_std,
};

#[derive(Clone)]
pub struct RustFunctionWrapper {
    pub runner: &'static dyn Fn(
        &VinegarScope,
        &StringLiteralMap,
        &VinegarScope,
    ) -> Result<VinegarObject, VinegarError>,
}

impl RustFunctionWrapper {
    fn run(
        &self,
        global_scope: &VinegarScope,
        string_literals: &StringLiteralMap,
        new_local_scope: &VinegarScope,
    ) -> Result<VinegarObject, VinegarError> {
        (*self.runner)(global_scope, string_literals, new_local_scope)
    }
}

pub trait Library {
    fn get_globals(
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> HashMap<String, VinegarObject>;
}

#[derive(Clone)]
pub enum FunctionBody {
    VinegarBody(CodeBody),
    RustWrapper(RustFunctionWrapper),
}

impl std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionBody::VinegarBody(code_body) => write!(f, "VinegarBody({:?})", code_body),
            FunctionBody::RustWrapper(_) => write!(f, "<RustWrapper>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArg {
    pub name: String,
    pub value: Option<VinegarObject>,
}

impl FunctionArg {
    pub fn new(name: String, value: Option<VinegarObject>) -> Self {
        FunctionArg { name, value }
    }
}

impl Ord for FunctionArg {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for FunctionArg {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for FunctionArg {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for FunctionArg {}

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<FunctionArg>,
    pub body: FunctionBody,
}

impl Function {
    pub fn new(args: Vec<FunctionArg>, body: FunctionBody) -> Self {
        Self {
            args: args,
            body: body,
        }
    }
}

#[derive(Debug, Clone)]
pub enum VinegarObject {
    None,
    String(u64),
    Int(i64),
    Float(f64),
    Function(Function),
    List(Vec<VinegarObject>),
    RustStructWrapper(RustStructWrapper),
}

impl VinegarObject {
    pub fn mul(&self, other: &Self) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => match other {
                VinegarObject::Int(other_i) => Ok(VinegarObject::Int(i * other_i)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 * other_f)),
                _ => other.mul(self),
            },
            VinegarObject::Float(_) => other.mul(self),
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "cannot perform arithemtic objects of type {}.",
                self.type_name()
            ))),
        }
    }

    pub fn add(
        &self,
        other: &Self,
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => match other {
                VinegarObject::Int(other_i) => Ok(VinegarObject::Int(i + other_i)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 + other_f)),
                _ => other.add(self, string_literals, string_hasher),
            },
            VinegarObject::Float(_) => other.add(self, string_literals, string_hasher),
            VinegarObject::String(h) => {
                let mut self_string = string_literals.get_lit(h).clone();
                let other_string = other.format_string(string_literals)?;
                self_string.push_str(&other_string);
                Ok(VinegarObject::String(
                    string_literals.add_lit(self_string, string_hasher),
                ))
            }
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "cannot perform arithemtic objects of type {}.",
                self.type_name()
            ))),
        }
    }

    pub fn sub(&self, other: &Self) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => match other {
                VinegarObject::Int(other_i) => Ok(VinegarObject::Int(i - other_i)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 - other_f)),
                _ => other.sub(self),
            },
            &VinegarObject::Float(f) => match other {
                &VinegarObject::Int(other_i) => Ok(VinegarObject::Float(f - other_i as f64)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(f - other_f)),
                _ => other.sub(self),
            },
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "cannot perform arithemtic objects of type {}.",
                self.type_name()
            ))),
        }
    }

    pub fn div(&self, other: &Self) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => match other {
                &VinegarObject::Int(other_i) => Ok(VinegarObject::Float(i as f64 / other_i as f64)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 / other_f)),
                _ => other.mul(self),
            },
            &VinegarObject::Float(f) => match other {
                &VinegarObject::Int(other_i) => Ok(VinegarObject::Float(f / other_i as f64)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(f / other_f)),
                _ => other.sub(self),
            },
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "cannot perform arithemtic objects of type {}.",
                self.type_name()
            ))),
        }
    }

    pub fn invert(&self) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => Ok(VinegarObject::Int(-i)),
            &VinegarObject::Float(f) => Ok(VinegarObject::Float(-f)),
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "cannot perform arithemtic objects of type {}.",
                self.type_name()
            ))),
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            VinegarObject::None => "None",
            VinegarObject::Int(_) => "Int",
            VinegarObject::Float(_) => "Float",
            VinegarObject::String(_) => "String",
            VinegarObject::Function(..) => "Function",
            VinegarObject::List(..) => "List",
            VinegarObject::RustStructWrapper(..) => "RustStructWrapper",
        }
    }

    pub fn type_hash(&self) -> u64 {
        match self {
            VinegarObject::None => 0,
            VinegarObject::Int(_) => 1,
            VinegarObject::Float(_) => 2,
            VinegarObject::String(_) => 3,
            VinegarObject::Function(..) => 4,
            VinegarObject::List(..) => 5,
            VinegarObject::RustStructWrapper(..) => 6,
        }
    }

    pub fn as_string<'a>(
        &'a self,
        string_literals: &'a StringLiteralMap,
    ) -> Result<&String, VinegarError> {
        match self {
            VinegarObject::String(s) => Ok(string_literals.get(s).unwrap()),
            _ => Err(VinegarError::TypeError(format!(
                "cannot represent {} as Rust String",
                self.type_name()
            ))),
        }
    }

    pub fn as_usize(&self) -> Result<usize, VinegarError> {
        match self {
            &VinegarObject::Int(i) => {
                if i < 0 {
                    Err(VinegarError::TypeError(format!(
                        "cannot represent negative Int {} as Rust usize",
                        i
                    )))
                } else {
                    Ok(i as usize)
                }
            }
            &VinegarObject::Float(f) => {
                if f < 0.0 {
                    Err(VinegarError::TypeError(format!(
                        "cannot represent negative Float {} as Rust usize",
                        f
                    )))
                } else {
                    let fract = f.fract();
                    if fract < 0.0000001 {
                        return Ok(f as usize);
                    }
                    if fract > 0.9999999 {
                        return Ok(f as usize + 1);
                    }
                    Err(VinegarError::TypeError(format!(
                        "cannot represent fractional Float {} as Rust usize",
                        f
                    )))
                }
            }
            _ => Err(VinegarError::TypeError(format!(
                "cannot represent {} as Rust usize",
                self.type_name()
            ))),
        }
    }

    pub fn as_i64(&self) -> Result<i64, VinegarError> {
        match self {
            &VinegarObject::Int(i) => Ok(i),
            &VinegarObject::Float(f) => {
                if f < 0.0 {
                    Err(VinegarError::TypeError(format!(
                        "cannot represent negative Float {} as Rust usize",
                        f
                    )))
                } else {
                    let fract = f.fract();
                    if fract < 0.0000001 {
                        return Ok(f as i64);
                    }
                    if fract > 0.9999999 {
                        return Ok(f as i64 + 1);
                    }
                    Err(VinegarError::TypeError(format!(
                        "cannot represent fractional Float {} as Rust usize",
                        f
                    )))
                }
            }
            _ => Err(VinegarError::TypeError(format!(
                "cannot represent {} as Rust usize",
                self.type_name()
            ))),
        }
    }

    pub fn as_f64(&self) -> Result<f64, VinegarError> {
        match self {
            &VinegarObject::Float(f) => Ok(f),
            &VinegarObject::Int(i) => Ok(i as f64),
            _ => Err(VinegarError::TypeError(format!(
                "cannot represent {} as Rust usize",
                self.type_name()
            ))),
        }
    }

    pub fn format_string(
        &self,
        string_literals: &StringLiteralMap,
    ) -> Result<String, VinegarError> {
        Ok(match self {
            VinegarObject::None => "None".to_string(),
            VinegarObject::Int(i) => format!("{}", i),
            VinegarObject::Float(f) => format!("{}", f),
            VinegarObject::String(h) => format!("{}", string_literals.get_lit(h)),
            VinegarObject::List(l) => {
                let mut object_strings: Vec<String> = Vec::new();
                for o in l {
                    object_strings.push(o.format_string(string_literals)?);
                }
                format!("{}", object_strings.join(" | "))
            }
            VinegarObject::Function(f) => format!(
                "{}({})",
                match f.body {
                    FunctionBody::VinegarBody(..) => "vinegar_function",
                    FunctionBody::RustWrapper(..) => "rust_function_wrapper",
                },
                {
                    let mut rv = "".to_string();
                    let mut args_iter = f.args.iter().peekable();
                    loop {
                        let a = match args_iter.next() {
                            Some(a) => {
                                rv.push_str(", ");
                                a
                            }
                            None => break,
                        };
                        let arg_str = if let Some(dv) = &a.value {
                            format!("{}: {}", a.name, dv.format_string(string_literals)?)
                        } else {
                            format!("{}", a.name)
                        };
                        rv.push_str(&arg_str);
                    }
                    rv
                }
            ),
            VinegarObject::RustStructWrapper(w) => {
                format!(
                    "<RustStructWrapper {{ object: {} }}>",
                    w.object.lock().unwrap().to_string(string_literals)?
                )
            }
        })
    }

    pub fn from_struct<T>(struct_: T) -> VinegarObject
    where
        T: RustStructInterface + 'static,
    {
        VinegarObject::RustStructWrapper(RustStructWrapper {
            object: Arc::new(Mutex::new(struct_)),
        })
    }

    pub fn get_attribute(
        &self,
        name: &String,
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError> {
        match name {
            _ => (),
        }
        match self {
            VinegarObject::RustStructWrapper(w) => {
                let object = w.object.lock().unwrap();
                object.get_attribute(name, string_literals, string_hasher)
            }
            _ => Err(VinegarError::AttributeNotFound(
                self.format_string(string_literals)?,
                name.clone(),
            )),
        }
    }
}

pub trait RustStructInterface: std::fmt::Debug {
    fn get_attribute(
        &self,
        name: &String,
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError>;

    fn set_attribute(
        &mut self,
        name: &String,
        value: VinegarObject,
        string_literals: &StringLiteralMap,
    ) -> Result<(), VinegarError>;

    fn to_string(&self, string_literals: &StringLiteralMap) -> Result<String, VinegarError>;

    fn write_debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

pub trait VinegarConstructor {
    fn new_vinegar(
        _global_scope: &VinegarScope,
        string_literals: &StringLiteralMap,
        args: &VinegarScope,
    ) -> Result<VinegarObject, VinegarError>;

    fn import_vinegar_constructor(scope: &mut VinegarScope);
}

#[derive(Clone)]
pub struct RustStructWrapper {
    pub object: Arc<Mutex<dyn RustStructInterface>>,
}

impl std::fmt::Debug for RustStructWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let locked_data = self.object.lock().unwrap();

        locked_data.write_debug(f)
    }
}

pub trait VinegarObjectConversion<T>
where
    T: Sized,
    Self: Sized,
{
    fn into_other(&self, string_literals: &StringLiteralMap) -> Result<T, VinegarError>;

    fn from_other(
        value: T,
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> Result<Self, VinegarError>;
}

pub type VinegarScope = HashMap<String, VinegarObject>;

pub struct VinegarRuntime {
    string_literals: StringLiteralMap,
    string_hasher: DefaultHasher,
    global_scope: VinegarScope,
    local_stack: Vec<VinegarScope>,
    debug_info: DebugInfo,
}
impl VinegarRuntime {
    #[allow(dead_code)]
    pub fn interpret_string(s: &String, debug_info: Option<DebugInfo>) -> Result<(), Error> {
        let tokens = Lexer::lex_string(s.to_string(), debug_info.clone())?;
        let parser_result = Parser::parse_tokens(tokens, debug_info.clone())?;
        Self::interpret_parse_tree(parser_result, debug_info)?;
        Ok(())
    }

    pub fn interpret_file(path: String) -> Result<VinegarObject, FileOrOtherError> {
        let file_content = match file_handler::get_file_contents(&path) {
            Ok(contents) => contents,
            Err(err) => return Err(FileOrOtherError::IOError(err)),
        };
        let debug_info = DebugInfo::new(Some(file_content.clone()), format!("\"{}\"", path));
        let lexer_result = Lexer::lex_string(file_content, Some(debug_info.clone()));
        let tokens = match lexer_result {
            Ok(t) => t,
            Err(err) => return Err(FileOrOtherError::from(err)),
        };
        let parser_result = Parser::parse_tokens(tokens, Some(debug_info.clone()));
        let parser_result = match parser_result {
            Ok(c) => c,
            Err(err) => return Err(FileOrOtherError::from(err)),
        };
        let interpreter_result = Self::interpret_parse_tree(parser_result, Some(debug_info));
        match interpreter_result {
            Ok(result) => Ok(result),
            Err(err) => Err(FileOrOtherError::from(err)),
        }
    }

    fn interpret_parse_tree(
        parser_result: ParserResult,
        debug_info: Option<DebugInfo>,
    ) -> Result<VinegarObject, Error> {
        let mut instance: VinegarRuntime = Self {
            string_literals: parser_result.string_literals,
            string_hasher: parser_result.string_hasher,
            global_scope: VinegarScope::new(),
            local_stack: vec![VinegarScope::new()],
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new(None, "(unknown source)".to_string()),
            },
        };
        instance.interpret(parser_result.tree_root)
    }

    fn interpret(&mut self, tree_root: CodeBody) -> Result<VinegarObject, Error> {
        self.import_library::<vinegar_std::StandardLibrary>();
        self.interpret_code_body(&tree_root)
    }

    fn interpret_code_body(&mut self, code_body: &CodeBody) -> Result<VinegarObject, Error> {
        let mut last_result = VinegarObject::None;
        for statement in &code_body.statements {
            last_result = match statement {
                Statement::Assignment(a) => {
                    self.interpret_assignment(&a)?;
                    VinegarObject::None
                }
                Statement::Expression(e) => self.interpret_expression(&e)?,
                Statement::FunctionDefinition(f) => {
                    self.interpret_function_definition(f)?;
                    VinegarObject::None
                }
            };
        }
        Ok(last_result)
    }

    fn interpret_assignment(&mut self, assignment: &Assignment) -> Result<(), Error> {
        let name = &assignment.name;
        let value: VinegarObject = self.interpret_expression(&assignment.value)?;
        let local_scope = self.local_stack.last_mut().unwrap();
        match local_scope.get_mut(name) {
            Some(v) => {
                *v = value;
            }
            None => {
                local_scope.insert(name.clone(), value);
            }
        }
        Ok(())
    }

    fn interpret_expression(&mut self, expression: &Expression) -> Result<VinegarObject, Error> {
        match expression {
            Expression::UnaryOperator(un_op) => self.interpret_un_op(un_op),
            Expression::BinaryOperator(bin_op) => self.interpret_bin_op(bin_op),
            Expression::Literal(l) => self.interpret_literal(l),
            Expression::Identifier(v) => self.interpret_variable(v),
            Expression::FunctionCall(f) => self.interpret_function_call(f),
        }
    }

    fn interpret_variable(&mut self, identifier: &Indentifier) -> Result<VinegarObject, Error> {
        match identifier {
            Indentifier::Final(name, range) => {
                match self.global_scope.get(name) {
                    Some(value) => return Ok(value.clone()),
                    None => (),
                };
                match self.local_stack.last().unwrap().get(name) {
                    Some(value) => return Ok(value.clone()),
                    None => (),
                };
                Err(Error::VinegarError(
                    self.get_error_prefix(range.clone()),
                    VinegarError::UnknownIdentifier(name.clone()),
                ))
            }
            Indentifier::Member(name, expr, range) => {
                let value = self.interpret_expression(expr)?;
                match value.get_attribute(name, &mut self.string_literals, &mut self.string_hasher)
                {
                    Ok(v) => Ok(v),
                    Err(_) => Err(Error::VinegarError(
                        self.get_error_prefix(range.clone()),
                        VinegarError::VarAttributeNotFound(
                            value.type_name().to_string(),
                            expr.debug_string(),
                            name.to_string(),
                        ),
                    )),
                }
            }
        }
    }

    fn interpret_literal(&mut self, literal: &Literal) -> Result<VinegarObject, Error> {
        match literal {
            &Literal::Int(i) => Ok(VinegarObject::Int(i)),
            &Literal::Float(f) => Ok(VinegarObject::Float(f)),
            &Literal::String(s) => Ok(VinegarObject::String(s)),
        }
    }

    fn interpret_bin_op(&mut self, op: &Rc<BinaryOperator>) -> Result<VinegarObject, Error> {
        match &**op {
            BinaryOperator::Add(a, b) => {
                let value_a = self.interpret_expression(a)?;
                let value_b = self.interpret_expression(b)?;
                match value_a.add(&value_b, &mut self.string_literals, &mut self.string_hasher) {
                    Ok(v) => Ok(v),
                    Err(err) => Err(Error::VinegarError(self.get_error_prefix(1..0), err)),
                }
            }
            BinaryOperator::Sub(a, b) => {
                let value_a = self.interpret_expression(a)?;
                let value_b = self.interpret_expression(b)?;
                match value_a.sub(&value_b) {
                    Ok(v) => Ok(v),
                    Err(err) => Err(Error::VinegarError(self.get_error_prefix(1..0), err)),
                }
            }
            BinaryOperator::Mul(a, b) => {
                let value_a = self.interpret_expression(a)?;
                let value_b = self.interpret_expression(b)?;
                match value_a.mul(&value_b) {
                    Ok(v) => Ok(v),
                    Err(err) => Err(Error::VinegarError(self.get_error_prefix(1..0), err)),
                }
            }
            BinaryOperator::Div(a, b) => {
                let value_a = self.interpret_expression(a)?;
                let value_b = self.interpret_expression(b)?;
                match value_a.div(&value_b) {
                    Ok(v) => Ok(v),
                    Err(err) => Err(Error::VinegarError(self.get_error_prefix(1..0), err)),
                }
            }
        }
    }

    fn interpret_un_op(&mut self, op: &Rc<UnaryOperator>) -> Result<VinegarObject, Error> {
        match (&**op).op_type {
            UnaryOperatorType::Invert => {
                let value = self.interpret_expression(&op.expr)?;
                Ok(value
                    .invert()
                    .or_error(self.get_error_prefix(op.get_char_range()))?)
            }
            _ => todo!(), // UnaryOperatorType::Not => {
                          //     let value = self.interpret_expression(&op.expr)?;
                          //     Ok(value.not()?)
                          // }
        }
    }

    fn interpret_function_call(&mut self, f: &Rc<FunctionCall>) -> Result<VinegarObject, Error> {
        let func = self.interpret_expression(&f.expr)?;
        match &func {
            VinegarObject::Function(function) => {
                let mut expected_args = function.args.clone();
                let body = &function.body;
                let mut function_scope = VinegarScope::new();

                for (name_option, value) in &f.args {
                    let name_index = match name_option {
                        Some(name) => {
                            let comparator = |key: &String, item: &FunctionArg| key.cmp(&item.name);
                            match expected_args.binary_search_by(|item| comparator(name, item)) {
                                Ok(index) => Ok(index),
                                Err(_) => Err(VinegarError::InvalidArgumentsError(format!(
                                    "invalid keyword argument for function {}: {}",
                                    func.format_string(&self.string_literals)
                                        .or_error(self.get_error_prefix(1..0))?,
                                    name
                                ))),
                            }
                        }
                        None => {
                            let expected_args_len = function.args.len();
                            if expected_args_len > 0 {
                                Ok(0)
                            } else {
                                let given_args_len = f.args.len();
                                Err(VinegarError::InvalidArgumentsError(format!(
                                    "\"{}\" takes {} {}, but {} {} provided.",
                                    f.expr.debug_string(),
                                    expected_args_len,
                                    match expected_args_len {
                                        1 => "argument".to_string(),
                                        _ => "arguments".to_string(),
                                    },
                                    given_args_len,
                                    match given_args_len {
                                        1 => "was".to_string(),
                                        _ => "were".to_string(),
                                    },
                                )))
                            }
                        }
                    }
                    .or_error(self.get_error_prefix(1..0))?;

                    let owned_arg = expected_args.remove(name_index);
                    function_scope.insert(owned_arg.name, self.interpret_expression(value)?);
                }

                for arg in expected_args {
                    if let Some(v) = arg.value {
                        function_scope.insert(arg.name, v);
                    } else {
                        return Err(VinegarError::InvalidArgumentsError(format!(
                            "too few arguments for {}",
                            f.expr.debug_string()
                        )))
                        .or_error(self.get_error_prefix(f.get_char_range()));
                    }
                }

                self.local_stack.push(function_scope);

                let result = match body {
                    FunctionBody::VinegarBody(b) => self.interpret_code_body(b),
                    FunctionBody::RustWrapper(w) => {
                        match w.run(
                            &self.global_scope,
                            &self.string_literals,
                            self.local_stack.last().unwrap(),
                        ) {
                            Ok(result) => Ok(result),
                            Err(err) => Err(Error::VinegarError(
                                self.get_error_prefix(f.expr.get_char_range()),
                                err,
                            )),
                        }
                    }
                };
                self.local_stack.pop();
                result
            }
            _ => {
                return Err(Error::VinegarError(
                    self.get_error_prefix(f.expr.get_char_range()),
                    VinegarError::NotCallableError(f.expr.debug_string(), func.type_name().into()),
                ))
            }
        }
    }

    fn interpret_function_definition(&mut self, func: &FunctionDefinition) -> Result<(), Error> {
        let name = &func.name;
        let mut args = vec![];
        for a in &func.args {
            args.push(FunctionArg {
                name: a.name.clone(),
                value: match &a.default {
                    Some(dv) => Some(self.interpret_expression(dv)?),
                    None => None,
                },
            })
        }

        let value: VinegarObject = VinegarObject::Function(Function::new(
            args,
            FunctionBody::VinegarBody(func.body.clone()),
        ));
        let local_scope = self.local_stack.last_mut().unwrap();
        match local_scope.get_mut(name) {
            Some(v) => {
                *v = value;
            }
            None => {
                local_scope.insert(name.clone(), value);
            }
        }
        Ok(())
    }

    fn import_library<T>(&mut self)
    where
        T: Library,
    {
        self.global_scope.extend(T::get_globals(
            &mut self.string_literals,
            &mut self.string_hasher,
        ));
    }

    fn get_error_prefix(&self, range: Range<usize>) -> String {
        self.get_error_prefix_for_debug_info(&self.debug_info, range)
    }

    fn get_error_prefix_for_debug_info(
        &self,
        debug_info: &DebugInfo,
        range: Range<usize>,
    ) -> String {
        if let None = self.debug_info.source {
            return "".to_string();
        }

        let mut line = 0;
        let mut column = 0;
        let s: &String = debug_info
            .source
            .as_ref()
            .expect("suddenly there was no string");
        for char in s.chars().take(range.start) {
            column += 1;
            if char == '\n' {
                line += 1;
                column = 0;
            }
        }

        let file_info = format!("in {},", debug_info.source_name,);

        if range.start > range.end {
            return format!("{} unknown position:", file_info);
        }
        format!("{} line {}, column {}:", file_info, line + 1, column + 1,)
    }
}
