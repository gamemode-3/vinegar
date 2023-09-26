use std::any::Any;
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    ops::Range,
};

use crate::file_handler;
use crate::interpreter::parser::ElseBody;

use super::debug::{get_error_prefix, OrError};
use super::parser::{
    BinaryOperatorType, DebugString, GetCharRange, If, ParserResult, Return, UnaryOperator,
    UnaryOperatorType,
};
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
    Int(i64),
    Bool(bool),
    Float(f64),
    String(u64),
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
                "operator \"*\" not valid for types {} and {}.",
                self.type_name(),
                other.type_name()
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
                "operator \"+\" not valid for types {} and {}.",
                self.type_name(),
                other.type_name()
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
                "operator \"-\" not valid for types {} and {}.",
                self.type_name(),
                other.type_name()
            ))),
        }
    }

    pub fn div(&self, other: &Self) -> Result<Self, VinegarError> {
        match self {
            &VinegarObject::Int(i) => match other {
                &VinegarObject::Int(other_i) => Ok(VinegarObject::Float(i as f64 / other_i as f64)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 / other_f)),
                _ => other.div(self),
            },
            &VinegarObject::Float(f) => match other {
                &VinegarObject::Int(other_i) => Ok(VinegarObject::Float(f / other_i as f64)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(f / other_f)),
                _ => other.div(self),
            },
            _ => Err(VinegarError::IncompatibleTypesError(format!(
                "operator \"/\" not valid for types {} and {}.",
                self.type_name(),
                other.type_name()
            ))),
        }
    }

    pub fn and(
        &self,
        other: &Self,
        string_literals: &StringLiteralMap,
    ) -> Result<Self, VinegarError> {
        Ok(VinegarObject::Bool(
            self.as_bool(string_literals)? && other.as_bool(string_literals)?,
        ))
    }

    pub fn or(
        &self,
        other: &Self,
        string_literals: &StringLiteralMap,
    ) -> Result<Self, VinegarError> {
        Ok(VinegarObject::Bool(
            self.as_bool(string_literals)? || other.as_bool(string_literals)?,
        ))
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

    pub fn not(&self, string_literals: &StringLiteralMap) -> Result<Self, VinegarError> {
        Ok(VinegarObject::Bool(!self.as_bool(string_literals)?))
    }

    pub fn type_name(&self) -> &str {
        match self {
            VinegarObject::None => "None",
            VinegarObject::Int(_) => "Int",
            VinegarObject::Bool(_) => "Bool",
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
            VinegarObject::Bool(_) => 2,
            VinegarObject::Float(_) => 3,
            VinegarObject::String(_) => 4,
            VinegarObject::Function(..) => 5,
            VinegarObject::List(..) => 6,
            VinegarObject::RustStructWrapper(..) => 7,
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

    pub fn as_bool(&self, string_literals: &StringLiteralMap) -> Result<bool, VinegarError> {
        match self {
            &VinegarObject::Bool(b) => Ok(b),
            &VinegarObject::Int(i) => Ok(i != 0),
            &VinegarObject::Float(f) => Ok(f != 0.0),
            &VinegarObject::String(s) => Ok(string_literals.get(&s).unwrap() != ""),
            _ => Err(VinegarError::TypeError(format!(
                "cannot represent {} as Rust bool",
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
            VinegarObject::Bool(b) => format!("{}", b),
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
                    FunctionBody::VinegarBody(..) => "fn",
                    FunctionBody::RustWrapper(..) => "<rust_function_wrapper>",
                },
                {
                    let mut rv = "".to_string();
                    let mut args_iter = f.args.iter().peekable();
                    loop {
                        let a = match args_iter.next() {
                            Some(a) => a,
                            None => break,
                        };
                        let arg_str = if let Some(dv) = &a.value {
                            format!("{}: {}", a.name, dv.format_string(string_literals)?)
                        } else {
                            format!("{}", a.name)
                        };
                        rv.push_str(&arg_str);
                        if let Some(_) = args_iter.peek() {
                            rv.push_str(", ");
                        }
                    }
                    rv
                }
            ),
            VinegarObject::RustStructWrapper(w) => {
                format!(
                    "<RustStructWrapper {{ object: {} }}>",
                    w.object.lock().unwrap().format_string(string_literals)?
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

pub trait RustStructInterface: std::fmt::Debug + AsAny {
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

    fn format_string(&self, string_literals: &StringLiteralMap) -> Result<String, VinegarError>;

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
pub unsafe trait AsAny {
    /// SAFETY: has to return self as it's used for type casting
    fn as_any(&self) -> &dyn Any;
}

pub fn downcast_struct_interface<T: RustStructInterface + 'static>(
    object: Arc<Mutex<dyn RustStructInterface>>,
) -> Option<Arc<Mutex<T>>> {
    if object.lock().unwrap().as_any().is::<T>() {
        let raw: *const Mutex<dyn RustStructInterface> = Arc::into_raw(object.clone());
        let raw: *const Mutex<T> = raw.cast();

        // SAFETY: This is safe because the pointer orignally came from an Arc
        // with the same size and alignment since we've checked (via Any) that
        // the object within is the type being casted to.
        Some(unsafe { Arc::from_raw(raw) })
    } else {
        None
    }
}

pub fn wrapper_into<T: RustStructInterface + 'static>(wrapper: RustStructWrapper) -> T {
    let specific_arc = downcast_struct_interface::<T>(wrapper.object).unwrap();
    Arc::into_inner(specific_arc).unwrap().into_inner().unwrap()
}

pub fn try_from_vinegar_object<T>(vinegar_object: VinegarObject) -> Option<T>
where
    T: RustStructInterface + 'static,
{
    match vinegar_object {
        VinegarObject::RustStructWrapper(w) => Some(wrapper_into(w)),
        _ => None,
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

struct VinegarScopeStack {
    lifetime_stack: Vec<VinegarScope>,
}

impl VinegarScopeStack {
    fn new() -> Self {
        Self {
            lifetime_stack: vec![VinegarScope::new()],
        }
    }

    fn get_mut(&mut self, name: &String) -> Option<&mut VinegarObject> {
        for lifetime in self.lifetime_stack.iter_mut().rev() {
            match lifetime.get_mut(name) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        return None;
    }

    fn get(&self, name: &String) -> Option<&VinegarObject> {
        for lifetime in self.lifetime_stack.iter().rev() {
            match lifetime.get(name) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        return None;
    }

    fn insert(&mut self, name: String, value: VinegarObject) {
        self.lifetime_stack.last_mut().unwrap().insert(name, value);
    }

    fn add_scope(&mut self) {
        self.lifetime_stack.push(VinegarScope::new());
    }

    fn pop_scope(&mut self) -> Option<VinegarScope> {
        self.lifetime_stack.pop()
    }

    fn last(&self) -> Option<&VinegarScope> {
        self.lifetime_stack.last()
    }
}

#[derive(Debug)]
pub struct EvalResult {
    value: VinegarObject,
    return_: bool,
}

pub struct VinegarRuntime {
    string_literals: StringLiteralMap,
    string_hasher: DefaultHasher,
    global_scope: VinegarScope,
    call_stack: Vec<VinegarScopeStack>,
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
            call_stack: vec![VinegarScopeStack::new()],
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new(None, "(unknown source)".to_string()),
            },
        };
        instance.interpret(parser_result.tree_root)
    }

    fn interpret(&mut self, tree_root: CodeBody) -> Result<VinegarObject, Error> {
        self.import_library::<vinegar_std::StandardLibrary>();
        Ok(self.interpret_code_body(&tree_root)?.value)
    }

    fn interpret_code_body(&mut self, code_body: &CodeBody) -> Result<EvalResult, Error> {
        self.call_stack.last_mut().unwrap().add_scope();
        let mut last_result = VinegarObject::None;
        for statement in &code_body.statements {
            let result = self.interpret_statement(statement)?;
            if result.return_ {
                return Ok(result);
            }
            last_result = result.value;
        }
        self.call_stack.last_mut().unwrap().pop_scope();
        Ok(EvalResult {
            value: last_result,
            return_: false,
        })
    }

    fn interpret_statement(&mut self, statement: &Statement) -> Result<EvalResult, Error> {
        let value = match statement {
            Statement::Assignment(a) => self.interpret_assignment(a)?,
            Statement::Expression(e) => self.interpret_expression(e)?,
            Statement::FunctionDefinition(f) => self.interpret_function_definition(f)?,
            Statement::If(i) => return self.interpret_if(i),
            Statement::Return(r) => {
                return Ok(EvalResult {
                    value: self.interpret_return(r)?,
                    return_: true,
                })
            }
        };

        Ok(EvalResult {
            value,
            return_: false,
        })
    }

    fn interpret_assignment(&mut self, assignment: &Assignment) -> Result<VinegarObject, Error> {
        let name = &assignment.name;
        let value: VinegarObject = self.interpret_expression(&assignment.value)?;
        let local_scope = self.call_stack.last_mut().unwrap();
        match local_scope.get_mut(name) {
            Some(v) => {
                *v = value;
            }
            None => {
                local_scope.insert(name.clone(), value);
            }
        }
        Ok(VinegarObject::None)
    }

    fn interpret_expression(&mut self, expression: &Expression) -> Result<VinegarObject, Error> {
        match expression {
            Expression::Literal(l) => self.interpret_literal(l),
            Expression::Identifier(v) => self.interpret_variable(v),
            Expression::UnaryOperator(un_op) => self.interpret_un_op(un_op),
            Expression::BinaryOperator(bin_op) => self.interpret_bin_op(bin_op),
            Expression::FunctionCall(f) => self.interpret_function_call(f),
        }
    }

    fn interpret_literal(&mut self, literal: &Literal) -> Result<VinegarObject, Error> {
        match literal {
            &Literal::Int(i, _) => Ok(VinegarObject::Int(i)),
            &Literal::Float(f, _) => Ok(VinegarObject::Float(f)),
            &Literal::String(s, _) => Ok(VinegarObject::String(s)),
            &Literal::Bool(b, _) => Ok(VinegarObject::Bool(b)),
        }
    }

    fn interpret_variable(&mut self, identifier: &Indentifier) -> Result<VinegarObject, Error> {
        match identifier {
            Indentifier::Final(name, range) => {
                match self.global_scope.get(name) {
                    Some(value) => return Ok(value.clone()),
                    None => (),
                };
                match self.call_stack.last().unwrap().get(name) {
                    Some(value) => return Ok(value.clone()),
                    None => (),
                };
                Err(Error::VinegarError(
                    self.get_error_prefix(&range),
                    VinegarError::UnknownIdentifier(name.clone()),
                ))
            }
            Indentifier::Member(name, expr, range) => {
                let value = self.interpret_expression(expr)?;
                match value.get_attribute(name, &mut self.string_literals, &mut self.string_hasher)
                {
                    Ok(v) => Ok(v),
                    Err(_) => Err(Error::VinegarError(
                        self.get_error_prefix(&range),
                        VinegarError::VarAttributeNotFound(
                            value.type_name().to_string(),
                            expr.debug_string(&self.string_literals),
                            name.to_string(),
                        ),
                    )),
                }
            }
        }
    }

    fn interpret_un_op(&mut self, op: &UnaryOperator) -> Result<VinegarObject, Error> {
        let value = self.interpret_expression(&op.expr)?;
        match op.op_type {
            UnaryOperatorType::Minus => Ok(value
                .invert()
                .or_error(self.get_error_prefix(&op.get_char_range()))?),
            UnaryOperatorType::Not => {
                let value = self.interpret_expression(&op.expr)?;
                Ok(value
                    .not(&self.string_literals)
                    .or_error(self.get_error_prefix(&op.get_char_range()))?)
            }
        }
    }

    fn interpret_bin_op(&mut self, op: &BinaryOperator) -> Result<VinegarObject, Error> {
        let value_left = self.interpret_expression(&op.left)?;
        let value_right = self.interpret_expression(&op.right)?;
        match op.op_type {
            BinaryOperatorType::Add => value_left
                .add(
                    &value_right,
                    &mut self.string_literals,
                    &mut self.string_hasher,
                )
                .or_error(self.get_error_prefix(&op.get_char_range())),
            BinaryOperatorType::Sub => value_left
                .sub(&value_right)
                .or_error(self.get_error_prefix(&op.get_char_range())),
            BinaryOperatorType::Mul => value_left
                .mul(&value_right)
                .or_error(self.get_error_prefix(&op.get_char_range())),
            BinaryOperatorType::Div => value_left
                .div(&value_right)
                .or_error(self.get_error_prefix(&op.get_char_range())),
            BinaryOperatorType::And => value_left
                .and(&value_right, &self.string_literals)
                .or_error(self.get_error_prefix(&op.get_char_range())),
            BinaryOperatorType::Or => value_left
                .or(&value_right, &self.string_literals)
                .or_error(self.get_error_prefix(&op.get_char_range())),
        }
    }

    fn interpret_function_call(&mut self, f: &FunctionCall) -> Result<VinegarObject, Error> {
        let func = self.interpret_expression(&f.expr)?;
        match &func {
            VinegarObject::Function(function) => {
                let mut remaining_expected_args = function.args.clone();
                let body = &function.body;
                let mut function_scope = VinegarScopeStack::new();

                for arg in &f.args {
                    let name_index = match &arg.debug_name {
                        Some((name, range)) => {
                            let comparator = |key: &String, item: &FunctionArg| key.cmp(&item.name);
                            match remaining_expected_args
                                .binary_search_by(|item| comparator(name, item))
                            {
                                Ok(index) => index,
                                Err(_) => {
                                    return Err(VinegarError::InvalidArgumentsError(format!(
                                        "invalid keyword argument for function {}: {}",
                                        func.format_string(&self.string_literals)
                                            .or_error(self.get_error_prefix(&range))?,
                                        name
                                    )))
                                    .or_error(self.get_error_prefix(&range))
                                }
                            }
                        }
                        None => {
                            let expected_args_len = function.args.len();
                            if remaining_expected_args.len() > 0 {
                                0
                            } else {
                                let given_args_len = f.args.len();
                                return Err(VinegarError::InvalidArgumentsError(format!(
                                    "\"{}\" takes {} {}, but {} {} provided.",
                                    f.expr.debug_string(&self.string_literals),
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
                                .or_error(self.get_error_prefix(&f.get_char_range()));
                            }
                        }
                    };

                    let owned_arg = remaining_expected_args.remove(name_index);
                    function_scope.insert(owned_arg.name, self.interpret_expression(&arg.expr)?);
                }

                for arg in remaining_expected_args {
                    if let Some(v) = arg.value {
                        function_scope.insert(arg.name, v);
                    } else {
                        return Err(VinegarError::InvalidArgumentsError(format!(
                            "too few arguments for \"{}\"",
                            f.expr.debug_string(&self.string_literals)
                        )))
                        .or_error(self.get_error_prefix(&f.get_char_range()));
                    }
                }

                self.call_stack.push(function_scope);

                let result = match body {
                    FunctionBody::VinegarBody(b) => self.interpret_code_body(b)?.value,
                    FunctionBody::RustWrapper(w) => w
                        .run(
                            &self.global_scope,
                            &self.string_literals,
                            self.call_stack.last().unwrap().last().unwrap(),
                        )
                        .or_error(self.get_error_prefix(&f.expr.get_char_range()))?,
                };
                self.call_stack.pop();

                Ok(result)
            }
            _ => {
                return Err(Error::VinegarError(
                    self.get_error_prefix(&f.expr.get_char_range()),
                    VinegarError::NotCallableError(
                        f.expr.debug_string(&self.string_literals),
                        func.type_name().into(),
                    ),
                ))
            }
        }
    }

    fn interpret_function_definition(
        &mut self,
        func: &FunctionDefinition,
    ) -> Result<VinegarObject, Error> {
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
        let local_scope = self.call_stack.last_mut().unwrap();
        match local_scope.get_mut(name) {
            Some(v) => {
                *v = value;
            }
            None => {
                local_scope.insert(name.clone(), value);
            }
        }
        Ok(VinegarObject::None)
    }

    fn interpret_if(&mut self, if_: &If) -> Result<EvalResult, Error> {
        let condition = self
            .interpret_expression(&if_.condition)?
            .as_bool(&self.string_literals)
            .or_error(self.get_error_prefix(&if_.condition.get_char_range()))?;
        if condition {
            self.interpret_code_body(&if_.if_body)
        } else {
            match &if_.else_body {
                Some(ElseBody::If(i)) => self.interpret_if(i.as_ref()),
                Some(ElseBody::CodeBody(b)) => self.interpret_code_body(&b),
                None => Ok(EvalResult {
                    value: VinegarObject::None,
                    return_: false,
                }),
            }
        }
    }

    fn interpret_return(&mut self, return_: &Return) -> Result<VinegarObject, Error> {
        match &return_.value {
            Some(v) => Ok(self.interpret_expression(v)?),
            None => Ok(VinegarObject::None),
        }
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

    fn get_error_prefix(&self, range: &Range<usize>) -> String {
        self.get_error_prefix_for_debug_info(&self.debug_info, range)
    }

    fn get_error_prefix_for_debug_info(
        &self,
        debug_info: &DebugInfo,
        range: &Range<usize>,
    ) -> String {
        get_error_prefix(&debug_info, range)
    }
}
