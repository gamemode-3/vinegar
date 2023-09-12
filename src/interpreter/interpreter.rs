use std::sync::{Arc, Mutex};
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{BuildHasherDefault, Hash, Hasher},
    ops::Range,
    rc::Rc,
};

use crate::file_handler;

use super::parser::{DebugString, GetCharRange};
use super::{
    debug::{DebugInfo, Error, FileOrOtherError, VinegarError},
    lexer::Lexer,
    parser::{
        Assignment, BinaryOperator, CodeBody, Expression, FunctionCall, FunctionDefinition,
        Indentifier, Literal, Parser, Statement,
    },
    vinegar_std,
};

#[derive(Clone, Debug)]
pub struct NoHasher;

impl Default for NoHasher {
    fn default() -> Self {
        NoHasher
    }
}

impl Hasher for NoHasher {
    fn write(&mut self, _bytes: &[u8]) {
        // No action is taken; we're not actually hashing anything here.
    }

    fn finish(&self) -> u64 {
        0 // Return a constant hash value (0 in this case)
    }
}

pub type ManualHashMap<K, V> = HashMap<K, V, BuildHasherDefault<NoHasher>>;

#[derive(Clone)]
pub struct RustFunctionWrapper {
    pub runner: &'static dyn Fn(
        &VinegarScope,
        &ManualHashMap<u64, String>,
        &VinegarScope,
    ) -> Result<VinegarObject, VinegarError>,
}

impl RustFunctionWrapper {
    fn run(
        &self,
        global_scope: &VinegarScope,
        string_literals: &ManualHashMap<u64, String>,
        new_local_scope: &VinegarScope,
    ) -> Result<VinegarObject, VinegarError> {
        (*self.runner)(global_scope, string_literals, new_local_scope)
    }
}

pub trait Library {
    fn get_globals() -> HashMap<String, VinegarObject>;
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
pub enum VinegarObject {
    None,
    String(u64),
    Int(i64),
    Float(f64),
    Function(Vec<String>, FunctionBody),
    List(Vec<VinegarObject>),
    RustStructWrapper(RustStructWrapper),
}

trait StringLiteralMap {
    fn add_lit(&mut self, s: String, hasher: &mut DefaultHasher) -> u64;

    fn get_lit(&self, h: &u64) -> &String;
}

impl StringLiteralMap for ManualHashMap<u64, String> {
    fn add_lit(&mut self, s: String, hasher: &mut DefaultHasher) -> u64 {
        s.hash(hasher);
        let hash = hasher.finish();
        if !self.contains_key(&hash) {
            self.insert(hash, s);
        }
        hash
    }

    fn get_lit(&self, h: &u64) -> &String {
        self.get(h)
            .expect(&format!("string literal does not exist: {}", h))
    }
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
        string_literals: &mut ManualHashMap<u64, String>,
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
        string_literals: &'a ManualHashMap<u64, String>,
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
        string_literals: &ManualHashMap<u64, String>,
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
            VinegarObject::Function(args, body) => format!(
                "{}({})",
                match body {
                    FunctionBody::VinegarBody(..) => "vinegar_function",
                    FunctionBody::RustWrapper(..) => "rust_function_wrapper",
                },
                args.join(", ")
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
        string_literals: &mut ManualHashMap<u64, String>,
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
            )), // TODO
        }
    }
}

pub trait RustStructInterface: std::fmt::Debug {
    fn get_attribute(
        &self,
        name: &String,
        string_literals: &mut ManualHashMap<u64, String>,
        string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError>;

    fn set_attribute(
        &mut self,
        name: &String,
        value: VinegarObject,
        string_literals: &ManualHashMap<u64, String>,
    ) -> Result<(), VinegarError>;

    fn to_string(
        &self,
        string_literals: &ManualHashMap<u64, String>,
    ) -> Result<String, VinegarError>;

    fn write_debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

pub trait VinegarConstructor {
    fn new_vinegar(
        _global_scope: &VinegarScope,
        string_literals: &ManualHashMap<u64, String>,
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
    fn into_other(&self, string_literals: &ManualHashMap<u64, String>) -> Result<T, VinegarError>;

    fn from_other(
        value: T,
        string_literals: &mut ManualHashMap<u64, String>,
        string_hasher: &mut DefaultHasher,
    ) -> Result<Self, VinegarError>;
}

pub type VinegarScope = HashMap<String, VinegarObject>;

pub struct Interpreter {
    string_hasher: DefaultHasher,
    string_literals: ManualHashMap<u64, String>,
    global_scope: VinegarScope,
    local_stack: Vec<VinegarScope>,
    debug_info: DebugInfo,
}
impl Interpreter {
    #[allow(dead_code)]
    pub fn interpret_string(s: &String, debug_info: Option<DebugInfo>) -> Result<(), Error> {
        let tokens = Lexer::lex_string(s.to_string(), debug_info.clone())?;
        let code_body = Parser::parse_tokens(tokens, debug_info.clone())?;
        Self::interpret_parse_tree(code_body, debug_info)?;
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
        let code_body = match parser_result {
            Ok(c) => c,
            Err(err) => return Err(FileOrOtherError::from(err)),
        };
        let interpreter_result = Self::interpret_parse_tree(code_body, Some(debug_info));
        match interpreter_result {
            Ok(result) => Ok(result),
            Err(err) => Err(FileOrOtherError::from(err)),
        }
    }

    fn interpret_parse_tree(
        tree_root: CodeBody,
        debug_info: Option<DebugInfo>,
    ) -> Result<VinegarObject, Error> {
        let mut instance: Interpreter = Self {
            string_hasher: DefaultHasher::new(),
            string_literals: HashMap::default(),
            global_scope: VinegarScope::new(),
            local_stack: vec![VinegarScope::new()],
            debug_info: match debug_info {
                Some(d) => d,
                None => DebugInfo::new(None, "(unknown source)".to_string()),
            },
        };
        instance.interpret(tree_root)
    }

    fn interpret(&mut self, code_body: CodeBody) -> Result<VinegarObject, Error> {
        self.import_library::<vinegar_std::StandardLibrary>();
        self.interpret_code_body(code_body)
    }

    fn interpret_code_body(&mut self, code_body: CodeBody) -> Result<VinegarObject, Error> {
        let mut last_result = VinegarObject::None;
        for statement in code_body.statements {
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
            Literal::String(s) => Ok(VinegarObject::String(
                self.string_literals
                    .add_lit(s.clone(), &mut self.string_hasher),
            )),
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

    fn interpret_function_call(&mut self, f: &Rc<FunctionCall>) -> Result<VinegarObject, Error> {
        let func = self.interpret_expression(&f.expr)?;
        match func {
            VinegarObject::Function(expected_args, body) => {
                let given_args_len = f.args.len();
                let expected_args_len = expected_args.len();
                if given_args_len != expected_args_len {
                    return Err(Error::VinegarError(
                        self.get_error_prefix(f.expr.get_char_range()),
                        VinegarError::InvalidArgumentsError(format!(
                            "function \"{}\" takes {} {}, but {} {} provided.",
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
                        )),
                    ));
                }

                let mut function_scope = VinegarScope::new();
                for (arg, name) in (&f.args).iter().zip(expected_args) {
                    function_scope.insert(name, self.interpret_expression(&arg)?);
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

    fn interpret_function_definition(&mut self, func: FunctionDefinition) -> Result<(), Error> {
        let name = &func.name;

        let value: VinegarObject =
            VinegarObject::Function(func.args, FunctionBody::VinegarBody(func.body));
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
        self.global_scope.extend(T::get_globals());
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
