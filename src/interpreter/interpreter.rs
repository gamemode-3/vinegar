use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{BuildHasherDefault, Hash, Hasher},
    ops::Range,
    rc::Rc,
};

use crate::file_handler;

use super::{
    debug::{DebugInfo, FileInterpreterError, InterpreterError, VinegarError},
    lexer::Lexer,
    parser::{
        Assignment, BinaryOperator, CodeBody, Expression, FunctionCall, Literal, Parser, Statement,
        VariableName,
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
    pub runner: Box<
        &'static dyn Fn(
            &VinegarScope,
            &ManualHashMap<u64, String>,
            &Vec<VinegarObject>,
        ) -> Result<VinegarObject, InterpreterError>,
    >,
}

impl RustFunctionWrapper {
    fn run(
        &self,
        global_scope: &VinegarScope,
        string_literals: &ManualHashMap<u64, String>,
        args: &Vec<VinegarObject>,
    ) -> Result<VinegarObject, InterpreterError> {
        (*self.runner)(global_scope, string_literals, args)
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
            FunctionBody::RustWrapper(_) => write!(f, "RustWrapper(...)"),
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
}

impl VinegarObject {
    pub fn mul(&self, other: &Self) -> Result<Self, InterpreterError> {
        match self {
            &VinegarObject::Int(i) => match other {
                VinegarObject::Int(other_i) => Ok(VinegarObject::Int(i * other_i)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 * other_f)),
                _ => other.mul(self),
            },
            VinegarObject::Float(_) => other.mul(self),
            _ => Err(InterpreterError::IncompatibleTypesError(
                "".into(),
                format!(
                    "cannot perform arithemtic objects of type {}.",
                    self.type_name()
                ),
            )),
        }
    }

    pub fn add(&self, other: &Self) -> Result<Self, InterpreterError> {
        match self {
            &VinegarObject::Int(i) => match other {
                VinegarObject::Int(other_i) => Ok(VinegarObject::Int(i + other_i)),
                VinegarObject::Float(other_f) => Ok(VinegarObject::Float(i as f64 + other_f)),
                _ => other.add(self),
            },
            VinegarObject::Float(_) => other.add(self),
            _ => Err(InterpreterError::IncompatibleTypesError(
                "".into(),
                format!(
                    "cannot perform arithemtic objects of type {}.",
                    self.type_name()
                ),
            )),
        }
    }

    pub fn sub(&self, other: &Self) -> Result<Self, InterpreterError> {
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
            _ => Err(InterpreterError::IncompatibleTypesError(
                "".into(),
                format!(
                    "cannot perform arithemtic objects of type {}.",
                    self.type_name()
                ),
            )),
        }
    }

    pub fn div(&self, other: &Self) -> Result<Self, InterpreterError> {
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
            _ => Err(InterpreterError::IncompatibleTypesError(
                "".into(),
                format!(
                    "cannot perform arithemtic objects of type {}.",
                    self.type_name()
                ),
            )),
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
        }
    }
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
    pub fn interpret_string(
        s: &String,
        debug_info: Option<DebugInfo>,
    ) -> Result<(), InterpreterError> {
        let tokens = Lexer::lex_string(s.to_string(), debug_info.clone())?;
        let code_body = Parser::parse_tokens(tokens, debug_info.clone())?;
        Self::interpret_parse_tree(code_body, debug_info)?;
        Ok(())
    }

    pub fn interpret_file(path: String) -> Result<VinegarObject, FileInterpreterError> {
        let file_content = match file_handler::get_file_contents(&path) {
            Ok(contents) => contents,
            Err(err) => return Err(FileInterpreterError::IOError(err)),
        };
        let debug_info = DebugInfo::new(Some(file_content.clone()), format!("\"{}\"", path));
        let lexer_result = Lexer::lex_string(file_content, Some(debug_info.clone()));
        let tokens = match lexer_result {
            Ok(t) => t,
            Err(err) => return Err(FileInterpreterError::from(err)),
        };
        let parser_result = Parser::parse_tokens(tokens, Some(debug_info.clone()));
        let code_body = match parser_result {
            Ok(c) => c,
            Err(err) => return Err(FileInterpreterError::from(err)),
        };
        let interpreter_result = Self::interpret_parse_tree(code_body, Some(debug_info));
        match interpreter_result {
            Ok(result) => Ok(result),
            Err(err) => Err(FileInterpreterError::from(err)),
        }
    }

    fn interpret_parse_tree(
        tree_root: CodeBody,
        debug_info: Option<DebugInfo>,
    ) -> Result<VinegarObject, InterpreterError> {
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

    fn interpret(&mut self, code_body: CodeBody) -> Result<VinegarObject, InterpreterError> {
        self.import_library::<vinegar_std::StandardLibrary>();
        self.interpret_code_body(code_body)
    }

    fn interpret_code_body(
        &mut self,
        code_body: CodeBody,
    ) -> Result<VinegarObject, InterpreterError> {
        for statement in code_body.statements {
            match statement {
                Statement::Assignment(a) => {
                    self.interpret_assignment(&a)?;
                }
                Statement::Expression(e) => {
                    self.interpret_expression(&e)?;
                }
                Statement::FunctionDefinition(_) => todo!()
            };
        }
        Ok(VinegarObject::None)
    }

    fn interpret_assignment(&mut self, assignment: &Assignment) -> Result<(), InterpreterError> {
        let name = &assignment.name;
        let value: VinegarObject = self.interpret_expression(&assignment.value)?;
        let local_scope = self.local_stack.last_mut().unwrap();
        match local_scope.get_mut(name) {
            Some(v) => {
                *v = value;
            }
            None => {
                local_scope.insert(assignment.name.clone(), value);
            }
        }
        Ok(())
    }

    fn interpret_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<VinegarObject, InterpreterError> {
        match expression {
            Expression::BinaryOperator(bin_op) => self.interpret_bin_op(bin_op),
            Expression::Literal(l) => self.interpret_literal(l),
            Expression::VariableName(v) => self.interpret_variable(v),
            Expression::FunctionCall(f) => self.interpret_function_call(f),
            _ => Ok(VinegarObject::None),
        }
    }

    fn interpret_variable(&self, v: &VariableName) -> Result<VinegarObject, InterpreterError> {
        match v {
            VariableName::Final(n, range) => {
                match self.global_scope.get(n) {
                    Some(v) => return Ok(v.clone()),
                    None => (),
                };
                match self.local_stack.last().unwrap().get(n) {
                    Some(v) => return Ok(v.clone()),
                    None => (),
                };
                Err(InterpreterError::UnknownIdentifier(
                    self.get_error_prefix(v.char_range()),
                    n.clone(),
                ))
            }
            _ => unimplemented!(),
        }
    }

    fn interpret_literal(&mut self, literal: &Literal) -> Result<VinegarObject, InterpreterError> {
        match literal {
            &Literal::Int(i) => Ok(VinegarObject::Int(i)),
            &Literal::Float(f) => Ok(VinegarObject::Float(f)),
            Literal::String(s) => {
                s.hash(&mut self.string_hasher);
                let hash = self.string_hasher.finish();
                if !self.string_literals.contains_key(&hash) {
                    self.string_literals.insert(hash, s.clone());
                }
                Ok(VinegarObject::String(hash))
            }
        }
    }

    fn interpret_bin_op(
        &mut self,
        op: &Rc<BinaryOperator>,
    ) -> Result<VinegarObject, InterpreterError> {
        match &**op {
            BinaryOperator::Add(a, b) => self
                .interpret_expression(a)?
                .add(&self.interpret_expression(b)?),
            BinaryOperator::Sub(a, b) => self
                .interpret_expression(a)?
                .sub(&self.interpret_expression(b)?),
            BinaryOperator::Mul(a, b) => self
                .interpret_expression(a)?
                .mul(&self.interpret_expression(b)?),
            BinaryOperator::Div(a, b) => self
                .interpret_expression(a)?
                .div(&self.interpret_expression(b)?),
        }
    }

    fn interpret_function_call(
        &mut self,
        f: &Rc<FunctionCall>,
    ) -> Result<VinegarObject, InterpreterError> {
        let func = self.interpret_variable(&f.var)?;
        match func {
            VinegarObject::Function(expected_args, body) => {
                let given_args_len = f.args.len();
                let expected_args_len = expected_args.len();
                if given_args_len != expected_args_len {
                    return Err(InterpreterError::VinegarError(
                        self.get_error_prefix(f.var.char_range()),
                        VinegarError::InvalidArgumentsError(format!(
                            "function \"{}\" takes {} {}, but {} {} provided.",
                            f.var.debug_string(),
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

                let mut arg_results = Vec::new();
                for arg in &f.args {
                    arg_results.push(self.interpret_expression(&arg)?);
                }

                let result = match body {
                    FunctionBody::VinegarBody(b) => self.interpret_code_body(b),
                    FunctionBody::RustWrapper(w) => {
                        w.run(&self.global_scope, &self.string_literals, &arg_results)
                    }
                };
                self.local_stack.pop();
                result
            }
            _ => {
                return Err(InterpreterError::VinegarError(
                    self.get_error_prefix(f.var.char_range()),
                    VinegarError::NotCallableError(f.var.debug_string(), func.type_name().into()),
                ))
            }
        }
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
