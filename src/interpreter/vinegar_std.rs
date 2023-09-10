use super::{
    debug::InterpreterError,
    interpreter::{
        FunctionBody, Library, ManualHashMap, RustFunctionWrapper, VinegarObject, VinegarScope,
    },
};
use std::collections::HashMap;

fn vinegar_print(
    _global_scope: &VinegarScope,
    string_literals: &ManualHashMap<u64, String>,
    args: &Vec<VinegarObject>,
) -> Result<VinegarObject, InterpreterError> {
    print!("{}{}", _vinegar_to_string(string_literals, &args[0]), _vinegar_to_string(string_literals, &args[1]));

    Ok(VinegarObject::None)
}

fn _vinegar_to_string(
    string_literals: &ManualHashMap<u64, String>,
    object: &VinegarObject,
) -> String {
    match object {
        VinegarObject::None => "None".to_string(),
        VinegarObject::Int(i) => format!("{}", i),
        VinegarObject::Float(f) => format!("{}", f),
        VinegarObject::String(s) => format!("{}", string_literals[s]),
        VinegarObject::List(l) => {
            let object_strings: Vec<String> = l
                .iter()
                .map(|o| _vinegar_to_string(string_literals, o))
                .collect();
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
    }
}

fn _vinegar_rep(string_literals: &ManualHashMap<u64, String>, object: &VinegarObject) -> String {
    match object {
        VinegarObject::None => todo!(),
        VinegarObject::Int(i) => format!("{:?}", i),
        VinegarObject::Float(f) => format!("{:?}", f),
        VinegarObject::String(s) => format!("{:?}", string_literals[s]),
        VinegarObject::List(l) => {
            let object_strings: Vec<String> = l
                .iter()
                .map(|o| _vinegar_rep(string_literals, o))
                .collect();
            format!("| {} |", object_strings.join(" | "))
        }
        VinegarObject::Function(args, body) => format!(
            "{}({})",
            match body {
                FunctionBody::VinegarBody(..) => "vinegar_function",
                FunctionBody::RustWrapper(..) => "rust_function_wrapper",
            },
            args.join(", ")
        ),
    }
}

pub struct StandardLibrary {}

impl Library for StandardLibrary {
    fn get_globals() -> HashMap<String, super::interpreter::VinegarObject> {
        let mut scope = HashMap::new();
        scope.insert(
            "print".to_string(),
            VinegarObject::Function(
                vec!["value".to_string(), "endswith".to_string()],
                FunctionBody::RustWrapper(RustFunctionWrapper {
                    runner: Box::new(&vinegar_print),
                }),
            ),
        );
        scope
    }
}
