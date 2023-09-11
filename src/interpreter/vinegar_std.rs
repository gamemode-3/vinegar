use super::{
    debug::VinegarError,
    interpreter::{
        FunctionBody, Library, ManualHashMap, RustFunctionWrapper, RustStructInterface,
        VinegarConstructor, VinegarObject, VinegarObjectConversion, VinegarScope,
    },
};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
extern crate rust_struct_wrapper_macro;
use rust_struct_wrapper_macro::{VinegarConstructor, VinegarRustStructInterface};

fn vinegar_print(
    _global_scope: &VinegarScope,
    string_literals: &ManualHashMap<u64, String>,
    args: &Vec<VinegarObject>,
) -> Result<VinegarObject, VinegarError> {
    print!(
        "{}{}",
        args[0].format_string(string_literals)?,
        args[1].format_string(string_literals)?,
    );

    Ok(VinegarObject::None)
}

#[derive(Debug, VinegarRustStructInterface, VinegarConstructor)]
struct Duck {
    name: String,
    height_in_cm: usize,
    quack_volume: f64,
}

fn _vinegar_rep(string_literals: &ManualHashMap<u64, String>, object: &VinegarObject) -> String {
    match object {
        VinegarObject::None => todo!(),
        VinegarObject::Int(i) => format!("{:?}", i),
        VinegarObject::Float(f) => format!("{:?}", f),
        VinegarObject::String(s) => format!("{:?}", string_literals[s]),
        VinegarObject::List(l) => {
            let object_strings: Vec<String> =
                l.iter().map(|o| _vinegar_rep(string_literals, o)).collect();
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
        VinegarObject::RustStructWrapper(..) => "<RustStructWrapper>".into(),
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
                    runner: &vinegar_print,
                }),
            ),
        );

        Duck::import_vinegar_constructor(&mut scope);

        scope
    }
}
