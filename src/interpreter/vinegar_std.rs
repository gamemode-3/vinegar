use super::{
    debug::VinegarError,
    runtime::{
        Function, FunctionArg, FunctionBody, Library, RustFunctionWrapper, RustStructInterface,
        VinegarConstructor, VinegarObject, VinegarObjectConversion, VinegarScope, AsAny,
    },
    string_literal_map::{MapStringLiterals, StringLiteralMap},
};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
extern crate rust_struct_wrapper_macro;
use rust_struct_wrapper_macro::{VinegarConstructor, VinegarRustStructInterface};

fn vinegar_print(
    _global_scope: &VinegarScope,
    string_literals: &StringLiteralMap,
    args: &VinegarScope,
) -> Result<VinegarObject, VinegarError> {
    print!(
        "{}{}",
        args["value"].format_string(string_literals)?,
        args["endswith"].format_string(string_literals)?,
    );

    Ok(VinegarObject::None)
}

#[derive(Debug, VinegarRustStructInterface, VinegarConstructor)]
pub struct Duck {
    name: String,
    height_in_cm: usize,
    quack_volume: f64,
}

pub struct StandardLibrary {}

impl Library for StandardLibrary {
    fn get_globals(
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> VinegarScope {
        let mut scope = HashMap::new();
        scope.insert(
            "print".to_string(),
            VinegarObject::from(Function::new(
                vec![
                    FunctionArg::new("value".to_string(), None),
                    FunctionArg::new(
                        "endswith".to_string(),
                        Some(VinegarObject::String(
                            string_literals.add_lit("\n".into(), string_hasher),
                        )),
                    ),
                ],
                FunctionBody::RustWrapper(RustFunctionWrapper {
                    runner: &vinegar_print,
                }),
            )),
        );

        Duck::import_vinegar_constructor(&mut scope);

        scope
    }
}
