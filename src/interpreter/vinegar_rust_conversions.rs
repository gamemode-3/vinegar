use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use super::{
    debug::VinegarError,
    runtime::{Function, RustStructInterface, VinegarObject, VinegarObjectConversion, wrapper_into},
    string_literal_map::StringLiteralMap,
};

impl VinegarObjectConversion<i64> for VinegarObject {
    fn into_other(&self, _: &StringLiteralMap) -> Result<i64, VinegarError> {
        self.as_i64()
    }

    fn from_other(
        value: i64,
        _string_literals: &mut StringLiteralMap,
        _string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError> {
        Ok(VinegarObject::Int(value))
    }
}

impl VinegarObjectConversion<usize> for VinegarObject {
    fn into_other(&self, _: &StringLiteralMap) -> Result<usize, VinegarError> {
        self.as_usize()
    }

    fn from_other(
        value: usize,
        _string_literals: &mut StringLiteralMap,
        _string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError> {
        Ok(VinegarObject::Int(value as i64))
    }
}

impl VinegarObjectConversion<f64> for VinegarObject {
    fn into_other(&self, _: &StringLiteralMap) -> Result<f64, VinegarError> {
        self.as_f64()
    }

    fn from_other(
        value: f64,
        _string_literals: &mut StringLiteralMap,
        _string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError> {
        Ok(VinegarObject::Float(value))
    }
}

impl VinegarObjectConversion<String> for VinegarObject {
    fn into_other(&self, string_literals: &StringLiteralMap) -> Result<String, VinegarError> {
        self.as_string(string_literals).cloned()
    }

    fn from_other(
        value: String,
        string_literals: &mut StringLiteralMap,
        string_hasher: &mut DefaultHasher,
    ) -> Result<VinegarObject, VinegarError> {
        value.hash(string_hasher);
        let hash = string_hasher.finish();
        if !string_literals.contains_key(&hash) {
            string_literals.insert(hash, value.clone());
        }
        Ok(VinegarObject::String(hash))
    }
}

impl From<usize> for VinegarObject {
    fn from(value: usize) -> Self {
        VinegarObject::Int(value as i64)
    }
}

impl From<f64> for VinegarObject {
    fn from(value: f64) -> Self {
        VinegarObject::Float(value)
    }
}

impl From<Function> for VinegarObject {
    fn from(value: Function) -> Self {
        VinegarObject::Function(value)
    }
}

impl From<VinegarObject> for i64 {
    fn from(value: VinegarObject) -> Self {
        match value.as_i64() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to usize: {:?}", e),
        }
    }
}

impl From<VinegarObject> for usize {
    fn from(value: VinegarObject) -> Self {
        match value.as_usize() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to usize: {:?}", e),
        }
    }
}

impl From<VinegarObject> for f64 {
    fn from(value: VinegarObject) -> Self {
        match value.as_f64() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to float: {:?}", e),
        }
    }
}

impl From<&VinegarObject> for i64 {
    fn from(value: &VinegarObject) -> Self {
        match value.as_i64() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to usize: {:?}", e),
        }
    }
}

impl From<&VinegarObject> for usize {
    fn from(value: &VinegarObject) -> Self {
        match value.as_usize() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to usize: {:?}", e),
        }
    }
}

impl From<&VinegarObject> for f64 {
    fn from(value: &VinegarObject) -> Self {
        match value.as_f64() {
            Ok(v) => v,
            Err(e) => panic!("Could not convert value to float: {:?}", e),
        }
    }
}

