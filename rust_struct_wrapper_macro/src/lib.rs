extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Visibility};

#[proc_macro_derive(VinegarRustStructInterface)]
pub fn vinegar_rust_struct_interface_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let getter_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut getters = Vec::new();
                for field in fields {
                    let field_vis = &field.vis;
                    let field_name = field.ident.as_ref().unwrap();
                    if let Visibility::Restricted(_) = field_vis {
                        continue;
                    }
                    getters.push(quote! {
                        if name == stringify!(#field_name) {
                            return VinegarObject::from_other(self.#field_name.clone(), string_literals, string_hasher);
                        }
                    });
                }

                quote! {
                    fn get_attribute(&self, name: &String, string_literals: &mut StringLiteralMap, string_hasher: &mut DefaultHasher) -> Result<VinegarObject, VinegarError> {
                        #(#getters)*
                        Err(VinegarError::AttributeNotFound(self.to_string(string_literals)?, name.clone()))
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let setter_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut setters = Vec::new();
                for field in fields {
                    let field_vis = &field.vis;
                    let field_name = field.ident.as_ref().unwrap();
                    if let Visibility::Restricted(_) = field_vis {
                        continue;
                    }
                    setters.push(quote! {
                        if name == stringify!(#field_name) {
                            self.#field_name = value.into_other(string_literals)?;
                            return Ok(());
                        }
                    });
                }

                quote! {
                    fn set_attribute(&mut self, name: &String, value: VinegarObject, string_literals: &StringLiteralMap) -> Result<(), VinegarError> {
                        #(#setters)*
                        Err(VinegarError::AttributeNotFound(self.to_string(string_literals)?, name.clone()))
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let to_string_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut arguments = Vec::new();
                for (i, field) in fields.iter().enumerate() {
                    let field_name = field.ident.as_ref().unwrap();
                    arguments.push(quote! {
                        rv.push_str(stringify!(#field_name));
                        rv.push_str(": ");
                        rv.push_str(&format!("{:?}", self.#field_name));
                    });
                    if i < fields.len() - 1 {
                        arguments.push(quote! {
                            rv.push_str(", ");
                        });
                    }
                }

                quote! {
                    fn to_string(&self, string_literals: &StringLiteralMap) -> Result<String, VinegarError> {
                        let mut rv = String::new();
                        rv.push_str(stringify!(#struct_name));
                        rv.push_str(" { ");
                        #(#arguments)*
                        rv.push_str(" }");
                        Ok(rv)
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let write_debug_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut arguments = Vec::new();
                for (i, field) in fields.iter().enumerate() {
                    let field_name = field.ident.as_ref().unwrap();
                    arguments.push(quote! {
                        write!(f, "{}", stringify!(#field_name))?;
                        write!(f, ": ")?;
                        write!(f, "{}", &format!("{:?}", self.#field_name))?;
                    });
                    if i < fields.len() - 1 {
                        arguments.push(quote! {
                            write!(f, ", ")?;
                        });
                    }
                }

                quote! {
                    fn write_debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        // write!(f, stringify!(#struct_name));
                        write!(f, " {{ ")?;
                        #(#arguments)*
                        write!(f, " }}")
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let expanded = quote! {
        impl RustStructInterface for #struct_name {
            #getter_impl
            #setter_impl
            #to_string_impl
            #write_debug_impl
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(VinegarConstructor)]
pub fn vinegar_constructor_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let constructor_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut assignments = Vec::new();
                for field in fields {
                    let field_name = field.ident.as_ref().unwrap();
                    assignments.push(quote! {
                        #field_name: VinegarObject::into_other(&args.get(stringify!(#field_name)).expect(&format!("not found: {}", stringify!(#field_name))), string_literals)?,
                    });
                }

                quote! {
                    fn new_vinegar(
                        _global_scope: &VinegarScope,
                        string_literals: &StringLiteralMap,
                        args: &VinegarScope,) -> Result<VinegarObject, VinegarError>
                    {
                        let new_struct = #struct_name {
                            #(#assignments)*
                        };
                        Ok(VinegarObject::from_struct(new_struct))
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let vinegar_func_impl = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let named_fields = &named_fields.named;
                let field_iter = named_fields.iter();
                let fields: Vec<_> = field_iter.collect();
                let mut arguments = Vec::new();
                for field in fields {
                    let field_name = field.ident.as_ref().unwrap();
                    arguments.push(quote! {
                        FunctionArg::new(stringify!(#field_name).to_string(), None),
                    });
                }

                quote! {
                    fn import_vinegar_constructor(scope: &mut VinegarScope) {
                        scope.insert(
                            stringify!(#struct_name).to_string(),
                            VinegarObject::from(
                                Function::new(
                                    vec![#(#arguments)*],
                                    FunctionBody::RustWrapper(RustFunctionWrapper {
                                        runner: &Duck::new_vinegar,
                                    }),
                                )
                            ),
                        );
                    }
                }
            }
            _ => panic!("RustStructInterface can only be derived for structs with named fields."),
        },
        _ => panic!("RustStructInterface can only be derived for structs with named fields."),
    };

    let expanded = quote! {
        impl VinegarConstructor for #struct_name {
            #constructor_impl
            #vinegar_func_impl
        }
    };

    TokenStream::from(expanded)
}
