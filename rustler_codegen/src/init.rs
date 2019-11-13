use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr, Result, Token};

#[derive(Debug)]
pub struct InitMacroInput {
    name: syn::Lit,
    funcs: syn::ExprArray,
    load: Option<syn::Expr>,
    unload: Option<syn::Expr>,
    upgrade: Option<syn::Expr>,
}

impl Parse for InitMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = syn::Lit::parse(input)?;
        let _comma = <syn::Token![,]>::parse(input)?;
        let funcs = syn::ExprArray::parse(input)?;
        let options = parse_options(input);

        let allowed = ["load", "unload", "upgrade"];

        for (key, _) in options.iter() {
            if !allowed.contains(&key.as_str()) {
                panic!("Option {} is not supported on init!()")
            }
        }

        let get = |key| options.get(key).map(Clone::clone);

        let load = get("load");
        let unload = get("unload");
        let upgrade = get("upgrade");

        Ok(InitMacroInput {
            name,
            funcs,
            load,
            unload,
            upgrade,
        })
    }
}

fn parse_options(input: ParseStream) -> HashMap<String, syn::Expr> {
    let mut result = HashMap::new();

    while let Ok(_) = <Token![,]>::parse(input) {
        match syn::ExprAssign::parse(input) {
            Ok(syn::ExprAssign { left, right, .. }) => {
                if let syn::Expr::Path(syn::ExprPath { path, .. }) = &*left {
                    if let Some(ident) = path.get_ident() {
                        result.insert(ident.to_string(), *right.clone());
                    }
                }
            }
            Err(err) => panic!("{} (i.e. `load = load`)", err),
        }
    }

    result
}

/* fn extract_options(args: &Vec<syn::ExprAssign>, name: &str) -> HashMap<&str, TokenStream> {
    for syn::ExprAssign { left, right, .. } in args.into_iter() {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = &*left {
            if let Some(ident) = path.get_ident() {
                if *ident == name {
                    let value = *right.clone();
                    return quote!(Some(#value));
                }
            }
        }
    }

    let none = Ident::new("None", Span::call_site());
    quote!(#none)
}*/

impl Into<proc_macro2::TokenStream> for InitMacroInput {
    fn into(self) -> proc_macro2::TokenStream {
        let name = self.name;
        let num_of_funcs = self.funcs.elems.len();
        let funcs = nif_funcs(self.funcs.elems);
        let load = self.load;

        let inner = quote! {
            static mut NIF_ENTRY: Option<rustler::codegen_runtime::DEF_NIF_ENTRY> = None;
            use rustler::Nif;

            let entry = rustler::codegen_runtime::DEF_NIF_ENTRY {
                major: rustler::codegen_runtime::NIF_MAJOR_VERSION,
                minor: rustler::codegen_runtime::NIF_MINOR_VERSION,
                name: concat!(#name, "\0").as_ptr() as *const u8,
                num_of_funcs: #num_of_funcs as rustler::codegen_runtime::c_int,
                funcs: [#funcs].as_ptr(),
                load: {
                    extern "C" fn nif_load(
                        env: rustler::codegen_runtime::NIF_ENV,
                        _priv_data: *mut *mut rustler::codegen_runtime::c_void,
                        load_info: rustler::codegen_runtime::NIF_TERM
                    ) -> rustler::codegen_runtime::c_int {
                        unsafe {
                            // TODO: If an unwrap ever happens, we will unwind right into C! Fix this!
                            rustler::codegen_runtime::handle_nif_init_call(Some(#load), env, load_info)
                        }
                    }
                    Some(nif_load)
                },
                reload: None,
                upgrade: None,
                unload: None,
                vm_variant: b"beam.vanilla\0".as_ptr(),
                options: 0,
                sizeof_ErlNifResourceTypeInit: rustler::codegen_runtime::get_nif_resource_type_init_size(),
            };

            unsafe {
                NIF_ENTRY = Some(entry);
                NIF_ENTRY.as_ref().unwrap()
            }
        };

        quote! {
            #[cfg(unix)]
            #[no_mangle]
            pub extern "C" fn nif_init() -> *const rustler::codegen_runtime::DEF_NIF_ENTRY {
                #inner
            }

            #[cfg(windows)]
            #[no_mangle]
            pub extern "C" fn nif_init(callbacks: *mut rustler::codegen_runtime::TWinDynNifCallbacks) -> *const rustler::codegen_runtime::DEF_NIF_ENTRY {
                unsafe {
                    rustler::codegen_runtime::WIN_DYN_NIF_CALLBACKS = Some(*callbacks);
                }

                #inner
            }
        }
    }
}

fn nif_funcs(funcs: Punctuated<Expr, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();

    for func in funcs.iter() {
        if let Expr::Path(_) = *func {
            tokens.extend(quote!(#func::FUNC,));
        } else {
            panic!("Expected an expression, found: {}", stringify!(func));
        }
    }

    tokens
}
