use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Result, Token};

#[derive(Debug)]
pub struct InitMacroInput {
    name: syn::Lit,
    load: TokenStream,
    upgrade: Upgrade,
}

#[derive(Debug)]
enum Upgrade {
    Disabled,
    Empty,
    Callback(TokenStream),
}

impl Parse for InitMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = syn::Lit::parse(input)?;

        let options = parse_expr_assigns(input);
        let load = extract_option(&options, "load");
        let upgrade = extract_upgrade(&options);

        Ok(InitMacroInput {
            name,
            load,
            upgrade,
        })
    }
}

fn parse_expr_assigns(input: ParseStream) -> Vec<syn::ExprAssign> {
    let mut vec = Vec::new();

    while <Token![,]>::parse(input).is_ok() {
        match syn::ExprAssign::parse(input) {
            Ok(expr) => vec.push(expr),
            Err(err) => panic!("{err} (i.e. `load = load`)"),
        }
    }
    vec
}

fn extract_upgrade(args: &[syn::ExprAssign]) -> Upgrade {
    for syn::ExprAssign { left, right, .. } in args.iter() {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = &**left {
            if path.get_ident().is_some_and(|ident| *ident == "upgrade") {
                match &**right {
                    // Special case: If `upgrade` is a boolean, use an empty callback or disable upgrading, respectively
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Bool(value),
                        ..
                    }) => {
                        return if value.value {
                            Upgrade::Empty
                        } else {
                            Upgrade::Disabled
                        }
                    }

                    value => return Upgrade::Callback(quote!(#value)),
                }
            }
        }
    }

    Upgrade::Disabled
}

fn extract_option(args: &[syn::ExprAssign], name: &str) -> TokenStream {
    for syn::ExprAssign { left, right, .. } in args.iter() {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = &**left {
            if let Some(ident) = path.get_ident() {
                if *ident == name {
                    let value = &**right;
                    return quote!(Some(#value));
                }
            }
        }
    }

    let none = Ident::new("None", Span::call_site());
    quote!(#none)
}

impl From<InitMacroInput> for proc_macro2::TokenStream {
    fn from(input: InitMacroInput) -> Self {
        let name = input.name;
        let load = input.load;
        let upgrade = match input.upgrade {
            Upgrade::Disabled => quote!(None),
            Upgrade::Empty => quote!({
                extern "C" fn nif_upgrade(
                    _env: *mut rustler::sys::ErlNifEnv,
                    _old_priv_data: *mut *mut rustler::codegen_runtime::c_void,
                    _priv_data: *mut *mut rustler::codegen_runtime::c_void,
                    _load_info: rustler::sys::ErlNifTerm,
                ) -> rustler::codegen_runtime::c_int {
                    0
                }

                Some(nif_upgrade)
            }),
            Upgrade::Callback(callback) => quote!({
                extern "C" fn nif_upgrade(
                    env: *mut rustler::sys::ErlNifEnv,
                    _old_priv_data: *mut *mut rustler::codegen_runtime::c_void,
                    _priv_data: *mut *mut rustler::codegen_runtime::c_void,
                    load_info: rustler::sys::ErlNifTerm
                ) -> rustler::codegen_runtime::c_int {
                    unsafe {
                        let mut env = rustler::Env::new_init_env(&env, env);
                        let load_info = rustler::Term::new(env, load_info);

                        if !rustler::codegen_runtime::ResourceRegistration::register_all_collected(env).is_ok() {
                            return 1;
                        }

                        rustler::codegen_runtime::handle_nif_init_call(
                            #callback, env, load_info
                        )
                    }
                };

                Some(nif_upgrade)
            }),
        };

        let inner = quote! {
            static mut NIF_ENTRY: Option<rustler::codegen_runtime::ErlNifEntry> = None;
            let nif_funcs: Box<[_]> =
                rustler::codegen_runtime::inventory::iter::<rustler::Nif>()
                .map(rustler::Nif::get_def)
                .collect();

            let load = {
                extern "C" fn nif_load(
                    env: *mut rustler::sys::ErlNifEnv,
                    _priv_data: *mut *mut rustler::codegen_runtime::c_void,
                    load_info: rustler::sys::ErlNifTerm
                ) -> rustler::codegen_runtime::c_int {
                    unsafe {
                        let mut env = rustler::Env::new_init_env(&env, env);
                        let load_info = rustler::Term::new(env, load_info);

                        if !rustler::codegen_runtime::ResourceRegistration::register_all_collected(env).is_ok() {
                            return 1;
                        }

                        #load.map_or(0, |inner| {
                            rustler::codegen_runtime::handle_nif_init_call(
                                inner, env, load_info
                            )
                        })
                    }
                };

                nif_load
            };

            let upgrade = #upgrade;

            let entry = rustler::codegen_runtime::ErlNifEntry::new(
                unsafe { ::core::ffi::CStr::from_bytes_with_nul_unchecked(concat!(#name, "\0").as_bytes()) },
                &nif_funcs,
                load,
                upgrade
            );

            unsafe {
                // Leak nif_funcs
                std::mem::forget(nif_funcs);

                NIF_ENTRY = Some(entry);
                NIF_ENTRY.as_ref().unwrap()
            }
        };

        let nif_init_name = {
            let lib_name = std::env::var("CARGO_CRATE_NAME").unwrap();
            format!("{lib_name}_nif_init")
        };

        let nif_init_name = Ident::new(&nif_init_name, Span::call_site());

        let should_generate_primary_nif = std::env::var("RUSTLER_PRIMARY_NIF_INIT").is_ok()
            || std::env::var("CARGO_PRIMARY_PACKAGE").is_ok();

        let maybe_primary_nif_init = if should_generate_primary_nif {
            quote! {
                #[cfg(not(windows))]
                #[no_mangle]
                fn nif_init() -> *const ::rustler::codegen_runtime::ErlNifEntry {
                    #nif_init_name()
                }

                #[cfg(windows)]
                #[no_mangle]
                fn nif_init(callbacks: *mut ::rustler::codegen_runtime::DynNifCallbacks) -> *const ::rustler::codegen_runtime::ErlNifEntry {
                    #nif_init_name(callbacks)
                }
            }
        } else {
            quote!()
        };

        quote! {
            #[cfg(not(windows))]
            #[no_mangle]
            extern "C" fn #nif_init_name() -> *const ::rustler::codegen_runtime::ErlNifEntry {
                unsafe {
                    ::rustler::codegen_runtime::internal_write_symbols()
                }

                #inner
            }

            #[cfg(windows)]
            #[no_mangle]
            extern "C" fn #nif_init_name(callbacks: *mut ::rustler::codegen_runtime::DynNifCallbacks) -> *const ::rustler::codegen_runtime::ErlNifEntry {
                unsafe {
                    ::rustler::codegen_runtime::internal_set_symbols(callbacks);
                }

                #inner
            }

            #maybe_primary_nif_init
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{InitMacroInput, Upgrade};

    fn parse_upgrade(input: &str) -> Upgrade {
        syn::parse_str::<InitMacroInput>(input).unwrap().upgrade
    }

    #[test]
    fn upgrade_defaults_to_disabled() {
        assert!(matches!(
            parse_upgrade("\"Elixir.Test\""),
            Upgrade::Disabled
        ));
        assert!(matches!(
            parse_upgrade("\"Elixir.Test\", upgrade = false"),
            Upgrade::Disabled
        ));
    }

    #[test]
    fn true_upgrade_uses_empty_callback() {
        assert!(matches!(
            parse_upgrade("\"Elixir.Test\", upgrade = true"),
            Upgrade::Empty
        ));
    }

    #[test]
    fn upgrade_callback_is_preserved() {
        assert!(matches!(
            parse_upgrade("\"Elixir.Test\", upgrade = on_upgrade"),
            Upgrade::Callback(_)
        ));
    }
}
