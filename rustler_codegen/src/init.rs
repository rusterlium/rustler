use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Result, Token};

#[derive(Debug)]
pub struct InitMacroInput {
    name: syn::Lit,
    load: TokenStream,
}

impl Parse for InitMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = syn::Lit::parse(input)?;

        let options = parse_expr_assigns(input);
        let load = extract_option(options, "load");

        Ok(InitMacroInput { name, load })
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

fn extract_option(args: Vec<syn::ExprAssign>, name: &str) -> TokenStream {
    for syn::ExprAssign { left, right, .. } in args.into_iter() {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = &*left {
            if let Some(ident) = path.get_ident() {
                if *ident == name {
                    let value = *right;
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

        let inner = quote! {
            static mut NIF_ENTRY: Option<rustler::codegen_runtime::DEF_NIF_ENTRY> = None;
            let nif_funcs: Box<[_]> =
                rustler::codegen_runtime::inventory::iter::<rustler::Nif>()
                .map(rustler::Nif::get_def)
                .collect();

            let entry = rustler::codegen_runtime::DEF_NIF_ENTRY {
                major: rustler::codegen_runtime::NIF_MAJOR_VERSION,
                minor: rustler::codegen_runtime::NIF_MINOR_VERSION,
                name: concat!(#name, "\0").as_ptr() as *const rustler::codegen_runtime::c_char,
                num_of_funcs: nif_funcs.len() as rustler::codegen_runtime::c_int,
                funcs: nif_funcs.as_ptr(),
                load: {
                    extern "C" fn nif_load(
                        env: rustler::codegen_runtime::NIF_ENV,
                        _priv_data: *mut *mut rustler::codegen_runtime::c_void,
                        load_info: rustler::codegen_runtime::NIF_TERM
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
                    }
                    Some(nif_load)
                },
                reload: None,
                upgrade: None,
                unload: None,
                vm_variant: b"beam.vanilla\0".as_ptr() as *const rustler::codegen_runtime::c_char,
                options: 0,
                sizeof_ErlNifResourceTypeInit: rustler::codegen_runtime::get_nif_resource_type_init_size(),
                min_erts: rustler::codegen_runtime::min_erts().as_ptr() as *const rustler::codegen_runtime::c_char,
            };

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
                fn nif_init() -> *const ::rustler::codegen_runtime::DEF_NIF_ENTRY {
                    #nif_init_name()
                }

                #[cfg(windows)]
                #[no_mangle]
                fn nif_init(callbacks: *mut ::rustler::codegen_runtime::DynNifCallbacks) -> *const ::rustler::codegen_runtime::DEF_NIF_ENTRY {
                    #nif_init_name(callbacks)
                }
            }
        } else {
            quote!()
        };

        quote! {
            #[cfg(not(windows))]
            #[no_mangle]
            extern "C" fn #nif_init_name() -> *const ::rustler::codegen_runtime::DEF_NIF_ENTRY {
                unsafe {
                    ::rustler::codegen_runtime::internal_write_symbols()
                }

                #inner
            }

            #[cfg(windows)]
            #[no_mangle]
            extern "C" fn #nif_init_name(callbacks: *mut ::rustler::codegen_runtime::DynNifCallbacks) -> *const ::rustler::codegen_runtime::DEF_NIF_ENTRY {
                unsafe {
                    ::rustler::codegen_runtime::internal_set_symbols(*callbacks);
                }

                #inner
            }

            #maybe_primary_nif_init
        }
    }
}
