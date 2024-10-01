use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Ident, Result, Token};

#[derive(Debug)]
pub struct InitMacroInput {
    name: syn::Lit,
    load: TokenStream,
    maybe_warning: TokenStream,
}

impl Parse for InitMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = syn::Lit::parse(input)?;

        let maybe_warning = if input.peek(syn::token::Comma) && input.peek2(syn::token::Bracket) {
            // peeked, must be there
            let _ = syn::token::Comma::parse(input).unwrap();
            if let Ok(funcs) = syn::ExprArray::parse(input) {
                quote_spanned!(funcs.span() =>
                    #[allow(dead_code)]
                    fn rustler_init() {
                        #[deprecated(
                            since = "0.34.0",
                            note = "Passing NIF functions explicitly is deprecated and this argument is ignored, please remove it"
                        )]
                        #[allow(non_upper_case_globals)]
                        const explicit_nif_functions: () = ();
                        let _ = explicit_nif_functions;
                    }
                )
            } else {
                quote!()
            }
        } else {
            quote!()
        };

        let options = parse_expr_assigns(input);
        let load = extract_option(options, "load");

        Ok(InitMacroInput {
            name,
            load,
            maybe_warning,
        })
    }
}

fn parse_expr_assigns(input: ParseStream) -> Vec<syn::ExprAssign> {
    let mut vec = Vec::new();

    while <Token![,]>::parse(input).is_ok() {
        match syn::ExprAssign::parse(input) {
            Ok(expr) => vec.push(expr),
            Err(err) => panic!("{} (i.e. `load = load`)", err),
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
        let maybe_warning = input.maybe_warning;

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
            };

            unsafe {
                // Leak nif_funcs
                std::mem::forget(nif_funcs);

                NIF_ENTRY = Some(entry);
                NIF_ENTRY.as_ref().unwrap()
            }
        };

        quote! {
            #maybe_warning

            #[cfg(unix)]
            #[no_mangle]
            extern "C" fn nif_init() -> *const rustler::codegen_runtime::DEF_NIF_ENTRY {
                unsafe { rustler::codegen_runtime::internal_write_symbols() };
                #inner
            }

            #[cfg(windows)]
            #[no_mangle]
            extern "C" fn nif_init(callbacks: *mut rustler::codegen_runtime::DynNifCallbacks) -> *const rustler::codegen_runtime::DEF_NIF_ENTRY {
                unsafe {
                    rustler::codegen_runtime::internal_set_symbols(*callbacks);
                }

                #inner
            }
        }
    }
}
