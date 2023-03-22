use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::LitStr;

pub fn transcoder_decorator(
    schedule: Option<LitStr>,
    custom_name: Option<LitStr>,
    fun: syn::ItemFn,
) -> TokenStream {
    let sig = &fun.sig;
    let name = &sig.ident;
    let inputs = &sig.inputs;

    let flags = schedule_flag(schedule);
    let function = fun.to_owned().into_token_stream();
    let arity = arity(inputs.clone());
    let decoded_terms = extract_inputs(inputs.clone());
    let argument_names = create_function_params(inputs.clone());
    let erl_func_name = custom_name
        .map(|n| syn::Ident::new(n.value().as_str(), Span::call_site()))
        .unwrap_or_else(|| name.clone());

    quote! {
        #[allow(non_camel_case_types)]
        pub struct #name;

        impl rustler::Nif for #name {
            const NAME: *const u8 = concat!(stringify!(#erl_func_name), "\0").as_ptr() as *const u8;
            const ARITY: u32 = #arity;
            const FLAGS: u32 = #flags as u32;
            const RAW_FUNC: unsafe extern "C" fn(
                nif_env: rustler::codegen_runtime::NIF_ENV,
                argc: rustler::codegen_runtime::c_int,
                argv: *const rustler::codegen_runtime::NIF_TERM
            ) -> rustler::codegen_runtime::NIF_TERM = {
                unsafe extern "C" fn nif_func(
                    nif_env: rustler::codegen_runtime::NIF_ENV,
                    argc: rustler::codegen_runtime::c_int,
                    argv: *const rustler::codegen_runtime::NIF_TERM
                ) -> rustler::codegen_runtime::NIF_TERM {
                    let lifetime = ();
                    let env = rustler::Env::new(&lifetime, nif_env);

                    let terms = std::slice::from_raw_parts(argv, argc as usize)
                        .iter()
                        .map(|term| rustler::Term::new(env, *term))
                        .collect::<Vec<rustler::Term>>();

                    fn wrapper<'a>(
                        env: rustler::Env<'a>,
                        args: &[rustler::Term<'a>]
                    ) -> rustler::codegen_runtime::NifReturned {
                        let result: std::thread::Result<_> = std::panic::catch_unwind(move || {
                            #decoded_terms
                            #function
                            Ok(#name(#argument_names))
                        });

                        rustler::codegen_runtime::handle_nif_result(result, env)
                    }
                    wrapper(env, &terms).apply(env)
                }
                nif_func
            };
            const FUNC: rustler::codegen_runtime::DEF_NIF_FUNC = rustler::codegen_runtime::DEF_NIF_FUNC {
                arity: Self::ARITY,
                flags: Self::FLAGS,
                function: Self::RAW_FUNC,
                name: Self::NAME
            };
        }
    }
}

fn schedule_flag(schedule: Option<LitStr>) -> TokenStream {
    let mut tokens = TokenStream::new();

    let valid = ["DirtyCpu", "DirtyIo", "Normal"];

    let flag = match schedule {
        Some(lit_str) => {
            let value = lit_str.value();

            if valid.contains(&value.as_str()) {
                syn::Ident::new(value.as_str(), Span::call_site())
            } else {
                panic!("Invalid schedule option `{}`", value);
            }
        }
        None => syn::Ident::new("Normal", Span::call_site()),
    };

    tokens.extend(quote! { rustler::SchedulerFlags::#flag });
    tokens
}

fn extract_inputs(inputs: Punctuated<syn::FnArg, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();
    let mut idx: usize = 0;

    for item in inputs.iter() {
        if let syn::FnArg::Typed(ref typed) = item {
            let name = &typed.pat;

            match &*typed.ty {
                syn::Type::Reference(typ) => {
                    let decoder = quote! {
                        let #name: #typ = match args[#idx].decode() {
                            Ok(value) => value,
                            Err(err) => return Err(err)
                        };
                    };

                    tokens.extend(decoder);
                }
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let typ = &typed.ty;
                    let ident = path.segments.last().unwrap().ident.to_string();

                    match ident.as_ref() {
                        "Env" => {
                            continue;
                        }
                        "Term" => {
                            let arg = quote! {
                                let #name: #typ = args[#idx];
                            };

                            tokens.extend(arg);
                        }
                        _ => {
                            let decoder = quote! {
                                let #name: #typ = match args[#idx].decode() {
                                    Ok(value) => value,
                                    Err(err) => return Err(err)
                                };
                            };

                            tokens.extend(decoder);
                        }
                    }
                }
                other => {
                    panic!("unsupported input given: {:?}", other);
                }
            }
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };
        idx += 1;
    }

    tokens
}

fn create_function_params(inputs: Punctuated<syn::FnArg, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();

    for item in inputs.iter() {
        let name = if let syn::FnArg::Typed(ref typed) = item {
            &typed.pat
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };

        tokens.extend(quote!(#name,));
    }

    tokens
}

fn arity(inputs: Punctuated<syn::FnArg, Comma>) -> u32 {
    let mut arity: u32 = 0;

    for (i, item) in inputs.iter().enumerate() {
        if let syn::FnArg::Typed(ref typed) = item {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &*typed.ty {
                let ident = path.segments.last().unwrap().ident.to_string();

                if i == 0 && ident == "Env" {
                    continue;
                }

                if ident == "Env" {
                    panic!("Env must be the first argument in NIF functions");
                }
            };
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };
        arity += 1;
    }

    arity
}
