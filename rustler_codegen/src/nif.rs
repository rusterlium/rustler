use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::meta::ParseNestedMeta;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::LitStr;

const VALID_SCHEDULE_OPTIONS: [&str; 3] = ["Normal", "DirtyCpu", "DirtyIo"];

#[derive(Default)]
pub struct NifAttributes {
    schedule: Option<LitStr>,
    custom_name: Option<LitStr>,
}

impl NifAttributes {
    pub fn parse(&mut self, meta: ParseNestedMeta) -> syn::parse::Result<()> {
        if meta.path.is_ident("schedule") {
            let schedule: LitStr = meta.value()?.parse()?;

            if VALID_SCHEDULE_OPTIONS.contains(&schedule.value().as_str()) {
                self.schedule = Some(schedule);
                Ok(())
            } else {
                Err(meta.error(format!(
                    "The schedule option is expecting one of the values: {VALID_SCHEDULE_OPTIONS:?}"
                )))
            }
        } else if meta.path.is_ident("name") {
            self.custom_name = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("Unsupported nif macro attribute. Expecting schedule or name."))
        }
    }
}

pub fn transcoder_decorator(nif_attributes: NifAttributes, fun: syn::ItemFn) -> TokenStream {
    let sig = &fun.sig;
    let name = &sig.ident;
    let inputs = &sig.inputs;
    let is_async = sig.asyncness.is_some();

    let flags = schedule_flag(nif_attributes.schedule);
    let function = fun.to_owned().into_token_stream();
    let arity = arity(inputs.clone());
    let erl_func_name = nif_attributes
        .custom_name
        .map_or_else(|| name.to_string(), |n| n.value().to_string());

    if !erl_func_name.is_ascii() || erl_func_name.chars().any(|x| x.is_ascii_control()) {
        panic!("Only non-Control ASCII strings are supported as function names");
    }

    if is_async {
        generate_async_nif(erl_func_name, name, flags, arity, function, inputs.clone())
    } else {
        generate_sync_nif(erl_func_name, name, flags, arity, function, inputs.clone())
    }
}

fn generate_sync_nif(
    erl_func_name: String,
    name: &syn::Ident,
    flags: TokenStream,
    arity: u32,
    function: TokenStream,
    inputs: Punctuated<syn::FnArg, Comma>,
) -> TokenStream {
    let decoded_terms = extract_inputs(inputs.clone());
    let argument_names = create_function_params(inputs);

    quote! {
        rustler::codegen_runtime::inventory::submit!(
            rustler::Nif {
                name: concat!(#erl_func_name, "\0").as_ptr()
                    as *const rustler::codegen_runtime::c_char,
                arity: #arity,
                flags: #flags as rustler::codegen_runtime::c_uint,
                raw_func: {
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
                            let result: std::thread::Result<_> =
                                std::panic::catch_unwind(move || {
                                    #decoded_terms
                                    #function
                                    Ok(#name(#argument_names))
                                });

                            rustler::codegen_runtime::handle_nif_result(
                                result, env
                            )
                        }
                        wrapper(env, &terms).apply(env)
                    }
                    nif_func
                }
            }
        );
    }
}

fn generate_async_nif(
    erl_func_name: String,
    name: &syn::Ident,
    flags: TokenStream,
    arity: u32,
    function: TokenStream,
    inputs: Punctuated<syn::FnArg, Comma>,
) -> TokenStream {
    let decoded_terms_async = extract_inputs_for_async(inputs.clone());
    let argument_names = create_function_params(inputs);

    quote! {
        // Define the original async function at module level
        #function

        // Submit the NIF wrapper to inventory
        rustler::codegen_runtime::inventory::submit!(
            rustler::Nif {
                name: concat!(#erl_func_name, "\0").as_ptr()
                    as *const rustler::codegen_runtime::c_char,
                arity: #arity,
                flags: #flags as rustler::codegen_runtime::c_uint,
                raw_func: {
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
                            // Get the calling process PID
                            let pid = env.pid();

                            // Decode all arguments before spawning async task
                            #decoded_terms_async

                            // Spawn async task on tokio runtime
                            let handle = rustler::tokio::runtime_handle();
                            handle.spawn(async move {
                                // Execute the async function and get the result
                                let value = #name(#argument_names).await;

                                // Send result back to calling process
                                let mut msg_env = rustler::OwnedEnv::new();
                                let _ = msg_env.send_and_clear(&pid, |env| {
                                    rustler::Encoder::encode(&value, env)
                                });
                            });

                            // Return :ok immediately
                            rustler::codegen_runtime::NifReturned::Term(
                                rustler::types::atom::ok().to_term(env).as_c_arg()
                            )
                        }
                        wrapper(env, &terms).apply(env)
                    }
                    nif_func
                }
            }
        );
    }
}

fn schedule_flag(schedule: Option<LitStr>) -> TokenStream {
    let mut tokens = TokenStream::new();

    let flag = schedule.map_or("Normal".into(), |lit_str| lit_str.value());
    let flag_ident = syn::Ident::new(&flag, Span::call_site());

    tokens.extend(quote! { rustler::SchedulerFlags::#flag_ident });
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
                syn::Type::Tuple(typ) => {
                    let decoder = quote! {
                        let #name: #typ = match args[#idx].decode() {
                            Ok(value) => value,
                            Err(err) => return Err(err)
                        };
                    };

                    tokens.extend(decoder);
                }
                other => {
                    panic!("unsupported input given: {other:?}");
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

                // Skip Env and CallerPid when they're the first parameter
                if i == 0 && (ident == "Env" || ident == "CallerPid") {
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

fn extract_inputs_for_async(inputs: Punctuated<syn::FnArg, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();
    let mut args_offset = 0;

    for (param_idx, item) in inputs.iter().enumerate() {
        if let syn::FnArg::Typed(ref typed) = item {
            let name = &typed.pat;
            let typ = &typed.ty;

            match &**typ {
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let ident = path.segments.last().unwrap().ident.to_string();

                    // Special case: CallerPid as first parameter
                    if param_idx == 0 && ident == "CallerPid" {
                        let caller_setup = quote! {
                            let #name: #typ = rustler::types::CallerPid::new(pid);
                        };
                        tokens.extend(caller_setup);
                        args_offset = 1; // Don't consume an arg slot
                        continue;
                    }

                    // Async functions cannot take Env or Term parameters
                    if ident == "Env" || ident == "Term" {
                        panic!(
                            "Async NIFs cannot accept '{}' parameters. \
                            All arguments must be decodable types that can be moved into the async task.",
                            ident
                        );
                    }

                    let args_idx = param_idx - args_offset;
                    let decoder = quote! {
                        let #name: #typ = match args[#args_idx].decode() {
                            Ok(value) => value,
                            Err(_) => return rustler::codegen_runtime::NifReturned::BadArg
                        };
                    };

                    tokens.extend(decoder);
                }
                syn::Type::Reference(_) => {
                    panic!(
                        "Async NIFs cannot accept reference parameters. \
                        All arguments must be owned types that can be moved into the async task."
                    );
                }
                syn::Type::Tuple(typ) => {
                    let args_idx = param_idx - args_offset;
                    let decoder = quote! {
                        let #name: #typ = match args[#args_idx].decode() {
                            Ok(value) => value,
                            Err(_) => return rustler::codegen_runtime::NifReturned::BadArg
                        };
                    };

                    tokens.extend(decoder);
                }
                other => {
                    panic!("unsupported async input type: {other:?}");
                }
            }
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };
    }

    tokens
}
