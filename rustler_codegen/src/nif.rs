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
    executor: bool,
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
        } else if meta.path.is_ident("executor") {
            self.executor = true;
            Ok(())
        } else {
            Err(meta
                .error("Unsupported nif macro attribute. Expecting schedule, name or executor."))
        }
    }
}

pub fn transcoder_decorator(
    nif_attributes: NifAttributes,
    fun: syn::ItemFn,
    is_task: bool,
) -> TokenStream {
    let sig = &fun.sig;
    let name = &sig.ident;
    let inputs = &sig.inputs;
    let is_async = sig.asyncness.is_some();
    let executor = nif_attributes.executor;
    if executor && !is_async {
        return syn::Error::new_spanned(sig, "executor requires an async NIF").to_compile_error();
    }

    let flags = schedule_flag(nif_attributes.schedule);
    let function = fun.to_owned().into_token_stream();
    let arity = arity(inputs.clone());
    let erl_func_name = nif_attributes
        .custom_name
        .map_or_else(|| name.to_string(), |n| n.value().to_string());

    if !erl_func_name.is_ascii() || erl_func_name.chars().any(|x| x.is_ascii_control()) {
        panic!("Only non-Control ASCII strings are supported as function names");
    }

    let unstable_guard = is_async.then(|| {
        quote! {
            #[cfg(not(rustler_unstable))]
            const _: () = compile_error!(
                "async NIFs are an unstable rustler feature; build with \
                 `RUSTFLAGS=\"--cfg rustler_unstable\"` to use them"
            );
        }
    });

    if is_task {
        let generated = generate_task(
            erl_func_name,
            name,
            flags,
            arity,
            function,
            inputs.clone(),
            &sig.output,
        );
        return quote! {
            #unstable_guard
            #generated
        };
    }

    let decoded_terms = extract_inputs(inputs.clone());
    let argument_names = create_function_params(inputs.clone());
    let raw_func_name = if executor {
        format!("__rustler_async_{erl_func_name}")
    } else {
        erl_func_name.clone()
    };

    let wrapper = if sig.asyncness.is_some() {
        quote! {
            fn factory<'a>(
                env: rustler::Env<'a, rustler::ProcessIndependent>,
                args: Vec<rustler::Term<'a>>
            ) -> rustler::codegen_runtime::AsyncNifFuture<'a> {
                let result_env = rustler::types::atom::nil().to_term(env.as_env());
                Box::pin(async move {
                    let result: Result<_, rustler::Error> = async {
                        #decoded_terms
                        #function
                        Ok(#name(#argument_names).await)
                    }.await;
                    rustler::codegen_runtime::handle_nif_result(
                        Ok(result), result_env.get_env()
                    )
                })
            }
            rustler::codegen_runtime::start_async_nif(
                env, &terms, #erl_func_name, #flags, #executor, factory
            ).apply(env)
        }
    } else {
        quote! {
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
    };

    let registration = sig.asyncness.as_ref().map(|_| {
        quote! {
            rustler::codegen_runtime::inventory::submit!(
                rustler::codegen_runtime::AsyncNifRegistration { executor: #executor }
            );
        }
    });

    quote! {
        #unstable_guard
        #registration
        rustler::codegen_runtime::inventory::submit!(
            rustler::Nif {
                name: concat!(#raw_func_name, "\0").as_ptr()
                    as *const rustler::codegen_runtime::c_char,
                arity: #arity,
                flags: #flags as rustler::codegen_runtime::c_uint,
                raw_func: {
                    unsafe extern "C" fn nif_func(
                        nif_env: *mut rustler::sys::ErlNifEnv,
                        argc: rustler::codegen_runtime::c_int,
                        argv: *const rustler::sys::ErlNifTerm
                    ) -> rustler::sys::ErlNifTerm {
                        let lifetime = ();
                        let env = rustler::Env::new(&lifetime, nif_env);

                        let terms = std::slice::from_raw_parts(argv, argc as usize)
                            .iter()
                            .map(|term| rustler::Term::new(env, *term))
                            .collect::<Vec<rustler::Term>>();

                        #wrapper
                    }
                    nif_func
                }
            }
        );
    }
}

fn generate_task(
    erl_func_name: String,
    name: &syn::Ident,
    flags: TokenStream,
    arity: u32,
    function: TokenStream,
    inputs: Punctuated<syn::FnArg, Comma>,
    return_type: &syn::ReturnType,
) -> TokenStream {
    // Check if first parameter is Channel<Request, Response>
    // and extract the types if present
    let channel_info = inputs.first().and_then(|arg| {
        if let syn::FnArg::Typed(typed) = arg {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &*typed.ty {
                let segment = path.segments.last()?;
                if segment.ident == "Channel" {
                    // Return the full type for generating Channel::new
                    return Some(typed.ty.clone());
                }
            }
        }
        None
    });

    let uses_channel = channel_info.is_some();

    let decoded_terms_async = extract_inputs_for_async(inputs.clone(), return_type);
    let argument_names = create_function_params(inputs);

    // Determine the Channel type to use
    let channel_type = if let Some(ty) = channel_info {
        // Use the type from the function signature
        ty
    } else {
        // Default to Channel<(), Response> where Response is the return type
        let response_type = match return_type {
            syn::ReturnType::Type(_, ty) => ty.clone(),
            syn::ReturnType::Default => {
                panic!("Async tasks must have an explicit return type");
            }
        };
        syn::parse_quote! { rustler::runtime::Channel<(), #response_type> }
    };

    // Generate code for sending the final result
    let (clone_setup, send_result) = if uses_channel {
        // When using Channel, the function is responsible for calling finish()
        // The macro just executes the function and does nothing with the result
        let send = quote! {
            // Function is responsible for calling channel.finish()
        };
        (quote! {}, send)
    } else {
        // When not using Channel, clone channel_sender before async block
        let clone = quote! {
            let channel_sender_for_send = channel_sender.clone();
        };
        let send = quote! {
            let mut msg_env = rustler::OwnedEnv::new();
            let _ = msg_env.send_and_clear(&pid, |env| {
                use rustler::Encoder;
                (channel_sender_for_send, value).encode(env)
            });
        };
        (clone, send)
    };

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
                        nif_env: *mut rustler::sys::ErlNifEnv,
                        argc: rustler::codegen_runtime::c_int,
                        argv: *const rustler::sys::ErlNifTerm
                    ) -> rustler::sys::ErlNifTerm {
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

                            // Create channel - if task doesn't use Channel param,
                            // still create Channel<(), Response> for message tagging
                            let (channel_sender, channel): (_, #channel_type) = rustler::runtime::Channel::new(pid);

                            // Clone channel_sender if needed (for tasks without Channel param)
                            #clone_setup

                            // Decode all arguments before spawning async task
                            #decoded_terms_async

                            // Spawn async task
                            rustler::spawn(async move {
                                // Execute the async function
                                #[allow(unused_variables)]
                                let value = #name(#argument_names).await;

                                // Send {channel_sender, result} back to calling process
                                #send_result
                            });

                            // Return the channel sender as task reference
                            use rustler::Encoder;
                            rustler::codegen_runtime::NifReturned::Term(
                                channel_sender.encode(env).as_c_arg()
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

                // Skip Env, Caller, and Channel when they're the first parameter
                if i == 0 && (ident == "Env" || ident == "Caller" || ident == "Channel") {
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

fn extract_inputs_for_async(
    inputs: Punctuated<syn::FnArg, Comma>,
    return_type: &syn::ReturnType,
) -> TokenStream {
    let mut tokens = TokenStream::new();
    let mut args_offset = 0;

    // Check if first parameter is Channel (determines if explicit return type is required)
    let has_channel = inputs
        .first()
        .and_then(|arg| {
            if let syn::FnArg::Typed(typed) = arg {
                if let syn::Type::Path(syn::TypePath { path, .. }) = &*typed.ty {
                    return path.segments.last().map(|s| s.ident == "Channel");
                }
            }
            None
        })
        .unwrap_or(false);

    // Validate that async tasks have an explicit return type (unless they have a Channel parameter)
    if !has_channel && matches!(return_type, syn::ReturnType::Default) {
        panic!("Async tasks must have an explicit return type");
    }

    for (param_idx, item) in inputs.iter().enumerate() {
        if let syn::FnArg::Typed(ref typed) = item {
            let name = &typed.pat;
            let typ = &typed.ty;

            match &**typ {
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let ident = path.segments.last().unwrap().ident.to_string();

                    // Special case: Channel<Request, Response> as first parameter
                    if param_idx == 0 && ident == "Channel" {
                        // Channel is already created by wrapper, just pass it through
                        // No need to decode from args, and it doesn't consume an arg slot
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
