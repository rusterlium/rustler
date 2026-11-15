use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

/// Marks a function as a test that runs inside a BEAM.
///
/// The function takes no argument or a `rustler::Env`, and returns a
/// `rustler_beam_test::Outcome`. See the documentation of `rustler_beam_test`.
#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
    syn::parse_macro_input!(attr as syn::parse::Nothing);
    let mut function = syn::parse_macro_input!(item as syn::ItemFn);

    // The compiler knows these two only on a `#[test]` function, so the test
    // itself carries what they say.
    let mut ignore = false;
    let mut should_panic = quote!(::core::option::Option::None);
    let mut error = None;
    function.attrs.retain(|attr| {
        if attr.path().is_ident("ignore") {
            ignore = true;
        } else if attr.path().is_ident("should_panic") {
            match expected_message(attr) {
                Ok(expected) => should_panic = quote!(::core::option::Option::Some(#expected)),
                Err(invalid) => error = Some(invalid),
            }
        } else {
            return true;
        }
        false
    });
    if let Some(error) = error {
        return error.into_compile_error().into();
    }

    let name = &function.sig.ident;
    let run = match function.sig.inputs.len() {
        0 => quote!(|_| #name()),
        1 => quote!(#name),
        _ => {
            return syn::Error::new(
                function.sig.inputs.span(),
                "a test takes no argument or a `rustler::Env`",
            )
            .into_compile_error()
            .into()
        }
    };

    // The runner knows a test, and whether it is ignored, by the prefix of its
    // NIF name.
    let prefix = if ignore {
        "rustler_beam_test ignored "
    } else {
        "rustler_beam_test "
    };
    // The BEAM looks a NIF up by an atom of at most 255 Latin-1 characters.
    let too_long = format!(
        "the path of a test must be ASCII and at most {} bytes long",
        255 - prefix.len()
    );

    // The NIF that runs the test calls it by its name, so none of the names
    // that the NIF brings in can be the name of the test.
    let nif = quote::format_ident!("{name}_nif");
    let site = proc_macro::Span::mixed_site().into();
    let (env, argv) = (syn::Ident::new("env", site), syn::Ident::new("argv", site));

    // The test registers as a NIF with one argument, as `#[rustler::nif]`
    // registers a function.
    quote! {
        #[cfg(feature = "beam_test")]
        const _: () = {
            #function
            rustler::codegen_runtime::inventory::submit!(rustler::Nif {
                // In a block of its own, so that the test sees its own items
                // by these names.
                name: {
                    const NAME: &str =
                        concat!(#prefix, module_path!(), "::", stringify!(#name), "\0");
                    const _: () = assert!(NAME.is_ascii() && NAME.len() <= 256, #too_long);
                    NAME.as_ptr() as *const rustler::codegen_runtime::c_char
                },
                arity: 1,
                flags: 0,
                raw_func: {
                    unsafe extern "C" fn #nif(
                        #env: *mut rustler::codegen_runtime::ErlNifEnv,
                        _: rustler::codegen_runtime::c_int,
                        #argv: *const rustler::codegen_runtime::ErlNifTerm,
                    ) -> rustler::codegen_runtime::ErlNifTerm {
                        unsafe { ::rustler_beam_test::step(#env, #argv, #should_panic, #run) }
                    }
                    #nif
                },
            });
        };
    }
    .into()
}

/// The text of `#[should_panic = "..."]` or `#[should_panic(expected = "...")]`,
/// or an empty one, which every panic message contains.
fn expected_message(attr: &syn::Attribute) -> syn::Result<String> {
    let mut expected = String::new();
    match &attr.meta {
        syn::Meta::Path(_) => {}
        syn::Meta::NameValue(pair) => {
            expected = syn::parse2::<syn::LitStr>(pair.value.to_token_stream())?.value();
        }
        syn::Meta::List(_) => attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("expected") {
                return Err(meta.error("the only argument is `expected = \"...\"`"));
            }
            expected = meta.value()?.parse::<syn::LitStr>()?.value();
            Ok(())
        })?,
    }
    Ok(expected)
}
