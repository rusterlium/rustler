use proc_macro::TokenStream;
use quote::quote;
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
    let mut should_panic = quote!(None);
    let mut error = None;
    function.attrs.retain(|attr| {
        if attr.path().is_ident("ignore") {
            ignore = true;
        } else if attr.path().is_ident("should_panic") {
            match expected_message(attr) {
                Ok(expected) => should_panic = quote!(Some(#expected)),
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
    let call = match function.sig.inputs.len() {
        0 => quote!(|_env| #name()),
        1 => quote!(|env| #name(env)),
        _ => {
            return syn::Error::new(
                function.sig.inputs.span(),
                "a test takes no argument or a `rustler::Env`",
            )
            .into_compile_error()
            .into()
        }
    };

    // The test registers as a NIF with one argument, as `#[rustler::nif]`
    // registers a function. The runner knows it by the prefix of its name.
    quote! {
        #[cfg(feature = "beam_test")]
        const _: () = {
            #function
            rustler::codegen_runtime::inventory::submit!(rustler::Nif {
                // In a block of its own, so that the test sees its own items
                // by these names.
                name: {
                    const NAME: &str =
                        concat!("rustler_beam_test ", module_path!(), "::", stringify!(#name), "\0");
                    // The BEAM looks a NIF up by an atom of Latin-1 characters.
                    const _: () = assert!(
                        NAME.is_ascii() && NAME.len() <= 256,
                        "the path of a test must be ASCII and at most 237 bytes long"
                    );
                    NAME.as_ptr() as *const rustler::codegen_runtime::c_char
                },
                arity: 1,
                flags: 0,
                raw_func: {
                    unsafe extern "C" fn nif(
                        env: *mut rustler::codegen_runtime::ErlNifEnv,
                        _argc: rustler::codegen_runtime::c_int,
                        argv: *const rustler::codegen_runtime::ErlNifTerm,
                    ) -> rustler::codegen_runtime::ErlNifTerm {
                        const TEST: ::rustler_beam_test::Test = ::rustler_beam_test::Test {
                            ignore: #ignore,
                            should_panic: #should_panic,
                        };
                        unsafe { ::rustler_beam_test::step(env, argv, &TEST, #call) }
                    }
                    nif
                },
            });
        };
    }
    .into()
}

/// The text of `#[should_panic(expected = "...")]`, or an empty one, which
/// every panic message contains.
fn expected_message(attr: &syn::Attribute) -> syn::Result<String> {
    let mut expected = String::new();
    if !matches!(attr.meta, syn::Meta::Path(_)) {
        attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("expected") {
                return Err(meta.error("the only argument is `expected = \"...\"`"));
            }
            expected = meta.value()?.parse::<syn::LitStr>()?.value();
            Ok(())
        })?;
    }
    Ok(expected)
}
