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

    quote! {
        #[cfg(feature = "beam_test")]
        const _: () = {
            #function
            ::rustler_beam_test::__test!(#name, #ignore, #should_panic, #call);
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
