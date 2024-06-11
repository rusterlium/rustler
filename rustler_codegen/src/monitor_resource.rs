use proc_macro2::TokenStream;
use quote::quote;

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);
    let name = ctx.ident;
    let name_s = name.to_string();

    quote! {
        impl rustler::Resource for #name {}

        rustler::codegen_runtime::inventory::submit!(
            rustler::codegen_runtime::ResourceRegistration::add_down_callback::<#name>(
                rustler::codegen_runtime::ResourceRegistration::new::<#name>(
                    #name_s
                )
            )
        );
    }
}
