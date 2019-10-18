use proc_macro2::TokenStream;

use syn::{self, Fields, Variant};

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let variants = ctx
        .variants
        .as_ref()
        .expect("NifUntaggedEnum can only be used with enums");

    for variant in variants {
        if let Fields::Unnamed(_) = variant.fields {
            if variant.fields.iter().count() != 1 {
                panic!("NifUntaggedEnum can only be used with enums that contain all NewType variants.");
            }
        } else {
            panic!(
                "NifUntaggedEnum can only be used with enums that contain all NewType variants."
            );
        }
    }

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, &variants)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, &variants)
    } else {
        quote! {}
    };

    let gen = quote! {
        #decoder
        #encoder
    };

    gen
}

fn gen_decoder(ctx: &Context, variants: &[&Variant]) -> TokenStream {
    let enum_type = &ctx.ident_with_lifetime;
    let enum_name = ctx.ident;

    let variant_defs: Vec<_> = variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let field_type = &variant.fields.iter().next().unwrap().ty;

            quote! {
                if let Ok(inner) = #field_type::decode(term) {
                    return Ok( #enum_name :: #variant_name ( inner ) )
                }
            }
        })
        .collect();

    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #enum_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                #(#variant_defs)*

                Err(::rustler::Error::Atom("invalid_variant"))
            }
        }
    };

    gen
}

fn gen_encoder(ctx: &Context, variants: &[&Variant]) -> TokenStream {
    let enum_type = &ctx.ident_with_lifetime;
    let enum_name = ctx.ident;

    let variant_defs: Vec<_> = variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;

            quote! {
                #enum_name :: #variant_name ( ref inner ) => inner.encode(env),
            }
        })
        .collect();

    let gen = quote! {
        impl<'b> ::rustler::Encoder for #enum_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                match *self {
                    #(#variant_defs)*
                }
            }
        }
    };

    gen
}
