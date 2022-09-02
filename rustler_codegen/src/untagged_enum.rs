use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};

use syn::{self, spanned::Spanned, Fields, Variant};

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
                return quote_spanned! { variant.span() =>
                    compile_error!("NifUntaggedEnum can only be used with enums that contain all NewType variants.");
                };
            }
        } else {
            return quote_spanned! { variant.span() =>
                compile_error!("NifUntaggedEnum can only be used with enums that contain all NewType variants.");
            };
        }
    }

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, variants)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, variants)
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
    let enum_name = ctx.ident;

    let variant_defs: Vec<_> = variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let field_type = &variant.fields.iter().next().unwrap().ty;

            quote! {
                if let Ok(inner) = <#field_type as ::rustler::Decoder>::decode(term) {
                    return Ok( #enum_name :: #variant_name ( inner ) )
                }
            }
        })
        .collect();

    super::encode_decode_templates::decoder(
        ctx,
        quote! {
            #(#variant_defs)*

            Err(::rustler::Error::RaiseAtom("invalid_variant"))
        },
    )
}

fn gen_encoder(ctx: &Context, variants: &[&Variant]) -> TokenStream {
    let enum_name = ctx.ident;

    let variant_defs: Vec<_> = variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;

            quote! {
                #enum_name :: #variant_name ( ref inner ) => ::rustler::Encoder::encode(&inner, env),
            }
        })
        .collect();

    super::encode_decode_templates::encoder(
        ctx,
        quote! {
            match *self {
                #(#variant_defs)*
            }
        },
    )
}
