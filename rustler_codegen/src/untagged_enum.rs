use proc_macro2::TokenStream;

use syn::{self, Data, Fields, Ident, Variant};

use super::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);
    let variants = match ast.data {
        Data::Enum(ref data_enum) => &data_enum.variants,
        Data::Struct(_) => panic!("NifUntaggedEnum can only be used with enums"),
        Data::Union(_) => panic!("NifUntaggedEnum can only be used with enums"),
    };

    let num_lifetimes = ast.generics.lifetimes().count();
    if num_lifetimes > 1 {
        panic!("Enum can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

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

    let variants: Vec<&Variant> = variants.iter().collect();

    let decoder = if ctx.decode() {
        gen_decoder(&ast.ident, &variants, has_lifetime)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ast.ident, &variants, has_lifetime)
    } else {
        quote! {}
    };

    let gen = quote! {
        #decoder
        #encoder
    };

    gen.into()
}

pub fn gen_decoder(enum_name: &Ident, variants: &[&Variant], has_lifetime: bool) -> TokenStream {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

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

    gen.into()
}

pub fn gen_encoder(enum_name: &Ident, variants: &[&Variant], has_lifetime: bool) -> TokenStream {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

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

    gen.into()
}
