use ::syn::{self, Body, Ident, Variant, VariantData};
use ::quote::{self, Tokens};

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> Result<quote::Tokens, &str> {
    let variants = match ast.body {
        Body::Enum(ref variants) => variants,
        Body::Struct(_) => panic!("NifUntaggedEnum can only be used with enums"),
    };

    let num_lifetimes = ast.generics.lifetimes.len();
    if num_lifetimes > 1 { panic!("Enum can only have one lifetime argument"); }
    let has_lifetime = num_lifetimes == 1;

    for variant in variants {
        if let VariantData::Tuple(ref fields) = variant.data {
            if fields.len() != 1 {
                panic!("NifUntaggedEnum can only be used with enums that contain all NewType variants.");
            }
        } else {
            panic!("NifUntaggedEnum can only be used with enums that contain all NewType variants.");
        }
    }

    let decoder = gen_decoder(&ast.ident, variants, has_lifetime);
    let encoder = gen_encoder(&ast.ident, variants, has_lifetime);

    Ok(quote! {
        #decoder
        #encoder
    })
}

pub fn gen_decoder(enum_name: &Ident, variants: &[Variant], has_lifetime: bool) -> Tokens {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

    let variant_defs: Vec<Tokens> = variants.iter().map(|variant| {
        let variant_name = variant.ident.clone();
        let field_type   = variant.data.fields()[0].ty.clone();

        quote! {
            if let Ok(inner) = #field_type::decode(term) {
                return Ok( #enum_name :: #variant_name ( inner ) )
            }
        }
    }).collect();

    quote! {
        impl<'a> ::rustler::Decoder<'a> for #enum_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::NifError> {
                #(#variant_defs)*

                Err(::rustler::NifError::Atom("invalid_variant"))
            }
        }
    }
}

pub fn gen_encoder(enum_name: &Ident, variants: &[Variant], has_lifetime: bool) -> Tokens {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

    let variant_defs: Vec<Tokens> = variants.iter().map(|variant| {
        let variant_name = variant.ident.clone();

        quote! {
            #enum_name :: #variant_name ( ref inner ) => inner.encode(env),
        }
    }).collect();

    quote! {
        impl<'b> ::rustler::Encoder for #enum_type {
            fn encode<'a>(&self, env: ::rustler::NifEnv<'a>) -> ::rustler::Term<'a> {
                match *self {
                    #(#variant_defs)*
                }
            }
        }
    }
}
