use heck::SnakeCase;
use syn::{self, Body, Ident, Variant, VariantData};
use quote::{self, Tokens};

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> Result<quote::Tokens, &str> {
    let variants = match ast.body {
        Body::Enum(ref variants) => variants,
        Body::Struct(_) => panic!("NifUnitEnum can only be used with enums"),
    };

    let num_lifetimes = ast.generics.lifetimes.len();
    if num_lifetimes > 1 { panic!("Enum can only have one lifetime argument"); }
    let has_lifetime = num_lifetimes == 1;

    for variant in variants {
        if VariantData::Unit != variant.data {
            panic!("NifUnitEnum can only be used with enums that contain all unit variants.");
        }
    }

    let atoms: Vec<Tokens> = variants.iter().map(|variant| {
        let atom_str = variant.ident.to_string().to_snake_case();
        let atom_fn  = Ident::new(format!("atom_{}", atom_str));
        quote! {
            atom #atom_fn = #atom_str;
        }
    }).collect();

    let atom_defs = quote! {
        rustler_atoms! {
            #(#atoms)*
        }
    };

    let decoder = gen_decoder(&ast.ident, variants, &atom_defs, has_lifetime);
    let encoder = gen_encoder(&ast.ident, variants, &atom_defs, has_lifetime);

    Ok(quote! {
        #decoder
        #encoder
    })
}

pub fn gen_decoder(enum_name: &Ident, variants: &[Variant], atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

    let variant_defs: Vec<Tokens> = variants.iter().map(|variant| {
        let variant_ident = variant.ident.clone();
        let atom_str      = variant_ident.to_string().to_snake_case();
        let atom_fn       = Ident::new(format!("atom_{}", atom_str));

        quote! {
            if value == #atom_fn() {
                return Ok ( #enum_name :: #variant_ident );
            }
        }
    }).collect();

    quote! {
        impl<'a> ::rustler::NifDecoder<'a> for #enum_type {
            fn decode(term: ::rustler::NifTerm<'a>) -> Result<Self, ::rustler::NifError> {
                #atom_defs

                let value = ::rustler::types::atom::NifAtom::from_term(term)?;

                #(#variant_defs)*

                Err(::rustler::NifError::Atom("invalid_variant"))
            }
        }
    }
}

pub fn gen_encoder(enum_name: &Ident, variants: &[Variant], atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let enum_type = if has_lifetime {
        quote! { #enum_name <'b> }
    } else {
        quote! { #enum_name }
    };

    let variant_defs: Vec<Tokens> = variants.iter().map(|variant| {
        let variant_ident = variant.ident.clone();
        let atom_str      = variant_ident.to_string().to_snake_case();
        let atom_fn       = Ident::new(format!("atom_{}", atom_str));

        quote! {
            #enum_name :: #variant_ident => #atom_fn().encode(env),
        }
    }).collect();

    quote! {
        impl<'b> ::rustler::NifEncoder for #enum_type {
            fn encode<'a>(&self, env: ::rustler::NifEnv<'a>) -> ::rustler::NifTerm<'a> {
                #atom_defs

                match *self {
                    #(#variant_defs)*
                }
            }
        }
    }
}
