use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use heck::ToSnakeCase;
use syn::{self, spanned::Spanned, Fields, Ident, Variant};

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let variants = ctx
        .variants
        .as_ref()
        .expect("NifEnum can only be used with enums");

    // Remove duplicated atoms.
    let atom_set = variants
        .iter()
        .flat_map(|variant| {
            let mut ret: Vec<(String, Ident)> = if let Fields::Named(fields) = &variant.fields {
                fields
                    .named
                    .iter()
                    .map(|field| {
                        let atom_str = field.ident.as_ref().expect("Atom is expected").to_string().to_snake_case();
                        let atom_fn = Ident::new(&format!("atom_{}", atom_str), Span::call_site());
                        (atom_str, atom_fn)
                    })
                    .collect()
            } else {
                vec![]
            };

            let atom_str = variant.ident.to_string().to_snake_case();
            let atom_fn = Ident::new(&format!("atom_{}", atom_str), Span::call_site());
            ret.push((atom_str, atom_fn));

            ret
        })
        .collect::<HashMap<_, _>>();

    let atoms = atom_set
        .iter()
        .map(|(atom_str, atom_fn)| {
            quote! {
                #atom_fn = #atom_str,
            }
        })
        .collect::<Vec<_>>();

    let atom_defs = quote! {
        rustler::atoms! {
            #(#atoms)*
        }
    };

    let atoms_module_name = ctx.atoms_module_name(Span::call_site());

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, variants, &atoms_module_name)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, variants, &atoms_module_name)
    } else {
        quote! {}
    };

    let gen = quote! {
        mod #atoms_module_name {
            #atom_defs
        }

        #decoder
        #encoder
    };

    gen
}

fn gen_decoder(ctx: &Context, variants: &[&Variant], atoms_module_name: &Ident) -> TokenStream {
    let enum_type = &ctx.ident_with_lifetime;
    let enum_name = ctx.ident;

    let variant_defs: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let atom_str = variant_ident.to_string().to_snake_case();
            let atom_fn = Ident::new(&format!("atom_{}", atom_str), Span::call_site());

            match &variant.fields {
                Fields::Unit => quote! {
                    if let Ok(true) = value.as_ref().map(|a| *a == #atom_fn()) {
                        return Ok ( #enum_name :: #variant_ident );
                    }
                },
                Fields::Unnamed(_) => {
                    let field_type = &variant.fields.iter().next().unwrap().ty;
                    quote! {
                        if let Ok(tuple) = ::rustler::types::tuple::get_tuple(term) {
                            let name = ::rustler::types::atom::Atom::from_term(tuple[0])?;
                            if tuple.len() == 2 && name == #atom_fn() {
                                if let Ok(inner) = <#field_type>::decode(tuple[1]) {
                                    return Ok( #enum_name :: #variant_ident ( inner ) )
                                }
                            }
                        }
                    }
                }
                Fields::Named(fields) => {
                    let idents: Vec<_> = fields
                        .named
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect();

                    let (assignments, field_defs): (Vec<TokenStream>, Vec<TokenStream>) = fields
                        .named
                        .iter()
                        .zip(idents.iter())
                        .enumerate()
                        .map(|(index, (field, ident))| {
                            let atom_fun = Context::field_to_atom_fun(field);
                            let variable =
                                Context::escape_ident_with_index(&ident.to_string(), index, "map");

                            let assignment = quote_spanned! { field.span() =>
                                let #variable = try_decode_field(env, tuple[1], #atom_fun())?;
                            };

                            let field_def = quote! {
                                #ident: #variable
                            };
                            (assignment, field_def)
                        })
                        .unzip();

                    quote! {
                        if let Ok(tuple) = ::rustler::types::tuple::get_tuple(term) {
                            let name = ::rustler::types::atom::Atom::from_term(tuple[0])?;
                            if tuple.len() == 2 && name == #atom_fn() {
                                #(#assignments)*
                                return Ok( #enum_name :: #variant_ident { #(#field_defs),* } )
                            }
                        }
                    }
                }
            }
        })
        .collect();

    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #enum_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                use #atoms_module_name::*;

                let env = term.get_env();
                let value = ::rustler::types::atom::Atom::from_term(term);

                fn try_decode_field<'a, T>(
                    env: rustler::Env<'a>,
                    term: rustler::Term<'a>,
                    field: rustler::Atom,
                    ) -> Result<T, rustler::Error>
                    where
                        T: rustler::Decoder<'a>,
                {
                    use rustler::Encoder;
                    match ::rustler::Decoder::decode(term.map_get(field.encode(env))?) {
                        Err(_) => Err(::rustler::Error::RaiseTerm(Box::new(format!(
                                        "Could not decode field :{:?} on %{{}}",
                                        field
                        )))),
                        Ok(value) => Ok(value),
                    }
                }

                #(#variant_defs)*

                Err(::rustler::Error::RaiseAtom("invalid_variant"))
            }
        }
    };

    gen
}

fn gen_encoder(ctx: &Context, variants: &[&Variant], atoms_module_name: &Ident) -> TokenStream {
    let enum_type = &ctx.ident_with_lifetime;
    let enum_name = ctx.ident;

    let variant_defs: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let atom_str = variant_ident.to_string().to_snake_case();
            let atom_fn = Ident::new(&format!("atom_{}", atom_str), Span::call_site());

            match &variant.fields {
                Fields::Unit => quote! {
                    #enum_name :: #variant_ident => #atom_fn().encode(env),
                },
                Fields::Unnamed(_) => quote! {
                    #enum_name :: #variant_ident ( ref inner ) => (#atom_fn(), inner).encode(env),
                },
                Fields::Named(fields) => {
                    let field_decls = fields.named.iter().map(|field| {
                        let field_ident = &field.ident;
                        quote! {
                            #field_ident,
                        }
                    }).collect::<Vec<_>>();
                    let field_defs = fields.named.iter()
                        .map(|field| {
                            let field_ident = field.ident.as_ref().unwrap();
                            let atom_fun = Context::field_to_atom_fun(field);

                            quote_spanned! { field.span() =>
                                map = map.map_put(#atom_fun().encode(env), #field_ident.encode(env)).unwrap();
                            }
                        })
                        .collect::<Vec<_>>();
                    quote! {
                        #enum_name :: #variant_ident{
                            #(#field_decls)*
                        } => {
                            let mut map = ::rustler::types::map::map_new(env);
                            #(#field_defs)*
                            ::rustler::types::tuple::make_tuple(env, &[#atom_fn().encode(env), map])
                        }
                    }
                }
            }
        })
        .collect();

    let gen = quote! {
        impl<'b> ::rustler::Encoder for #enum_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                use #atoms_module_name::*;

                match self {
                    #(#variant_defs)*
                }
            }
        }
    };

    gen
}
