use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use heck::ToSnakeCase;
use std::collections::HashMap;
use syn::{self, spanned::Spanned, Field, Fields, FieldsNamed, FieldsUnnamed, Ident, Variant};

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let variants = ctx
        .variants
        .as_ref()
        .expect("NifTaggedEnum can only be used with enums");

    let atoms = variants
        .iter()
        .flat_map(|variant| {
            let mut ret = if let Fields::Named(fields) = &variant.fields {
                fields
                    .named
                    .iter()
                    .map(|field| {
                        field
                            .ident
                            .as_ref()
                            .expect("Named fields must have an ident.")
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![]
            };

            ret.push(&variant.ident);
            ret
        })
        .map(|atom_ident| {
            let atom_str = atom_ident.to_string().to_snake_case();
            let atom_fn = Context::ident_to_atom_fun(atom_ident);
            (atom_str, atom_fn)
        })
        .collect::<HashMap<_, _>>()
        .into_iter()
        .map(|(atom_str, atom_fn)| {
            quote! {
                #atom_fn = #atom_str,
            }
        })
        .collect::<Vec<_>>();

    let atom_defs = quote! {
        ::rustler::atoms! {
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
        #[allow(non_snake_case)]
        mod #atoms_module_name {
            #atom_defs
        }

        #decoder
        #encoder
    };

    gen
}

fn gen_decoder(ctx: &Context, variants: &[&Variant], atoms_module_name: &Ident) -> TokenStream {
    let enum_name = ctx.ident;
    let unit_decoders: Vec<TokenStream> = variants
        .iter()
        .filter_map(|variant| {
            let variant_ident = &variant.ident;
            let atom_fn = Context::ident_to_atom_fun(variant_ident);
            match &variant.fields {
                Fields::Unit => Some(gen_unit_decoder(enum_name, variant_ident, atom_fn)),
                _ => None,
            }
        })
        .collect();
    let named_unnamed_decoders: Vec<TokenStream> = variants
        .iter()
        .filter_map(|variant| {
            let variant_ident = &variant.ident;
            let atom_fn = Context::ident_to_atom_fun(variant_ident);
            match &variant.fields {
                Fields::Unnamed(fields) => Some(gen_unnamed_decoder(
                    enum_name,
                    fields,
                    variant.fields.iter(),
                    variant_ident,
                    atom_fn,
                )),
                Fields::Named(fields) => {
                    Some(gen_named_decoder(enum_name, fields, variant_ident, atom_fn))
                }
                _ => None,
            }
        })
        .collect();

    super::encode_decode_templates::decoder(
        ctx,
        quote! {
            use #atoms_module_name::*;

            fn try_decode_field<'a, T>(
                term: ::rustler::Term<'a>,
                field: ::rustler::Atom,
                ) -> ::rustler::NifResult<T>
                where
                    T: ::rustler::Decoder<'a>,
            {
                use ::rustler::Encoder;
                match ::rustler::Decoder::decode(term.map_get(&field)?) {
                    Err(_) => Err(::rustler::Error::RaiseTerm(Box::new(format!(
                                    "Could not decode field :{:?} on %{{}}",
                                    field
                    )))),
                    Ok(value) => Ok(value),
                }
            }

            if let Ok(unit) = ::rustler::types::atom::Atom::from_term(term) {
                #(#unit_decoders)*
            } else if let Ok(tuple) = ::rustler::Tuple::try_from(term) {
                let name = tuple
                            .get(0)
                            .and_then(|first| ::rustler::types::atom::Atom::from_term(first).ok())
                            .ok_or(::rustler::Error::RaiseAtom("invalid_variant"))?;
                #(#named_unnamed_decoders)*
            }

            Err(::rustler::Error::RaiseAtom("invalid_variant"))
        },
    )
}

fn gen_encoder(ctx: &Context, variants: &[&Variant], atoms_module_name: &Ident) -> TokenStream {
    let enum_name = ctx.ident;

    let variant_defs: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let atom_fn = Context::ident_to_atom_fun(variant_ident);

            match &variant.fields {
                Fields::Unit => gen_unit_encoder(enum_name, variant_ident, atom_fn),
                Fields::Unnamed(fields) => {
                    gen_unnamed_encoder(enum_name, fields, variant_ident, atom_fn)
                }
                Fields::Named(fields) => {
                    gen_named_encoder(enum_name, fields, variant_ident, atom_fn)
                }
            }
        })
        .collect();

    super::encode_decode_templates::encoder(
        ctx,
        quote! {
            use #atoms_module_name::*;

            match self {
                #(#variant_defs)*
            }
        },
    )
}

fn gen_unit_decoder(enum_name: &Ident, variant_ident: &Ident, atom_fn: Ident) -> TokenStream {
    quote! {
        if unit == #atom_fn() {
            return Ok ( #enum_name :: #variant_ident );
        }
    }
}

fn gen_unnamed_decoder<'a>(
    enum_name: &Ident,
    fields: &FieldsUnnamed,
    fields_iter: impl Iterator<Item = &'a Field>,
    variant_ident: &Ident,
    atom_fn: Ident,
) -> TokenStream {
    let decoded_field = &fields_iter
        .enumerate()
        .map(|(i, f)| {
            let i = i + 1;
            let ty = &f.ty;
            quote! {
                <#ty as ::rustler::Decoder>::decode(tuple.get(#i).unwrap()).map_err(|_| ::rustler::Error::RaiseTerm(
                    Box::new(format!("Could not decode field on position {}", #i))
                ))?
            }
        })
        .collect::<Vec<_>>();
    let len = fields.unnamed.len();
    quote! {
        if name == #atom_fn() {
            if tuple.len() - 1 != #len {
                return Err(::rustler::Error::RaiseTerm(Box::new(format!(
                    "The tuple must have {} elements, but it has {}",
                    #len + 1, tuple.len()
                ))));
            }
            return Ok( #enum_name :: #variant_ident ( #(#decoded_field),* ) )
        }
    }
}

fn gen_named_decoder(
    enum_name: &Ident,
    fields: &FieldsNamed,
    variant_ident: &Ident,
    atom_fn: Ident,
) -> TokenStream {
    let (assignments, field_defs): (Vec<TokenStream>, Vec<TokenStream>) = fields
        .named
        .iter()
        .enumerate()
        .map(|(index, field)| {
            let ident = field
                .ident
                .as_ref()
                .expect("Named fields must have an ident.");
            let atom_fun = Context::field_to_atom_fun(field);
            let variable = Context::escape_ident_with_index(&ident.to_string(), index, "map");

            let ident_string = ident.to_string();
            let enum_name_string = enum_name.to_string();

            let assignment = quote_spanned! { field.span() =>
                let #variable = try_decode_field(tuple.get(1).unwrap(), #atom_fun()).map_err(|_|{
                    ::rustler::Error::RaiseTerm(Box::new(format!(
                        "Could not decode field '{}' on Enum '{}'",
                        #ident_string, #enum_name_string
                    )))
                })?;
            };

            let field_def = quote! {
                #ident: #variable
            };
            (assignment, field_def)
        })
        .unzip();

    quote! {
        if tuple.len() == 2 && name == #atom_fn() {
            let len = tuple.get(1).unwrap().map_size().map_err(|_| ::rustler::Error::RaiseTerm(Box::new(
                "The second element of the tuple must be a map"
            )))?;
            #(#assignments)*
            return Ok( #enum_name :: #variant_ident { #(#field_defs),* } )
        }
    }
}

fn gen_unit_encoder(enum_name: &Ident, variant_ident: &Ident, atom_fn: Ident) -> TokenStream {
    quote! {
        #enum_name :: #variant_ident => ::rustler::Encoder::encode(&#atom_fn(), env),
    }
}

fn gen_unnamed_encoder(
    enum_name: &Ident,
    fields: &FieldsUnnamed,
    variant_ident: &Ident,
    atom_fn: Ident,
) -> TokenStream {
    let len = fields.unnamed.len();
    let inners = (0..len)
        .map(|i| Ident::new(&format!("inner{i}"), Span::call_site()))
        .collect::<Vec<_>>();
    quote! {
        #enum_name :: #variant_ident ( #(ref #inners),* ) => ::rustler::Encoder::encode(&(#atom_fn(), #(#inners),*), env),
    }
}

fn gen_named_encoder(
    enum_name: &Ident,
    fields: &FieldsNamed,
    variant_ident: &Ident,
    atom_fn: Ident,
) -> TokenStream {
    let field_decls = fields
        .named
        .iter()
        .map(|field| {
            let field_ident = &field.ident;
            quote! {
                #field_ident,
            }
        })
        .collect::<Vec<_>>();
    let (keys, values): (Vec<_>, Vec<_>) = fields
        .named
        .iter()
        .map(|field| {
            let field_ident = field
                .ident
                .as_ref()
                .expect("Named fields must have an ident.");
            let atom_fun = Context::field_to_atom_fun(field);
            (
                quote! { ::rustler::Encoder::encode(&#atom_fun(), env) },
                quote! { ::rustler::Encoder::encode(&#field_ident, env) },
            )
        })
        .unzip();
    quote! {
        #enum_name :: #variant_ident{
            #(#field_decls)*
        } => {
            let map = ::rustler::Term::map_from_term_arrays(env, &[#(#keys),*], &[#(#values),*])
                .expect("Failed to create map");
            env.make_tuple(&[::rustler::Encoder::encode(&#atom_fn(), env), map]).into()
        }
    }
}
