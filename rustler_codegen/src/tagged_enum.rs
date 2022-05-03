use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use heck::ToSnakeCase;
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
        mod #atoms_module_name {
            #atom_defs
        }

        #decoder
        #encoder
    };

    gen
}

fn invalid_variant() -> TokenStream {
    quote! {
        ::rustler::Error::RaiseAtom("invalid_variant")
    }
}

fn gen_decoder(ctx: &Context, variants: &[&Variant], atoms_module_name: &Ident) -> TokenStream {
    let invalid_variant = invalid_variant();
    let enum_type = &ctx.ident_with_lifetime;
    let enum_name = ctx.ident;

    let variant_defs: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let atom_fn = Context::ident_to_atom_fun(variant_ident);

            match &variant.fields {
                Fields::Unit => gen_unit_decoder(enum_name, variant_ident, atom_fn),
                Fields::Unnamed(fields) => gen_unnamed_decoder(
                    enum_name,
                    fields,
                    variant.fields.iter(),
                    variant_ident,
                    atom_fn,
                ),
                Fields::Named(fields) => {
                    gen_named_decoder(enum_name, fields, variant_ident, atom_fn)
                }
            }
        })
        .collect();

    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #enum_type {
            fn decode(term: ::rustler::Term<'a>) -> ::rustler::NifResult<Self> {
                use #atoms_module_name::*;

                let env = term.get_env();
                let value = ::rustler::types::atom::Atom::from_term(term);

                fn try_decode_field<'a, T>(
                    env: ::rustler::Env<'a>,
                    term: ::rustler::Term<'a>,
                    field: ::rustler::Atom,
                    ) -> ::rustler::NifResult<T>
                    where
                        T: ::rustler::Decoder<'a>,
                {
                    use ::rustler::Encoder;
                    match ::rustler::Decoder::decode(term.map_get(field.encode(env))?) {
                        Err(_) => Err(::rustler::Error::RaiseTerm(Box::new(format!(
                                        "Could not decode field :{:?} on %{{}}",
                                        field
                        )))),
                        Ok(value) => Ok(value),
                    }
                }

                #(#variant_defs)*

                Err(#invalid_variant)
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

fn gen_unit_decoder(enum_name: &Ident, variant_ident: &Ident, atom_fn: Ident) -> TokenStream {
    quote! {
        if let Ok(true) = value.as_ref().map(|a| *a == #atom_fn()) {
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
    let invalid_variant = invalid_variant();
    let decoded_field = &fields_iter
        .enumerate()
        .map(|(i, f)| {
            let i = i + 1;
            let ty = &f.ty;
            quote! {
                <#ty>::decode(tuple[#i]).map_err(|_| #invalid_variant)?
            }
        })
        .collect::<Vec<_>>();
    let len = fields.unnamed.len();
    quote! {
        if let Ok(tuple) = ::rustler::types::tuple::get_tuple(term) {
            let name = tuple
                .get(0)
                .and_then(|&first| ::rustler::types::atom::Atom::from_term(first).ok())
                .ok_or(#invalid_variant)?;
            if name == #atom_fn() {
                if tuple.len() - 1 != #len {
                    return Err(#invalid_variant);
                }
                return Ok( #enum_name :: #variant_ident ( #(#decoded_field),* ) )
            }
        }
    }
}

fn gen_named_decoder(
    enum_name: &Ident,
    fields: &FieldsNamed,
    variant_ident: &Ident,
    atom_fn: Ident,
) -> TokenStream {
    let invalid_variant = invalid_variant();
    let idents = fields.named.iter().map(|field| {
        field
            .ident
            .as_ref()
            .expect("Named fields must have an ident.")
    });

    let (assignments, field_defs): (Vec<TokenStream>, Vec<TokenStream>) = fields
        .named
        .iter()
        .zip(idents)
        .enumerate()
        .map(|(index, (field, ident))| {
            let atom_fun = Context::field_to_atom_fun(field);
            let variable = Context::escape_ident_with_index(&ident.to_string(), index, "map");

            let assignment = quote_spanned! { field.span() =>
                let #variable = try_decode_field(env, tuple[1], #atom_fun())
                  .map_err(|_| #invalid_variant)?;
            };

            let field_def = quote! {
                #ident: #variable
            };
            (assignment, field_def)
        })
        .unzip();
    let len = fields.named.len();

    quote! {
        if let Ok(tuple) = ::rustler::types::tuple::get_tuple(term) {
            let name = tuple
                .get(0)
                .and_then(|&first| ::rustler::types::atom::Atom::from_term(first).ok())
                .ok_or(#invalid_variant)?;
            if tuple.len() == 2 && name == #atom_fn() {
                if tuple[1]
                    .map_size()
                    .ok()
                    .and_then(|len| (len == #len).then(|| ()))
                    .is_none()
                {
                    return Err(#invalid_variant);
                }
                #(#assignments)*
                return Ok( #enum_name :: #variant_ident { #(#field_defs),* } )
            }
        }
    }
}

fn gen_unit_encoder(enum_name: &Ident, variant_ident: &Ident, atom_fn: Ident) -> TokenStream {
    quote! {
        #enum_name :: #variant_ident => #atom_fn().encode(env),
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
        .map(|i| Ident::new(&format!("inner{}", i), Span::call_site()))
        .collect::<Vec<_>>();
    quote! {
        #enum_name :: #variant_ident ( #(ref #inners),* ) => (#atom_fn(), #(#inners),*).encode(env),
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
    let field_defs = fields.named.iter()
        .map(|field| {
            let field_ident = field.ident.as_ref().expect("Named fields must have an ident.");
            let atom_fun = Context::field_to_atom_fun(field);

            quote_spanned! { field.span() =>
                map = map.map_put(#atom_fun().encode(env), #field_ident.encode(env)).expect("Failed to putting map");
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
