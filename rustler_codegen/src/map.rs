use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use syn::{self, spanned::Spanned, Field, Ident};

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let struct_fields = ctx
        .struct_fields
        .as_ref()
        .expect("NifMap can only be used with structs");

    // Unwrap is ok here, as we already determined that struct_fields is not None
    let field_atoms = ctx.field_atoms().unwrap();

    let atom_defs = quote! {
        rustler::atoms! {
            #(#field_atoms)*
        }
    };

    let atoms_module_name = ctx.atoms_module_name(Span::call_site());

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, struct_fields, &atoms_module_name)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, struct_fields, &atoms_module_name)
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

fn gen_decoder(ctx: &Context, fields: &[&Field], atoms_module_name: &Ident) -> TokenStream {
    let struct_name = ctx.ident;

    let idents: Vec<_> = fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap())
        .collect();

    let (assignments, field_defs): (Vec<TokenStream>, Vec<TokenStream>) = fields
        .iter()
        .zip(idents.iter())
        .enumerate()
        .map(|(index, (field, ident))| {
            let atom_fun = Context::field_to_atom_fun(field);
            let variable = Context::escape_ident_with_index(&ident.to_string(), index, "map");

            let assignment = quote_spanned! { field.span() =>
            let #variable = try_decode_field(term, #atom_fun())?;
            };

            let field_def = quote! {
                #ident: #variable
            };
            (assignment, field_def)
        })
        .unzip();

    super::encode_decode_templates::decoder(
        ctx,
        quote! {
            use #atoms_module_name::*;

            fn try_decode_field<'a, T>(
                term: rustler::Term<'a>,
                field: rustler::Atom,
                ) -> ::rustler::NifResult<T>
                where
                    T: rustler::Decoder<'a>,
                {
                    match ::rustler::Decoder::decode(term.map_get(&field)?) {
                        Err(_) => Err(::rustler::Error::RaiseTerm(Box::new(format!(
                                        "Could not decode field :{:?} on %{{}}",
                                        field
                        )))),
                        Ok(value) => Ok(value),
                    }
                };

            #(#assignments);*

            Ok(#struct_name { #(#field_defs),* })
        },
    )
}

fn gen_encoder(ctx: &Context, fields: &[&Field], atoms_module_name: &Ident) -> TokenStream {
    let (keys, values): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|field| {
            let field_ident = field.ident.as_ref().unwrap();
            let atom_fun = Context::field_to_atom_fun(field);
            (
                quote! { ::rustler::Encoder::encode(&#atom_fun(), env) },
                quote! { ::rustler::Encoder::encode(&self.#field_ident, env) },
            )
        })
        .unzip();

    super::encode_decode_templates::encoder(
        ctx,
        quote! {
            use #atoms_module_name::*;
            use ::rustler::Wrapper;
            env.map_from_term_arrays(&[#(#keys),*], &[#(#values),*]).unwrap().unwrap()
        },
    )
}
