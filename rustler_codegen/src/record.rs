use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use syn::{self, spanned::Spanned, Field, Ident, Index};

use super::context::Context;
use super::RustlerAttr;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let record_tag = get_tag(&ctx);

    let struct_fields = ctx
        .struct_fields
        .as_ref()
        .expect("NifRecord can only be used with structs");

    let atom_defs = quote! {
        rustler::atoms! {
            atom_tag = #record_tag,
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

    // Make a decoder for each of the fields in the struct.
    let (assignments, field_defs): (Vec<TokenStream>, Vec<TokenStream>) = fields
        .iter()
        .enumerate()
        .map(|(index, field)| {
            let ident = field.ident.as_ref();
            let pos_in_struct = if let Some(ident) = ident {
                ident.to_string()
            } else {
                index.to_string()
            };
            let actual_index = index + 1;

            let variable = Context::escape_ident(&pos_in_struct, "record");

            let assignment = quote_spanned! { field.span() =>
                let #variable = try_decode_index(&terms, #pos_in_struct, #actual_index)?;
            };

            let field_def = match ident {
                None => quote! { #variable },
                Some(ident) => {
                    quote! { #ident: #variable }
                }
            };

            (assignment, field_def)
        })
        .unzip();

    let field_num = field_defs.len();
    let struct_name_str = struct_name.to_string();

    // The implementation itself
    let construct = if ctx.is_tuple_struct {
        quote! {
            #(#assignments);*
            Ok(#struct_name ( #(#field_defs),* ))
        }
    } else {
        quote! {
            #(#assignments);*
            Ok(#struct_name { #(#field_defs),* })
        }
    };

    super::encode_decode_templates::decoder(
        ctx,
        quote! {
            use #atoms_module_name::*;

            let terms = match ::rustler::types::tuple::get_tuple(term) {
                Err(_) => return Err(::rustler::Error::RaiseTerm(
                        Box::new(format!("Invalid Record structure for {}", #struct_name_str)))),
                Ok(value) => value,
            };

            if terms.len() != #field_num + 1 {
                return Err(::rustler::Error::RaiseAtom("invalid_record"));
            }

            let tag : ::rustler::types::atom::Atom = terms[0].decode()?;

            if tag != atom_tag() {
                return Err(::rustler::Error::RaiseAtom("invalid_record"));
            }

            fn try_decode_index<'a, T>(terms: &[::rustler::Term<'a>], pos_in_struct: &str, index: usize) -> ::rustler::NifResult<T>
                where
                    T: rustler::Decoder<'a>,
            {
                match ::rustler::Decoder::decode(terms[index]) {
                    Err(_) => Err(::rustler::Error::RaiseTerm(Box::new(
                                format!("Could not decode field {} on Record {}", pos_in_struct, #struct_name_str)))),
                    Ok(value) => Ok(value)
                }
            }

            #construct
        },
    )
}

fn gen_encoder(ctx: &Context, fields: &[&Field], atoms_module_name: &Ident) -> TokenStream {
    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<TokenStream> = fields
        .iter()
        .enumerate()
        .map(|(index, field)| {
            let literal_index = Index::from(index);
            let field_source = match field.ident.as_ref() {
                None => quote! { self.#literal_index },
                Some(ident) => quote! { self.#ident },
            };

            quote_spanned! { field.span() => ::rustler::Encoder::encode(&#field_source, env) }
        })
        .collect();

    let tag_encoder = quote! { ::rustler::Encoder::encode(&atom_tag(), env) };

    // Build a slice ast from the field_encoders

    let field_list_ast = quote! {
        [#tag_encoder, #(#field_encoders),*]
    };

    super::encode_decode_templates::encoder(
        ctx,
        quote! {
            use #atoms_module_name::*;

            use ::rustler::Encoder;
            let arr = #field_list_ast;
            ::rustler::types::tuple::make_tuple(env, &arr)
        },
    )
}

fn get_tag(ctx: &Context) -> String {
    ctx.attrs
        .iter()
        .find_map(|attr| match attr {
            RustlerAttr::Tag(ref tag) => Some(tag.clone()),
            _ => None,
        })
        .expect("NifStruct requires a 'tag' attribute")
}
