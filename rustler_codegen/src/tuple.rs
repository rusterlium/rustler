use proc_macro2::TokenStream;

use syn::{self, Field};

use super::context::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let struct_fields = ctx
        .struct_fields
        .as_ref()
        .expect("NifTuple can only be used with structs");

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, &struct_fields, false)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, &struct_fields, false)
    } else {
        quote! {}
    };

    let gen = quote! {
        #decoder
        #encoder
    };

    gen
}

fn gen_decoder(ctx: &Context, fields: &[&Field], is_tuple: bool) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;
    let struct_name = ctx.ident;

    // Make a decoder for each of the fields in the struct.
    let field_defs: Vec<TokenStream> = fields
        .iter()
        .enumerate()
        .map(|(index, field)| {
            let error_message = format!("Could not decode index {} on tuple", index);
            let decoder = quote! {
                match ::rustler::Decoder::decode(terms[#index]) {
                    Err(_) => return Err(::rustler::Error::RaiseTerm(Box::new(#error_message))),
                    Ok(value) => value
                }
            };

            if is_tuple {
                unimplemented!();
            } else {
                let ident = field.ident.as_ref().unwrap();
                quote! { #ident: #decoder }
            }
        })
        .collect();

    let field_num = field_defs.len();

    // The implementation itself
    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                let terms = ::rustler::types::tuple::get_tuple(term)?;
                if terms.len() != #field_num {
                    return Err(::rustler::Error::BadArg);
                }
                Ok(
                    #struct_name {
                        #(#field_defs),*
                    }
                )
            }
        }
    };

    gen
}

fn gen_encoder(ctx: &Context, fields: &[&Field], is_tuple: bool) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;

    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<TokenStream> = fields
        .iter()
        .map(|field| {
            let field_source = if is_tuple {
                unimplemented!();
            } else {
                let field_ident = field.ident.as_ref().unwrap();
                quote! { self.#field_ident }
            };
            quote! { #field_source.encode(env) }
        })
        .collect();

    // Build a slice ast from the field_encoders
    let field_list_ast = quote! {
        [#(#field_encoders),*]
    };

    // The implementation itself
    let gen = quote! {
        impl<'b> ::rustler::Encoder for #struct_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                let arr = #field_list_ast;
                ::rustler::types::tuple::make_tuple(env, &arr)
            }
        }
    };

    gen
}
