use proc_macro2::TokenStream;

use syn::{self, Data, Field, Ident};

use super::Context;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let struct_fields = match ast.data {
        Data::Struct(ref struct_data) => &struct_data.fields,
        _ => panic!("Must decorate a struct"),
    };

    let num_lifetimes = ast.generics.lifetimes().count();
    if num_lifetimes > 1 {
        panic!("Struct can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

    let struct_fields: Vec<_> = struct_fields.iter().collect();

    let decoder = if ctx.decode() {
        gen_decoder(&ast.ident, &struct_fields, false, has_lifetime)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ast.ident, &struct_fields, false, has_lifetime)
    } else {
        quote! {}
    };

    let gen = quote! {
        #decoder
        #encoder
    };

    gen
}

pub fn gen_decoder(
    struct_name: &Ident,
    fields: &[&Field],
    is_tuple: bool,
    has_lifetime: bool,
) -> TokenStream {
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

    // If the struct has a lifetime argument, put that in the struct type.
    let struct_typ = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    let field_num = field_defs.len();

    // The implementation itself
    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_typ {
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

pub fn gen_encoder(
    struct_name: &Ident,
    fields: &[&Field],
    is_tuple: bool,
    has_lifetime: bool,
) -> TokenStream {
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

    // If the struct has a lifetime argument, put that in the struct type.
    let struct_typ = if has_lifetime {
        quote! { #struct_name <'b> }
    } else {
        quote! { #struct_name }
    };

    // The implementation itself
    let gen = quote! {
        impl<'b> ::rustler::Encoder for #struct_typ {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                let arr = #field_list_ast;
                ::rustler::types::tuple::make_tuple(env, &arr)
            }
        }
    };

    gen
}
