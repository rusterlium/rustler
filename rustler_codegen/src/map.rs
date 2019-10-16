use proc_macro2::{Span, TokenStream};

use syn::{self, Field, Ident};

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

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, &struct_fields, &atom_defs)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, &struct_fields, &atom_defs)
    } else {
        quote! {}
    };

    let gen = quote! {
        #decoder
        #encoder
    };

    gen
}

fn gen_decoder(ctx: &Context, fields: &[&Field], atom_defs: &TokenStream) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;
    let struct_name = ctx.ident;

    let field_defs: Vec<TokenStream> = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ident_str = ident.to_string();

            let atom_fun = Ident::new(&format!("atom_{}", ident_str), Span::call_site());
            let error_message = format!("Could not decode field :{} on %{{}}", ident.to_string());
            quote! {
                #ident: match ::rustler::Decoder::decode(term.map_get(#atom_fun().encode(env))?) {
                    Err(_) => return Err(::rustler::Error::RaiseTerm(Box::new(#error_message))),
                    Ok(value) => value
                }

            }
        })
        .collect();

    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                #atom_defs

                let env = term.get_env();
                Ok(#struct_name { #(#field_defs),* })
            }
        }
    };

    gen
}

fn gen_encoder(ctx: &Context, fields: &[&Field], atom_defs: &TokenStream) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;

    let field_defs: Vec<TokenStream> = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ident_str = field_ident.to_string();

        let atom_fun = Ident::new(&format!("atom_{}", field_ident_str), Span::call_site());

        quote! {
            map = map.map_put(#atom_fun().encode(env), self.#field_ident.encode(env)).ok().unwrap();
        }
    }).collect();

    let gen = quote! {
        impl<'b> ::rustler::Encoder for #struct_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                #atom_defs

                let mut map = ::rustler::types::map::map_new(env);
                #(#field_defs)*
                map
            }
        }
    };

    gen
}
