use proc_macro2::{Span, TokenStream};

use syn::{self, Field, Ident};

use super::context::Context;
use super::RustlerAttr;

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let elixir_module = get_module(&ctx);

    let struct_fields = ctx
        .struct_fields
        .as_ref()
        .expect("NifStruct can only be used with structs");

    // Unwrap is ok here, as we already determined that struct_fields is not None
    let field_atoms = ctx.field_atoms().unwrap();

    let atom_defs = quote! {
        rustler::atoms! {
            atom_struct = "__struct__",
            atom_module = #elixir_module,
            #(#field_atoms)*
        }
    };

    let atoms_module_name = ctx.atoms_module_name(Span::call_site());

    let decoder = if ctx.decode() {
        gen_decoder(&ctx, &struct_fields, &atoms_module_name)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ctx, &struct_fields, &atoms_module_name)
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

fn gen_decoder(ctx: &Context, fields: &[&Field], atoms_module_name: &Ident) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;
    let struct_name = ctx.ident;

    let field_defs: Vec<TokenStream> = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ident_str = ident.to_string();
            let atom_fun = Ident::new(&format!("atom_{}", ident_str), Span::call_site());
            let error_message = format!(
                "Could not decode field :{} on %{}{{}}",
                ident.to_string(),
                struct_name.to_string()
            );
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
                use #atoms_module_name::*;
                use ::rustler::Encoder;

                let env = term.get_env();

                let module: ::rustler::types::atom::Atom = term.map_get(atom_struct().to_term(env))?.decode()?;
                if module != atom_module() {
                    return Err(::rustler::Error::Atom("invalid_struct"));
                }

                Ok(#struct_name { #(#field_defs),* })
            }
        }
    };

    gen
}

fn gen_encoder(ctx: &Context, fields: &[&Field], atoms_module_name: &Ident) -> TokenStream {
    let struct_type = &ctx.ident_with_lifetime;

    let field_defs: Vec<TokenStream> = fields
        .iter()
        .map(|field| {
            let field_ident = field.ident.as_ref().unwrap();
            let field_ident_str = field_ident.to_string();
            let atom_fun = Ident::new(&format!("atom_{}", field_ident_str), Span::call_site());
            quote! {
                map = map.map_put(#atom_fun().encode(env), self.#field_ident.encode(env)).unwrap();
            }
        })
        .collect();

    let gen = quote! {
        impl<'b> ::rustler::Encoder for #struct_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                use #atoms_module_name::*;
                let mut map = ::rustler::types::map::map_new(env);
                map = map.map_put(atom_struct().encode(env), atom_module().encode(env)).unwrap();
                #(#field_defs)*
                map
            }
        }
    };

    gen
}

fn get_module(ctx: &Context) -> String {
    ctx.attrs
        .iter()
        .find_map(|attr| match attr {
            RustlerAttr::Module(ref module) => Some(module.clone()),
            _ => None,
        })
        .expect("NifStruct requires a 'module' attribute")
}
