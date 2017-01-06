use ::syn::{self, Field, VariantData, Ident};
use ::quote::{self, Tokens};

pub fn transcoder_decorator(ast: &syn::MacroInput) -> Result<quote::Tokens, &str> {
    let struct_fields = match ast.body {
        syn::Body::Struct(VariantData::Struct(ref data)) => data,
        _ => return Err("Must decorate a struct"),
    };

    let num_lifetimes = ast.generics.lifetimes.len();
    if num_lifetimes > 1 {
        return Err("Struct can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

    let decoder = gen_decoder(&ast.ident, struct_fields, has_lifetime);
    let encoder = gen_encoder(&ast.ident, struct_fields, has_lifetime);

    Ok(quote! {
        #decoder
        #encoder
    })
}

pub fn gen_decoder(struct_name: &Ident, fields: &Vec<Field>, has_lifetime: bool) -> Tokens {
    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let ident = field.clone().ident.unwrap();
        let ident_str = ident.to_string();
        quote! {
            #ident: {
                match rustler::NifDecoder::decode(
                    match rustler::map::get_map_value(
                        term,
                        rustler::atom::get_atom_init(#ident_str).to_term(env))
                    {
                        Some(term) => term,
                        None => return Err(rustler::NifError::BadArg),
                    })
                {
                    Ok(res) => res,
                    Err(err) => return Err(err),
                }
            }
        }
    }).collect();

    let struct_type = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    quote! {
        impl<'a> rustler::NifDecoder<'a> for #struct_type {
            fn decode(term: rustler::NifTerm<'a>) -> Result<Self, rustler::NifError> {
                let env = term.get_env();
                Ok(#struct_name { #(#field_defs),* })
            }
        }
    }
}

pub fn gen_encoder(struct_name: &Ident, fields: &Vec<Field>, has_lifetime: bool) -> Tokens {
    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let field_ident = field.clone().ident.unwrap();
        let field_ident_str = field_ident.to_string();
        quote! {
            map = rustler::map::map_put(
                map,
                rustler::atom::get_atom_init(#field_ident_str).to_term(env),
                self.#field_ident.encode(env)
                ).unwrap();
        }
    }).collect();

    let struct_type = if has_lifetime {
        quote! { #struct_name <'b> }
    } else {
        quote! { #struct_name }
    };

    quote! {
        impl<'a> rustler::codegen_runtime::GeneratedNifTranscoder<'a, rustler::codegen_runtime::MapType> {

        }
        impl<'b> rustler::NifEncoder for #struct_type {
            fn encode<'a>(&self, env: rustler::NifEnv<'a>) -> rustler::NifTerm<'a> {
                let mut map = rustler::map::map_new(env);
                #(field_defs)*
                map
            }
        }
    }
}
