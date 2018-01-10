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

    let field_atoms: Vec<Tokens> = struct_fields.iter().map(|field| {
        let ident = field.clone().ident.unwrap();
        let ident_str = ident.to_string();

        let atom_fun = Ident::new(format!("atom_{}", ident_str));

        quote! {
            atom #atom_fun = #ident_str;
        }
    }).collect();
    let atom_defs = quote! {
        rustler_atoms! {
            #(#field_atoms)*
        }
    };

    let decoder = gen_decoder(&ast.ident, struct_fields, &atom_defs, has_lifetime);
    let encoder = gen_encoder(&ast.ident, struct_fields, &atom_defs, has_lifetime);

    Ok(quote! {
        #decoder
        #encoder
    })
}

pub fn gen_decoder(struct_name: &Ident, fields: &Vec<Field>, atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let ident = field.clone().ident.unwrap();
        let ident_str = ident.to_string();

        let atom_fun = Ident::new(format!("atom_{}", ident_str));

        quote! {
            #ident: ::rustler::Decoder::decode(term.map_get(#atom_fun().encode(env))?)?
        }
    }).collect();

    let struct_type = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::NifError> {
                #atom_defs

                let env = term.get_env();
                Ok(#struct_name { #(#field_defs),* })
            }
        }
    }
}

pub fn gen_encoder(struct_name: &Ident, fields: &Vec<Field>, atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let field_ident = field.clone().ident.unwrap();
        let field_ident_str = field_ident.to_string();

        let atom_fun = Ident::new(format!("atom_{}", field_ident_str));

        quote! {
            map = map.map_put(#atom_fun().encode(env), self.#field_ident.encode(env)).ok().unwrap();
        }
    }).collect();

    let struct_type = if has_lifetime {
        quote! { #struct_name <'b> }
    } else {
        quote! { #struct_name }
    };

    quote! {
        impl<'b> ::rustler::Encoder for #struct_type {
            fn encode<'a>(&self, env: ::rustler::NifEnv<'a>) -> ::rustler::Term<'a> {
                #atom_defs

                let mut map = ::rustler::types::map::map_new(env);
                #(#field_defs)*
                map
            }
        }
    }
}
