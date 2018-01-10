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

    let decoder = gen_decoder(&ast.ident, struct_fields, false, has_lifetime);
    let encoder = gen_encoder(&ast.ident, struct_fields, false, has_lifetime);

    Ok(quote! {
        #decoder
        #encoder
    })
}

pub fn gen_decoder(struct_name: &Ident, fields: &Vec<Field>, is_tuple: bool, has_lifetime: bool) -> Tokens {
    // Make a decoder for each of the fields in the struct.
    let field_defs: Vec<Tokens> = fields.iter().enumerate().map(|(idx, field)| {
        let decoder = quote! { try!(::rustler::Decoder::decode(terms[#idx])) };

        if is_tuple {
            unimplemented!();
        } else {
            let ident = field.clone().ident.unwrap();
            quote! { #ident: #decoder }
        }
    }).collect();

    // If the struct has a lifetime argument, put that in the struct type.
    let struct_typ = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    let field_num = field_defs.len();

    // The implementation itself
    quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_typ {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::NifError> {
                let terms = try!(::rustler::types::tuple::get_tuple(term));
                if terms.len() != #field_num {
                    return Err(::rustler::NifError::BadArg);
                }
                Ok(
                    #struct_name {
                        #(#field_defs),*
                    }
                )
            }
        }
    }
}

pub fn gen_encoder(struct_name: &Ident, fields: &Vec<Field>, is_tuple: bool, has_lifetime: bool) -> Tokens {
    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<Tokens> = fields.iter().map(|field| {
        let field_source = if is_tuple {
            unimplemented!();
        } else {
            let field_ident = field.clone().ident.unwrap();
            quote! { self.#field_ident }
        };
        quote! { #field_source.encode(env) }
    }).collect();

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
    quote! {
        impl<'b> ::rustler::Encoder for #struct_typ {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                use ::rustler::Encoder;
                let arr = #field_list_ast;
                ::rustler::types::tuple::make_tuple(env, &arr)
            }
        }
    }
}
