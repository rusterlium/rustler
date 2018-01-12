use ::syn::{self, Field, VariantData, MetaItem, Lit, Ident};
use ::quote::{self, Tokens};

pub fn transcoder_decorator(ast: &syn::MacroInput) -> Result<quote::Tokens, &str> {
    let record_tag = {
        let ref attr_value = ast.attrs.first().expect("NifRecord requires a 'tag' attribute").value;
        assert!(attr_value.name() == "tag", "NifRecord requires a 'tag' attribute");
        match *attr_value {
            MetaItem::NameValue(_, Lit::Str(ref value, _)) => value,
            _ => panic!("NifRecord requires a 'tag' attribute"),
        }
    };

    let struct_fields = match ast.body {
        syn::Body::Struct(VariantData::Struct(ref data)) => data,
        _ => return Err("Must decorate a struct"),
    };

    let num_lifetimes = ast.generics.lifetimes.len();
    if num_lifetimes > 1 {
        return Err("Struct can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

    let atom_defs = quote! {
        rustler_atoms! {
            atom atom_tag = #record_tag;
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
    // Make a decoder for each of the fields in the struct.
    let field_defs: Vec<Tokens> = fields.iter().enumerate().map(|(idx, field)| {
        let decoder = quote! { try!(::rustler::Decoder::decode(terms[#idx + 1])) };

        let ident = field.clone().ident.unwrap();
        quote! { #ident: #decoder }
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
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                let terms = try!(::rustler::types::tuple::get_tuple(term));
                if terms.len() != #field_num + 1 {
                    return Err(::rustler::Error::Atom("invalid_record"));
                }

                #atom_defs

                let tag : ::rustler::types::atom::NifAtom  = terms[0].decode()?;
                if tag != atom_tag() {
                    return Err(::rustler::Error::Atom("invalid_record"));
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

pub fn gen_encoder(struct_name: &Ident, fields: &Vec<Field>, atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<Tokens> = fields.iter().map(|field| {
        let field_ident = field.clone().ident.unwrap();
        let field_source = quote! { self.#field_ident };
        quote! { #field_source.encode(env) }
    }).collect();

    let tag_encoder = quote! { atom_tag().encode(env) };

    // Build a slice ast from the field_encoders

    let field_list_ast = quote! {
        [#tag_encoder, #(#field_encoders),*]
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
                #atom_defs

                use ::rustler::Encoder;
                let arr = #field_list_ast;
                ::rustler::types::tuple::make_tuple(env, &arr)
            }
        }
    }
}
