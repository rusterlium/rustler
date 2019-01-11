use proc_macro2::TokenStream;

use ::syn::{self, Data, Field, Meta, Ident, Lit};

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let record_tag: String = {
        let ref attr_value = ast.attrs.iter()
            .map(|attr| attr.parse_meta())
            .find(|meta| match meta {
                Ok(Meta::NameValue(meta_name_value)) =>
                    meta_name_value.ident == "tag",
                _ => false
            })
            .expect("NifRecord requires a 'tag' attribute");

        match *attr_value {
            Ok(Meta::NameValue(ref meta_name_value)) => match meta_name_value.lit {
                Lit::Str(ref tag) => tag.value(),
                _ => panic!("Cannot parse tag")
            }
            _ => panic!("NifRecord requires a 'tag' attribute"),
        }
    };

    let struct_fields = match ast.data {
        Data::Struct(ref data_struct) => &data_struct.fields,
        Data::Enum(_) => panic!("NifRecord can only be used with structs"),
        Data::Union(_) => panic!("NifRecord can only be used with enums"),
    };

    let num_lifetimes = ast.generics.lifetimes().count();
    if num_lifetimes > 1 {
        panic!("Struct can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

    let atom_defs = quote! {
        rustler_atoms! {
            atom atom_tag = #record_tag;
        }
    };

    let struct_fields: Vec<_> = struct_fields.iter().collect();

    let decoder = gen_decoder(&ast.ident, &struct_fields, &atom_defs, has_lifetime);
    let encoder = gen_encoder(&ast.ident, &struct_fields, &atom_defs, has_lifetime);

    let gen = quote! {
        #decoder
        #encoder
    };

    gen.into()
}

pub fn gen_decoder(struct_name: &Ident, fields: &[&Field], atom_defs: &TokenStream, has_lifetime: bool) -> TokenStream {
    // Make a decoder for each of the fields in the struct.
    let field_defs: Vec<TokenStream> = fields.iter().enumerate().map(|(idx, field)| {
        let decoder = quote! { ::rustler::Decoder::decode(terms[#idx + 1])? };

        let ident = field.ident.as_ref().unwrap();
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
    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_typ {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                let terms = ::rustler::types::tuple::get_tuple(term)?;
                if terms.len() != #field_num + 1 {
                    return Err(::rustler::Error::Atom("invalid_record"));
                }

                #atom_defs

                let tag : ::rustler::types::atom::Atom  = terms[0].decode()?;
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
    };

    gen.into()
}

pub fn gen_encoder(struct_name: &Ident, fields: &[&Field], atom_defs: &TokenStream, has_lifetime: bool) -> TokenStream {
    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<TokenStream> = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
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
    let gen = quote! {
        impl<'b> ::rustler::Encoder for #struct_typ {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                #atom_defs

                use ::rustler::Encoder;
                let arr = #field_list_ast;
                ::rustler::types::tuple::make_tuple(env, &arr)
            }
        }
    };

    gen.into()
}
