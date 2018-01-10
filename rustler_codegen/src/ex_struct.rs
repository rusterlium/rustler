use ::syn::{self, Body, Field, MetaItem, Lit, Ident};
use ::quote::{self, Tokens};

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> Result<quote::Tokens, &str> {
    let elixir_module = {
        let ref attr_value = ast.attrs.first().expect("NifStruct requires a 'module' attribute").value;
        assert!(attr_value.name() == "module", "NifStruct requires a 'module' attribute");
        match *attr_value {
            MetaItem::NameValue(_, Lit::Str(ref value, _)) => format!("Elixir.{}", value),
            _ => panic!("NifStruct requires a 'module' attribute"),
        }
    };

    let struct_fields = match ast.body {
        Body::Struct(ref data) => data.fields(),
        Body::Enum(_) => panic!("NifStruct can only be used with structs"),
    };

    let num_lifetimes = ast.generics.lifetimes.len();
    if num_lifetimes > 1 { panic!("Struct can only have one lifetime argument"); }
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
            atom atom_struct = "__struct__";
            atom atom_module = #elixir_module;
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

pub fn gen_decoder(struct_name: &Ident, fields: &[Field], atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let struct_type = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let ident = field.clone().ident.unwrap();
        let ident_str = ident.to_string();
        let atom_fun = Ident::new(format!("atom_{}", ident_str));
        quote! {
            #ident: ::rustler::Decoder::decode(term.map_get(#atom_fun().encode(env))?)?
        }
    }).collect();

    quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                use ::rustler::Encoder;

                #atom_defs

                let env = term.get_env();
                let module: ::rustler::types::atom::NifAtom = term.map_get(atom_struct().to_term(env))?.decode()?;
                if module != atom_module() {
                    return Err(::rustler::Error::Atom("invalid_struct"));
                }

                Ok(#struct_name { #(#field_defs),* })
            }
        }
    }
}

pub fn gen_encoder(struct_name: &Ident, fields: &[Field], atom_defs: &Tokens, has_lifetime: bool) -> Tokens {
    let struct_type = if has_lifetime {
        quote! { #struct_name <'b> }
    } else {
        quote! { #struct_name }
    };

    let field_defs: Vec<Tokens> = fields.iter().map(|field| {
        let field_ident = field.clone().ident.unwrap();
        let field_ident_str = field_ident.to_string();
        let atom_fun = Ident::new(format!("atom_{}", field_ident_str));
        quote! {
            map = map.map_put(#atom_fun().encode(env), self.#field_ident.encode(env)).ok().unwrap();
        }
    }).collect();

    quote! {
        impl<'b> ::rustler::Encoder for #struct_type {
            fn encode<'a>(&self, env: ::rustler::Env<'a>) -> ::rustler::Term<'a> {
                #atom_defs

                let mut map = ::rustler::types::map::map_new(env);
                map = map.map_put(atom_struct().encode(env), atom_module().encode(env)).ok().unwrap();
                #(#field_defs)*
                map
            }
        }
    }
}
