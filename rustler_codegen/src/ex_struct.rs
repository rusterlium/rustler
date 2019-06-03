use proc_macro2::{Span, TokenStream};

use syn::{self, Data, Field, Ident, Lit, Meta};

use super::{Context, RustlerAttr};

pub fn transcoder_decorator(ast: &syn::DeriveInput) -> TokenStream {
    let ctx = Context::from_ast(ast);

    let elixir_module = get_module(ast, &ctx);

    let struct_fields = match ast.data {
        Data::Struct(ref data_struct) => &data_struct.fields,
        Data::Enum(_) => panic!("NifStruct can only be used with structs"),
        Data::Union(_) => panic!("NifStruct can only be used with structs"),
    };

    let num_lifetimes = ast.generics.lifetimes().count();
    if num_lifetimes > 1 {
        panic!("Struct can only have one lifetime argument");
    }
    let has_lifetime = num_lifetimes == 1;

    let field_atoms: Vec<TokenStream> = struct_fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ident_str = ident.to_string();

            let atom_fun = Ident::new(&format!("atom_{}", ident_str), Span::call_site());

            quote! {
                atom #atom_fun = #ident_str;
            }
        })
        .collect();

    let atom_defs = quote! {
        ::rustler::rustler_atoms! {
            atom atom_struct = "__struct__";
            atom atom_module = #elixir_module;
            #(#field_atoms)*
        }
    };

    let struct_fields: Vec<_> = struct_fields.iter().collect();

    let decoder = if ctx.decode() {
        gen_decoder(&ast.ident, &struct_fields, &atom_defs, has_lifetime)
    } else {
        quote! {}
    };

    let encoder = if ctx.encode() {
        gen_encoder(&ast.ident, &struct_fields, &atom_defs, has_lifetime)
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
    atom_defs: &TokenStream,
    has_lifetime: bool,
) -> TokenStream {
    let struct_type = if has_lifetime {
        quote! { #struct_name <'a> }
    } else {
        quote! { #struct_name }
    };

    let field_defs: Vec<TokenStream> = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ident_str = ident.to_string();
            let atom_fun = Ident::new(&format!("atom_{}", ident_str), Span::call_site());
            let error_message = format!("Could not decode field :{} on %{}{{}}", ident.to_string(), struct_name.to_string());
            quote! {
                #ident: match ::rustler::Decoder::decode(term.map_get(#atom_fun().encode(env))?) {
                    Err(_) => return Err(::rustler::Error::RaiseTerm(Box::new(#error_message))),
                    Ok(stuff) => stuff
                }
            }
        })
        .collect();

    let gen = quote! {
        impl<'a> ::rustler::Decoder<'a> for #struct_type {
            fn decode(term: ::rustler::Term<'a>) -> Result<Self, ::rustler::Error> {
                use ::rustler::Encoder;

                #atom_defs

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

pub fn gen_encoder(
    struct_name: &Ident,
    fields: &[&Field],
    atom_defs: &TokenStream,
    has_lifetime: bool,
) -> TokenStream {
    let struct_type = if has_lifetime {
        quote! { #struct_name <'b> }
    } else {
        quote! { #struct_name }
    };

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
                map = map.map_put(atom_struct().encode(env), atom_module().encode(env)).ok().unwrap();
                #(#field_defs)*
                map
            }
        }
    };

    gen
}

fn get_module(ast: &syn::DeriveInput, ctx: &Context) -> String {
    ctx.attrs
        .iter()
        .find_map(|attr| match attr {
            RustlerAttr::Module(ref module) => Some(module.clone()),
            _ => None,
        })
        .or_else(|| {
            let attr_value =
                &ast.attrs
                    .iter()
                    .map(|attr| attr.parse_meta())
                    .find(|meta| match meta {
                        Ok(Meta::NameValue(meta_name_value)) => meta_name_value.ident == "module",
                        _ => false,
                    });

            match *attr_value {
                Some(Ok(Meta::NameValue(ref meta_name_value))) => match meta_name_value.lit {
                    Lit::Str(ref module) => Some(format!("Elixir.{}", module.value())),
                    _ => panic!("Cannot parse module"),
                },
                _ => None,
            }
        })
        .expect("NifStruct requires a 'module' attribute")
}
