use syn::{Lit, Meta, NestedMeta};

use super::RustlerAttr;

///
/// A parsing context struct.
///
/// `Context` holds information usable for different codegen modules.
///
#[derive(Debug)]
pub(crate) struct Context<'a> {
    pub(crate) attrs: Vec<RustlerAttr>,
    pub(crate) ident: &'a proc_macro2::Ident,
    pub(crate) ident_with_lifetime: proc_macro2::TokenStream,
}

impl<'a> Context<'a> {
    pub(crate) fn from_ast(ast: &'a syn::DeriveInput) -> Self {
        let mut attrs: Vec<_> = ast
            .attrs
            .iter()
            .map(Context::get_rustler_attrs)
            .flatten()
            .collect();

        //
        // Default: generate encoder and decoder
        //
        if !Context::encode_decode_attr_set(&attrs) {
            attrs.push(RustlerAttr::Encode);
            attrs.push(RustlerAttr::Decode);
        }

        let has_lifetime = match ast.generics.lifetimes().count() {
            0 => false,
            1 => true,
            _ => panic!("Struct can only have one lifetime argument"),
        };

        let ident = &ast.ident;
        let ident_with_lifetime = if has_lifetime {
            quote! { #ident <'a> }
        } else {
            quote! { #ident }
        };

        Self {
            attrs,
            ident,
            ident_with_lifetime,
        }
    }

    pub(crate) fn encode(&self) -> bool {
        self.attrs.iter().any(|attr| match attr {
            RustlerAttr::Encode => true,
            _ => false,
        })
    }

    pub(crate) fn decode(&self) -> bool {
        self.attrs.iter().any(|attr| match attr {
            RustlerAttr::Decode => true,
            _ => false,
        })
    }

    fn encode_decode_attr_set(attrs: &[RustlerAttr]) -> bool {
        attrs.iter().any(|attr| match attr {
            RustlerAttr::Encode => true,
            RustlerAttr::Decode => true,
            _ => false,
        })
    }

    fn get_rustler_attrs(attr: &syn::Attribute) -> Vec<RustlerAttr> {
        attr.path
            .segments
            .iter()
            .filter_map(|segment| {
                let meta = attr.parse_meta().expect("can parse meta");
                match segment.ident.to_string().as_ref() {
                    "rustler" => Context::parse_rustler(&meta),
                    "tag" => Context::try_parse_tag(&meta),
                    "module" => Context::try_parse_module(&meta),
                    _ => None,
                }
            })
            .flatten()
            .collect()
    }

    fn parse_rustler(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::List(ref list) = meta {
            return Some(
                list.nested
                    .iter()
                    .map(Context::parse_nested_rustler)
                    .collect(),
            );
        }

        panic!("Expected encode and/or decode in rustler attribute");
    }

    fn parse_nested_rustler(nested: &NestedMeta) -> RustlerAttr {
        if let NestedMeta::Meta(ref meta) = nested {
            if let Meta::Path(ref path) = meta {
                match path.segments[0].ident.to_string().as_ref() {
                    "encode" => return RustlerAttr::Encode,
                    "decode" => return RustlerAttr::Decode,
                    other => panic!("Unexpected literal {}", other),
                }
            }
        }

        panic!("Expected encode and/or decode in rustler attribute");
    }

    fn try_parse_tag(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(ref name_value) = meta {
            if let Lit::Str(ref tag) = name_value.lit {
                return Some(vec![RustlerAttr::Tag(tag.value())]);
            }
        }
        panic!("Cannot parse module")
    }

    fn try_parse_module(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(name_value) = meta {
            if let Lit::Str(ref module) = name_value.lit {
                let ident = format!("Elixir.{}", module.value());
                return Some(vec![RustlerAttr::Module(ident)]);
            }
        }
        panic!("Cannot parse tag")
    }
}
