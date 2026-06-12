use std::fmt::Display;

use syn::{Ident, Lit, Meta};

pub(crate) trait TryFromRustlerNestedAttr: Sized {
    fn collect_attrs_for_ident(ident: &Ident, meta: &Meta) -> Option<Vec<Self>>;
    fn try_from_rustler_nested_attr(ident: &Ident) -> Option<Self>;
    fn parse_failure_message() -> impl Display;

    fn parse_rustler<T: TryFromRustlerNestedAttr>(meta: &Meta) -> Vec<T> {
        if let Meta::List(ref list) = meta {
            let mut attrs: Vec<T> = vec![];
            let _ = list.parse_nested_meta(|nested_meta| {
                let parsed_attr = nested_meta
                    .path
                    .get_ident()
                    .and_then(T::try_from_rustler_nested_attr);

                match parsed_attr {
                    None => Err(nested_meta.error(T::parse_failure_message())),
                    Some(attr) => {
                        attrs.push(attr);
                        Ok(())
                    }
                }
            });

            return attrs;
        }

        panic!("Expected nested attributes inside the rustler attribute");
    }
}

#[derive(Debug)]
pub(crate) enum RustlerAttr {
    Encode,
    Decode,
    Module(String),
    Tag(String),
}

impl RustlerAttr {
    fn try_parse_tag(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(ref name_value) = meta {
            let expr = &name_value.value;

            if let syn::Expr::Lit(lit_expr) = expr {
                if let Lit::Str(ref tag) = lit_expr.lit {
                    return Some(vec![RustlerAttr::Tag(tag.value())]);
                }
            }
        }
        panic!("Cannot parse tag")
    }

    fn try_parse_module(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(name_value) = meta {
            let expr = &name_value.value;

            if let syn::Expr::Lit(lit_expr) = expr {
                if let Lit::Str(ref module) = lit_expr.lit {
                    let ident = format!("Elixir.{}", module.value());
                    return Some(vec![RustlerAttr::Module(ident)]);
                }
            }
        }
        panic!("Cannot parse module")
    }
}

impl TryFromRustlerNestedAttr for RustlerAttr {
    fn parse_failure_message() -> impl Display {
        "Expected encode, decode and/or optional in rustler attribute"
    }

    fn collect_attrs_for_ident(ident: &Ident, meta: &Meta) -> Option<Vec<Self>> {
        match ident.to_string().as_ref() {
            "rustler" => Some(Self::parse_rustler(meta)),
            "tag" => Self::try_parse_tag(meta),
            "module" => Self::try_parse_module(meta),
            _ => None,
        }
    }

    fn try_from_rustler_nested_attr(ident: &Ident) -> Option<Self> {
        match ident.to_string().as_ref() {
            "encode" => Some(Self::Encode),
            "decode" => Some(Self::Decode),
            _ => None,
        }
    }
}
