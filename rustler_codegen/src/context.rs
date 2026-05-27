use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, Data, Field, Fields, Ident, Lifetime, Lit, Meta, TypeParam, Variant};

use super::RustlerAttr;

///
/// A parsing context struct.
///
/// `Context` holds information usable for different codegen modules.
///
pub(crate) struct Context<'a> {
    pub attrs: Vec<RustlerAttr>,
    pub ident: &'a proc_macro2::Ident,
    pub generics: &'a syn::Generics,
    pub lifetimes: Vec<Lifetime>,
    pub type_parameters: Vec<TypeParam>,
    pub variants: Option<Vec<&'a Variant>>,
    pub struct_fields: Option<Vec<&'a Field>>,
    pub is_tuple_struct: bool,
}

impl<'a> Context<'a> {
    pub fn from_ast(ast: &'a syn::DeriveInput) -> Self {
        let mut attrs: Vec<_> = ast
            .attrs
            .iter()
            .flat_map(Context::get_rustler_attrs)
            .collect();

        //
        // Default: generate encoder and decoder
        //
        if !Context::encode_decode_attr_set(&attrs) {
            attrs.push(RustlerAttr::Encode);
            attrs.push(RustlerAttr::Decode);
        }

        let variants = match ast.data {
            Data::Enum(ref data_enum) => Some(data_enum.variants.iter().collect()),
            _ => None,
        };

        let struct_fields = match ast.data {
            Data::Struct(ref data_struct) => Some(data_struct.fields.iter().collect()),
            _ => None,
        };

        let is_tuple_struct = match ast.data {
            Data::Struct(ref data_struct) => matches!(data_struct.fields, Fields::Unnamed(_)),
            _ => false,
        };

        let lifetimes: Vec<_> = ast
            .generics
            .params
            .iter()
            .filter_map(|g| match g {
                syn::GenericParam::Lifetime(l) => Some(l.lifetime.clone()),
                _ => None,
            })
            .collect();

        let type_parameters: Vec<_> = ast
            .generics
            .params
            .iter()
            .filter_map(|g| match g {
                syn::GenericParam::Type(t) => Some(t.clone()),
                // Don't keep lifetimes or generic constants
                _ => None,
            })
            .collect();

        Self {
            attrs,
            ident: &ast.ident,
            generics: &ast.generics,
            lifetimes,
            type_parameters,
            variants,
            struct_fields,
            is_tuple_struct,
        }
    }

    pub fn atoms_module_name(&self, span: Span) -> Ident {
        Ident::new(
            &format!("rustler_atoms_{}", self.ident).to_snake_case(),
            span,
        )
    }

    pub fn encode(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Encode))
    }

    pub fn decode(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Decode))
    }

    pub fn field_atoms(&self) -> Option<Vec<TokenStream>> {
        self.struct_fields.as_ref().map(|struct_fields| {
            struct_fields
                .iter()
                .map(|field| {
                    let atom_fun = Self::field_to_atom_fun(field);
                    let atom_name = Self::field_atom_name(field);

                    quote! {
                        #atom_fun = #atom_name,
                    }
                })
                .collect()
        })
    }

    pub fn field_to_atom_fun(field: &Field) -> Ident {
        Self::atom_fun(&Self::field_atom_name(field))
    }

    pub fn field_atom_name(field: &Field) -> String {
        Self::rename_attr(&field.attrs).unwrap_or_else(|| {
            let ident = field.ident.as_ref().unwrap();
            Self::ident_to_atom_name(ident)
        })
    }

    pub fn variant_to_atom_fun(variant: &Variant) -> Ident {
        Self::atom_fun(&Self::variant_atom_name(variant))
    }

    pub fn variant_atom_name(variant: &Variant) -> String {
        Self::rename_attr(&variant.attrs)
            .unwrap_or_else(|| Self::ident_to_atom_name(&variant.ident))
    }

    pub fn ident_to_atom_name(ident: &Ident) -> String {
        let ident_str = ident.to_string();
        Self::remove_raw(&ident_str).to_snake_case()
    }

    fn atom_fun(atom_name: &str) -> Ident {
        let suffix = atom_name
            .as_bytes()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();

        Ident::new(&format!("atom_{}", suffix), Span::call_site())
    }

    pub fn escape_ident_with_index(ident_str: &str, index: usize, infix: &str) -> Ident {
        Ident::new(
            &format!(
                "rustler_{}_field_{}_{}",
                infix,
                index,
                Self::remove_raw(ident_str)
            ),
            Span::call_site(),
        )
    }

    pub fn escape_ident(ident_str: &str, infix: &str) -> Ident {
        Ident::new(
            &format!("rustler_{}_field_{}", infix, Self::remove_raw(ident_str)),
            Span::call_site(),
        )
    }

    fn remove_raw(ident_str: &str) -> &str {
        ident_str
            .split("r#")
            .last()
            .expect("split has always at least one element")
    }

    fn rename_attr(attrs: &[Attribute]) -> Option<String> {
        attrs.iter().find_map(|attr| {
            if !attr.path().is_ident("rustler") {
                return None;
            }

            let Meta::List(list) = &attr.meta else {
                return None;
            };

            let mut rename = None;
            list.parse_nested_meta(|nested_meta| {
                if nested_meta.path.is_ident("rename") {
                    let value = nested_meta.value()?;
                    let lit: syn::LitStr = value.parse()?;
                    rename = Some(lit.value());
                    Ok(())
                } else {
                    Err(nested_meta.error("Expected rename in rustler attribute"))
                }
            })
            .unwrap_or_else(|err| panic!("{}", err));

            rename
        })
    }

    fn encode_decode_attr_set(attrs: &[RustlerAttr]) -> bool {
        attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Encode | RustlerAttr::Decode))
    }

    fn get_rustler_attrs(attr: &syn::Attribute) -> Vec<RustlerAttr> {
        attr.path()
            .segments
            .iter()
            .filter_map(|segment| {
                let meta = &attr.meta;
                match segment.ident.to_string().as_ref() {
                    "rustler" => Some(Context::parse_rustler(meta)),
                    "tag" => Context::try_parse_tag(meta),
                    "module" => Context::try_parse_module(meta),
                    _ => None,
                }
            })
            .flatten()
            .collect()
    }

    fn parse_rustler(meta: &Meta) -> Vec<RustlerAttr> {
        if let Meta::List(ref list) = meta {
            let mut attrs: Vec<RustlerAttr> = vec![];
            let _ = list.parse_nested_meta(|nested_meta| {
                if nested_meta.path.is_ident("encode") {
                    attrs.push(RustlerAttr::Encode);
                    Ok(())
                } else if nested_meta.path.is_ident("decode") {
                    attrs.push(RustlerAttr::Decode);
                    Ok(())
                } else {
                    Err(nested_meta.error("Expected encode and/or decode in rustler attribute"))
                }
            });

            return attrs;
        }

        panic!("Expected encode and/or decode in rustler attribute");
    }

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
