use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::collections::HashSet;

pub fn transcoder_decorator(mut input: syn::ItemImpl) -> TokenStream {
    // Should be `Resource` but will fail somewhere else anyway if it isn't.
    // let (_, _trait_path, _) = input.trait_.unwrap();
    let type_path = match *input.self_ty {
        syn::Type::Path(ref type_path) => type_path.clone(),
        _ => panic!("Can only implement trait on concrete types"),
    };

    let mut to_add: HashSet<String> = HashSet::new();
    let mut already_has: HashSet<String> = HashSet::new();

    for item in input.items.iter() {
        if let syn::ImplItem::Fn(f) = item {
            to_add.insert(
                format!("IMPLEMENTS_{}", f.sig.ident.to_string().to_uppercase()).to_string(),
            );
        }

        if let syn::ImplItem::Const(f) = item {
            already_has.insert(f.ident.to_string());
        }
    }

    for add in to_add.difference(&already_has) {
        let ident = syn::Ident::new(add, Span::call_site());
        let impl_item: syn::ImplItem = syn::parse_quote!(const #ident: bool = true;);

        input.items.push(impl_item);
    }

    quote!(
        #input

        rustler::codegen_runtime::inventory::submit!(
            rustler::codegen_runtime::ResourceRegistration::new::<#type_path>()
        );
    )
}
