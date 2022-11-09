use proc_macro2::{Span, TokenStream};
use quote::quote;

use super::context::Context;

pub(crate) fn decoder(ctx: &Context, inner: TokenStream) -> TokenStream {
    let ident = ctx.ident;
    let generics = ctx.generics;
    let (_impl_generics, ty_generics, _where_clause) = generics.split_for_impl();

    // The Decoder uses a special lifetime '__rustler_decode_lifetime. We need to ensure that all
    // other lifetimes are bound to this lifetime: As we decode from a term (which has a lifetime),
    // references to that term may not outlive the term itself.
    let lifetimes: Vec<_> = generics
        .params
        .iter()
        .filter_map(|g| match g {
            syn::GenericParam::Lifetime(l) => Some(l.lifetime.clone()),
            _ => None,
        })
        .collect();

    let mut impl_generics = generics.clone();
    let decode_lifetime = syn::Lifetime::new("'__rustler_decode_lifetime", Span::call_site());
    let lifetime_def = syn::LifetimeDef::new(decode_lifetime.clone());
    impl_generics
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def));

    if !lifetimes.is_empty() {
        let where_clause = impl_generics.make_where_clause();

        for lifetime in lifetimes {
            let mut puncated = syn::punctuated::Punctuated::new();
            puncated.push(lifetime.clone());
            let predicate = syn::PredicateLifetime {
                lifetime: decode_lifetime.clone(),
                colon_token: syn::token::Colon {
                    spans: [Span::call_site()],
                },
                bounds: puncated,
            };
            where_clause.predicates.push(predicate.into());

            let mut puncated = syn::punctuated::Punctuated::new();
            puncated.push(decode_lifetime.clone());
            let predicate = syn::PredicateLifetime {
                lifetime: lifetime.clone(),
                colon_token: syn::token::Colon {
                    spans: [Span::call_site()],
                },
                bounds: puncated,
            };
            where_clause.predicates.push(predicate.into());
        }
    }

    let (impl_generics, _, where_clause) = impl_generics.split_for_impl();

    quote! {
        impl #impl_generics ::rustler::Decoder<'__rustler_decode_lifetime> for #ident #ty_generics #where_clause {
            #[allow(clippy::needless_borrow)]
            fn decode(term: ::rustler::Term<'__rustler_decode_lifetime>) -> ::rustler::NifResult<Self> {
                #inner
            }
        }
    }
}

pub(crate) fn encoder(ctx: &Context, inner: TokenStream) -> TokenStream {
    let ident = ctx.ident;
    let generics = ctx.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics ::rustler::Encoder for #ident #ty_generics #where_clause {
            #[allow(clippy::needless_borrow)]
            fn encode<'__rustler__encode_lifetime>(&self, env: ::rustler::Env<'__rustler__encode_lifetime>) -> ::rustler::Term<'__rustler__encode_lifetime> {
                #inner
            }
        }
    }
}
