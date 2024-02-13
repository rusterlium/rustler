use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{GenericArgument, PathSegment, TraitBound};

use super::context::Context;

pub(crate) fn decoder(ctx: &Context, inner: TokenStream) -> TokenStream {
    let ident = ctx.ident;
    let generics = ctx.generics;
    let (_impl_generics, ty_generics, _where_clause) = generics.split_for_impl();

    // The Decoder uses a special lifetime '__rustler_decode_lifetime. We need to ensure that all
    // other lifetimes are bound to this lifetime: As we decode from a term (which has a lifetime),
    // references to that term may not outlive the term itself.
    let mut impl_generics = generics.clone();
    let decode_lifetime = syn::Lifetime::new("'__rustler_decode_lifetime", Span::call_site());
    let lifetime_def = syn::LifetimeParam::new(decode_lifetime.clone());
    impl_generics
        .params
        .push(syn::GenericParam::Lifetime(lifetime_def));

    let where_clause = impl_generics.make_where_clause();

    for lifetime in ctx.lifetimes.iter() {
        let mut punctuated = syn::punctuated::Punctuated::new();
        punctuated.push(lifetime.clone());
        let predicate = syn::PredicateLifetime {
            lifetime: decode_lifetime.clone(),
            colon_token: syn::token::Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause.predicates.push(predicate.into());

        let mut punctuated = syn::punctuated::Punctuated::new();
        punctuated.push(decode_lifetime.clone());
        let predicate = syn::PredicateLifetime {
            lifetime: lifetime.clone(),
            colon_token: syn::token::Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause.predicates.push(predicate.into());
    }

    for type_parameter in ctx.type_parameters.iter() {
        let mut punctuated = syn::punctuated::Punctuated::new();
        punctuated.push(decode_lifetime.clone().into());
        punctuated.push(syn::TypeParamBound::Trait(TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::Path {
                leading_colon: Some(syn::token::PathSep::default()),
                segments: [
                    PathSegment {
                        ident: syn::Ident::new("rustler", Span::call_site()),
                        arguments: syn::PathArguments::None,
                    },
                    PathSegment {
                        ident: syn::Ident::new("Decoder", Span::call_site()),
                        arguments: syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: Default::default(),
                                args: std::iter::once(GenericArgument::Lifetime(
                                    decode_lifetime.clone(),
                                ))
                                .collect(),
                                gt_token: Default::default(),
                            },
                        ),
                    },
                ]
                .iter()
                .cloned()
                .collect(),
            },
        }));
        let predicate = syn::PredicateType {
            lifetimes: None,
            bounded_ty: syn::Type::Path(syn::TypePath {
                qself: None,
                path: type_parameter.clone().ident.into(),
            }),
            colon_token: syn::token::Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause.predicates.push(predicate.into());
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
    let mut generics = ctx.generics.clone();

    let where_clause = generics.make_where_clause();

    for type_parameter in ctx.type_parameters.iter() {
        let mut punctuated = syn::punctuated::Punctuated::new();
        punctuated.push(syn::TypeParamBound::Trait(TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::Path {
                leading_colon: Some(syn::token::PathSep::default()),
                segments: [
                    PathSegment {
                        ident: syn::Ident::new("rustler", Span::call_site()),
                        arguments: syn::PathArguments::None,
                    },
                    PathSegment {
                        ident: syn::Ident::new("Encoder", Span::call_site()),
                        arguments: syn::PathArguments::None,
                    },
                ]
                .iter()
                .cloned()
                .collect(),
            },
        }));
        let predicate = syn::PredicateType {
            lifetimes: None,
            bounded_ty: syn::Type::Path(syn::TypePath {
                qself: None,
                path: type_parameter.ident.clone().into(),
            }),
            colon_token: syn::token::Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause.predicates.push(predicate.into());
    }
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
