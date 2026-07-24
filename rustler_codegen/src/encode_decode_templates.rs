use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    punctuated::Punctuated,
    token::{Colon, PathSep},
    AngleBracketedGenericArguments, GenericArgument, GenericParam, Ident, Lifetime, LifetimeParam,
    Path, PathArguments, PathSegment, PredicateLifetime, PredicateType, TraitBound, Type,
    TypeParamBound, TypePath, WherePredicate,
};

use super::context::Context;

pub(crate) fn decoder(ctx: &Context, inner: TokenStream) -> TokenStream {
    let ident = ctx.ident;
    let generics = ctx.generics;
    let (_impl_generics, ty_generics, _where_clause) = generics.split_for_impl();

    // The Decoder uses a special lifetime '__rustler_decode_lifetime. We need to ensure that all
    // other lifetimes are bound to this lifetime: As we decode from a term (which has a lifetime),
    // references to that term may not outlive the term itself.
    let mut impl_generics = generics.clone();
    let decode_lifetime = Lifetime::new("'__rustler_decode_lifetime", Span::call_site());
    let lifetime_def = LifetimeParam::new(decode_lifetime.clone());
    impl_generics
        .params
        .push(GenericParam::Lifetime(lifetime_def));

    let where_clause = impl_generics.make_where_clause();

    for lifetime in ctx.lifetimes.iter() {
        let mut punctuated = Punctuated::new();
        punctuated.push(lifetime.clone());
        let predicate = PredicateLifetime {
            lifetime: decode_lifetime.clone(),
            colon_token: Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
            attrs: vec![],
        };
        where_clause
            .predicates
            .push(WherePredicate::Lifetime(predicate));

        let mut punctuated = Punctuated::new();
        punctuated.push(decode_lifetime.clone());
        let predicate = PredicateLifetime {
            lifetime: lifetime.clone(),
            colon_token: Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
            attrs: vec![],
        };
        where_clause
            .predicates
            .push(WherePredicate::Lifetime(predicate));
    }

    for type_parameter in ctx.type_parameters.iter() {
        let mut punctuated = Punctuated::new();
        punctuated.push(TypeParamBound::Lifetime(decode_lifetime.clone()));
        punctuated.push(TypeParamBound::Trait(TraitBound {
            paren_token: None,
            maybe: None,
            modifiers: Default::default(),
            lifetimes: None,
            path: Path {
                leading_colon: Some(PathSep::default()),
                segments: [
                    PathSegment {
                        ident: Ident::new("rustler", Span::call_site()),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: Ident::new("Decoder", Span::call_site()),
                        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            colon2_token: None,
                            lt_token: Default::default(),
                            args: std::iter::once(GenericArgument::Lifetime(
                                decode_lifetime.clone(),
                            ))
                            .collect(),
                            gt_token: Default::default(),
                        }),
                    },
                ]
                .iter()
                .cloned()
                .collect(),
            },
        }));
        let predicate = PredicateType {
            attrs: Default::default(),
            lifetimes: None,
            bounded_ty: Type::Path(TypePath {
                attrs: vec![],
                qself: None,
                path: type_parameter.clone().ident.into(),
            }),
            colon_token: Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause
            .predicates
            .push(WherePredicate::Type(predicate));
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
        let mut punctuated = Punctuated::new();
        punctuated.push(TypeParamBound::Trait(TraitBound {
            maybe: None,
            paren_token: None,
            modifiers: Default::default(),
            lifetimes: None,
            path: Path {
                leading_colon: Some(PathSep::default()),
                segments: [
                    PathSegment {
                        ident: Ident::new("rustler", Span::call_site()),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: Ident::new("Encoder", Span::call_site()),
                        arguments: PathArguments::None,
                    },
                ]
                .iter()
                .cloned()
                .collect(),
            },
        }));
        let predicate = PredicateType {
            attrs: Default::default(),
            lifetimes: None,
            bounded_ty: Type::Path(TypePath {
                attrs: Default::default(),
                qself: None,
                path: type_parameter.ident.clone().into(),
            }),
            colon_token: Colon {
                spans: [Span::call_site()],
            },
            bounds: punctuated,
        };
        where_clause
            .predicates
            .push(WherePredicate::Type(predicate));
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics ::rustler::Encoder for #ident #ty_generics #where_clause {
            #[allow(clippy::needless_borrow)]
            fn encode<'__rustler__encode_lifetime>(&self, env: ::rustler::Env<'__rustler__encode_lifetime>) -> ::rustler::Term<'__rustler__encode_lifetime> {
                #inner.into()
            }
        }
    }
}
