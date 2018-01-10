#![feature(prelude_import)]
#![no_std]
#![crate_type = "dylib"]
#![feature(plugin, plugin_registrar, quote, rustc_private)]
#![plugin(easy_plugin)]
#[prelude_import]
use std::prelude::v1::*;
#[macro_use]
extern crate std as std;

#[allow(plugin_as_library)]
extern crate easy_plugin;
extern crate aster;

//extern crate rustc;

#[cfg(feature = "stable")]
extern crate syntex;
#[cfg(feature = "stable")]
extern crate syntex_syntax as syntax;
#[cfg(feature = "stable")]
use std::path::Path;

mod ex_struct {
    use ::syntax::ptr::P;
    use ::syntax::ast::{MetaItem, ItemKind, Ident, StructField, VariantData, Expr, Stmt};
    use ::syntax::codemap::{Span};
    use ::syntax::ext::base::{Annotatable, ExtCtxt};
    use ::util::{get_meta_item_value};
    pub fn transcoder_decorator(cx: &mut ExtCtxt, span: Span,
                                meta_item: &MetaItem,
                                annotatable: &Annotatable,
                                push: &mut FnMut(Annotatable)) {
        let ex_module_name =
            match get_meta_item_value(meta_item, "module") {
                Some(value) => value,
                None => {
                    cx.span_err(span, "attribute must have module parameter");
                    return;
                }
            };
        match annotatable {
            &Annotatable::Item(ref item) =>
            match &item.node {
                &ItemKind::Struct(VariantData::Struct(ref fields, _),
                                  ref generics) => {
                    if generics.lifetimes.len() > 1 {
                        cx.span_err(span,
                                    "struct can only have one lifetime argument");
                        return;
                    }
                    let has_lifetime = generics.lifetimes.len() == 1;
                    push(gen_decoder(cx, &item.ident, &fields,
                                     &ex_module_name, has_lifetime));
                    push(gen_encoder(cx, &item.ident, &fields,
                                     &ex_module_name, has_lifetime));
                }
                _ =>
                cx.span_err(span,
                            "must decorate a normal struct (not unit, not tuple)"),
            },
            _ => cx.span_err(span, "must decorate a struct"),
        }
    }
    fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident,
                   fields: &Vec<StructField>, ex_module_name: &str,
                   has_lifetime: bool) -> Annotatable {
        let builder = ::aster::AstBuilder::new();
        let field_defs: Vec<(Ident, P<Expr>)> =
            fields.iter().map(|field| {
                              let field_ident =
                                  builder.id(field.ident.unwrap());
                              let field_ident_str = field_ident.name.as_str();
                              let field_encoder =
                                  {
                                      use syntax::ext::quote::rt::*;
                                      let ext_cx = &*cx;
                                      ::syntax::ext::quote::parse_expr_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                      ext_cx.cfg(),
                                                                                                      {
                                                                                                          let _sp =
                                                                                                              ext_cx.call_site();
                                                                                                          let mut tt =
                                                                                                              ::std::vec::Vec::new();
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("match"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Decoder"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("decode"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("match"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("get_map_value"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Comma));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("atom"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("get_atom_init"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.extend(field_ident_str.to_tokens(ext_cx).into_iter());
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Dot));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("to_term"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Some"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::FatArrow));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Comma));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("None"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::FatArrow));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::ModSep));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("BadArg"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Comma));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Ok"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("res"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::FatArrow));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("res"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Comma));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("err"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::FatArrow));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Ident(ext_cx.ident_of("err"))));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::Comma));
                                                                                                          tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                          ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                          tt
                                                                                                      }))
                                  }; (field_ident, field_encoder)
                          }).collect();
        let struct_def_ast =
            builder.expr().struct_path(struct_name.clone()).with_id_exprs(field_defs).build();
        let struct_typ =
            if has_lifetime {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lt));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Gt));
                                                                                      tt
                                                                                  }))
                }
            } else {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt
                                                                                  }))
                }
            };
        let decoder_ast =
            {
                use syntax::ext::quote::rt::*;
                let ext_cx = &*cx;
                ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                ext_cx.cfg(),
                                                                                {
                                                                                    let _sp =
                                                                                        ext_cx.call_site();
                                                                                    let mut tt =
                                                                                        ::std::vec::Vec::new();
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("impl"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Decoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("for"))));
                                                                                    tt.extend(struct_typ.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("decode"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Colon));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::RArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Result"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Self"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("let"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Eq));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Dot));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("get_env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("match"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("ex_struct"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("get_ex_struct_name"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Some"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("atom"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::FatArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("if"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("atom"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ne));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("atom"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("get_atom_init"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.extend(ex_module_name.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("BadArg"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("None"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::FatArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("BadArg"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Ok"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.extend(struct_def_ast.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt
                                                                                }))
            }.unwrap();
        Annotatable::Item(decoder_ast)
    }
    fn gen_encoder(cx: &ExtCtxt, struct_name: &Ident,
                   fields: &Vec<StructField>, ex_module_name: &str,
                   has_lifetime: bool) -> Annotatable {
        let builder = ::aster::AstBuilder::new();
        let field_defs: Vec<Stmt> =
            fields.iter().map(|field| {
                              let field_ident =
                                  builder.id(field.ident.unwrap());
                              let field_ident_str = field_ident.name.as_str();
                              {
                                  use syntax::ext::quote::rt::*;
                                  let ext_cx = &*cx;
                                  ::syntax::ext::quote::parse_stmt_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                  ext_cx.cfg(),
                                                                                                  {
                                                                                                      let _sp =
                                                                                                          ext_cx.call_site();
                                                                                                      let mut tt =
                                                                                                          ::std::vec::Vec::new();
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Eq));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::ModSep));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::ModSep));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("map_put"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Comma));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::ModSep));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("atom"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::ModSep));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("get_atom_init"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.extend(field_ident_str.to_tokens(ext_cx).into_iter());
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Dot));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("to_term"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Comma));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("self"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Dot));
                                                                                                      tt.extend(field_ident.to_tokens(ext_cx).into_iter());
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Dot));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("encode"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Dot));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("unwrap"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Semi));
                                                                                                      tt
                                                                                                  }))
                              }.unwrap() }).collect();
        let struct_typ =
            if has_lifetime {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lt));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'b"))));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Gt));
                                                                                      tt
                                                                                  }))
                }
            } else {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt
                                                                                  }))
                }
            };
        let encoder_ast =
            {
                use syntax::ext::quote::rt::*;
                let ext_cx = &*cx;
                ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                ext_cx.cfg(),
                                                                                {
                                                                                    let _sp =
                                                                                        ext_cx.call_site();
                                                                                    let mut tt =
                                                                                        ::std::vec::Vec::new();
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("impl"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'b"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Encoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("for"))));
                                                                                    tt.extend(struct_typ.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("encode"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("self"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Colon));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifEnv"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::RArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("use"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Encoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("let"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("mut"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Eq));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("ex_struct"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("make_ex_struct"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.extend(ex_module_name.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Dot));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("expect"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Literal(::syntax::parse::token::Str_(ext_cx.name_of("issue #1 on github")),
                                                                                                                                                                    ::std::option::Option::None)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.extend(field_defs.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("map"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt
                                                                                }))
            }.unwrap();
        Annotatable::Item(encoder_ast)
    }
}
mod resource {
    use ::syntax::ext::base::{Annotatable, ExtCtxt, MacResult, MacEager};
    use ::syntax::ast::{MetaItem};
    use ::syntax::codemap::{Span};
    use easy_plugin::{PluginResult};
    use ::syntax::util::small_vector::{SmallVector};
    pub fn resource_struct_def_decorator(cx: &mut ExtCtxt, span: Span,
                                         _meta_item: &MetaItem,
                                         annotatable: &Annotatable,
                                         push: &mut FnMut(Annotatable)) {
        let builder = ::aster::AstBuilder::new().span(span);
        let struct_ident = annotatable.clone().expect_item().ident;
        let struct_ident_str = &*struct_ident.name.as_str();
        let type_field_name =
            builder.id(["_rustler_nif_struct_type_",
                        struct_ident_str].concat());
        let dtor_name_ident =
            builder.id(["_rustler_nif_struct_type_dtor_",
                        struct_ident_str].concat());
        push(Annotatable::Item({
                                   use syntax::ext::quote::rt::*;
                                   let ext_cx = &*cx;
                                   ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                   ext_cx.cfg(),
                                                                                                   {
                                                                                                       let _sp =
                                                                                                           ext_cx.call_site();
                                                                                                       let mut tt =
                                                                                                           ::std::vec::Vec::new();
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Pound));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Bracket)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("allow"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("non_snake_case"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Bracket)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("static"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("mut"))));
                                                                                                       tt.extend(type_field_name.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("Option"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NifResourceType"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.extend(struct_ident.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::BinOp(::syntax::parse::token::Shr)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Eq));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("None"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Semi));
                                                                                                       tt
                                                                                                   }))
                               }.unwrap()));
        push(Annotatable::Item({
                                   use syntax::ext::quote::rt::*;
                                   let ext_cx = &*cx;
                                   ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                   ext_cx.cfg(),
                                                                                                   {
                                                                                                       let _sp =
                                                                                                           ext_cx.call_site();
                                                                                                       let mut tt =
                                                                                                           ::std::vec::Vec::new();
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Pound));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Bracket)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("allow"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("non_snake_case"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Bracket)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("extern"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Literal(::syntax::parse::token::Str_(ext_cx.name_of("C")),
                                                                                                                                                                                       ::std::option::Option::None)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                                       tt.extend(dtor_name_ident.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("wrapper"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("nif_interface"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NIF_ENV"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Comma));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("obj"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("wrapper"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("nif_interface"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("MUTABLE_NIF_RESOURCE_HANDLE"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("unsafe"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("codegen_runtime"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("handle_drop_resource_struct_handle"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.extend(struct_ident.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Gt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Comma));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("obj"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt
                                                                                                   }))
                               }.unwrap()));
        push(Annotatable::Item({
                                   use syntax::ext::quote::rt::*;
                                   let ext_cx = &*cx;
                                   ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                   ext_cx.cfg(),
                                                                                                   {
                                                                                                       let _sp =
                                                                                                           ext_cx.call_site();
                                                                                                       let mut tt =
                                                                                                           ::std::vec::Vec::new();
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("impl"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NifResourceTypeProvider"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("for"))));
                                                                                                       tt.extend(struct_ident.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("get_dtor"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::RArrow));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("extern"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Literal(::syntax::parse::token::Str_(ext_cx.name_of("C")),
                                                                                                                                                                                       ::std::option::Option::None)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("_env"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("wrapper"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("nif_interface"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NIF_ENV"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Comma));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("handle"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("wrapper"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("nif_interface"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("MUTABLE_NIF_RESOURCE_HANDLE"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.extend(dtor_name_ident.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("get_type"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Gt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::RArrow));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NifResourceType"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("Self"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Gt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("unsafe"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                                       tt.extend(type_field_name.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Dot));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("as_ref"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Dot));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("unwrap"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("unsafe"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("set_type"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("typ"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Colon));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::ModSep));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("NifResourceType"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Lt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("Self"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Gt));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.extend(type_field_name.to_tokens(ext_cx).into_iter());
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Eq));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("Some"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::Ident(ext_cx.ident_of("typ"))));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                       ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                                       tt
                                                                                                   }))
                               }.unwrap()));
    }
    pub fn resource_struct_init_macro<'cx>(context:
                                               &'cx mut ::syntax::ext::base::ExtCtxt,
                                           span: ::syntax::codemap::Span,
                                           arguments:
                                               &[::syntax::tokenstream::TokenTree])
     -> Box<::syntax::ext::base::MacResult+ 'cx> {
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::std::clone::Clone for Arguments {
            #[inline]
            fn clone(&self) -> Arguments {
                match *self {
                    Arguments {
                    struct_ident: ref __self_0_0, env: ref __self_0_1 } =>
                    Arguments{struct_ident:
                                  ::std::clone::Clone::clone(&(*__self_0_0)),
                              env:
                                  ::std::clone::Clone::clone(&(*__self_0_1)),},
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::std::fmt::Debug for Arguments {
            fn fmt(&self, __arg_0: &mut ::std::fmt::Formatter)
             -> ::std::fmt::Result {
                match *self {
                    Arguments {
                    struct_ident: ref __self_0_0, env: ref __self_0_1 } => {
                        let mut builder = __arg_0.debug_struct("Arguments");
                        let _ =
                            builder.field("struct_ident", &&(*__self_0_0));
                        let _ = builder.field("env", &&(*__self_0_1));
                        builder.finish()
                    }
                }
            }
        }
        struct Arguments {
            pub struct_ident: ::syntax::codemap::Spanned<::syntax::ast::Ident>,
            pub env: ::syntax::codemap::Spanned<::syntax::ast::Ident>,
        }
        #[allow(non_snake_case)]
        fn parse(session: &::syntax::parse::ParseSess,
                 arguments: &[::syntax::tokenstream::TokenTree])
         -> ::easy_plugin::PluginResult<Arguments> {
            let specification =
                ::easy_plugin::Specification([::easy_plugin::Specifier::Ident("struct_ident".to_string()),
                                              ::easy_plugin::Specifier::Specific(::syntax::parse::token::Token::Comma),
                                              ::easy_plugin::Specifier::Ident("env".to_string())].to_vec());
            ::easy_plugin::parse_args(session, arguments,
                                      &specification.0).map(|_m| {
                                                            Arguments{struct_ident:
                                                                          _m.get("struct_ident").unwrap().into(),
                                                                      env:
                                                                          _m.get("env").unwrap().into(),}
                                                        })
        }
        fn resource_struct_init_macro_<'cx>(cx: &'cx mut ExtCtxt, _span: Span,
                                            arguments: Arguments)
         -> PluginResult<Box<MacResult+ 'cx>> {
            let env_ident = arguments.env;
            let struct_ident = arguments.struct_ident;
            let struct_ident_str = &*struct_ident.node.name.as_str();
            let init_item =
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_stmt_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                    ext_cx.cfg(),
                                                                                    {
                                                                                        let _sp =
                                                                                            ext_cx.call_site();
                                                                                        let mut tt =
                                                                                            ::std::vec::Vec::new();
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("let"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("res"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Eq));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("match"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("open_struct_resource_type"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Lt));
                                                                                        tt.extend(struct_ident.to_tokens(ext_cx).into_iter());
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Gt));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                        tt.extend(env_ident.to_tokens(ext_cx).into_iter());
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Comma));
                                                                                        tt.extend(struct_ident_str.to_tokens(ext_cx).into_iter());
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Comma));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("wrapper"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("nif_interface"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("NIF_RESOURCE_FLAGS"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("ERL_NIF_RT_CREATE"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("Some"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("inner"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::FatArrow));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("inner"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Comma));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("None"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::FatArrow));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("println"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Not));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Literal(::syntax::parse::token::Str_(ext_cx.name_of("Failiure in creating resource type")),
                                                                                                                                                                        ::std::option::Option::None)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("false"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("unsafe"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("use"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("resource"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("NifResourceTypeProvider"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.extend(struct_ident.to_tokens(ext_cx).into_iter());
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::ModSep));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("set_type"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Ident(ext_cx.ident_of("res"))));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::Semi));
                                                                                        tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                        ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                        tt
                                                                                    }))
                }.unwrap();
            Ok(MacEager::stmts(SmallVector::one(init_item)))
        }
        match parse(context.parse_sess,
                    arguments).and_then(|a|
                                            resource_struct_init_macro_(context,
                                                                        span,
                                                                        a)) {
            Ok(result) => result,
            Err((subspan, message)) => {
                let span =
                    if subspan == ::syntax::codemap::DUMMY_SP {
                        span
                    } else { subspan };
                context.span_err(span, &message);
                ::syntax::ext::base::DummyResult::any(span)
            }
        }
    }
}
mod util {
    use ::syntax::ast::{MetaItem, MetaItemKind};
    use ::syntax::codemap::{Spanned};
    use syntax::attr::AttrMetaMethods;
    pub fn get_meta_item_value(meta_item: &MetaItem, name: &str)
     -> Option<String> {
        match meta_item {
            &Spanned { node: MetaItemKind::List(_, ref items), span: _ } => {
                for ref item in items {
                    if item.name() == name {
                        match item.value_str() {
                            Some(value) => return Some(value.to_string()),
                            None => (),
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }
}
mod tuple {
    use ::syntax::ptr::P;
    use ::syntax::ast::{MetaItem, ItemKind, Ident, StructField, VariantData,
                        Expr};
    use ::syntax::codemap::{Span};
    use ::syntax::ext::base::{Annotatable, ExtCtxt};
    pub fn transcoder_decorator(cx: &mut ExtCtxt, span: Span,
                                _meta_item: &MetaItem,
                                annotatable: &Annotatable,
                                push: &mut FnMut(Annotatable)) {
        match annotatable {
            &Annotatable::Item(ref item) =>
            match &item.node {
                &ItemKind::Struct(VariantData::Struct(ref fields, _),
                                  ref generics) => {
                    if generics.lifetimes.len() > 1 {
                        cx.span_err(span,
                                    "struct can only have one lifetime argument");
                        return;
                    }
                    let has_lifetime = generics.lifetimes.len() == 1;
                    push(gen_decoder(cx, &item.ident, &fields, false,
                                     has_lifetime));
                    push(gen_encoder(cx, &item.ident, &fields, false,
                                     has_lifetime));
                }
                _ => cx.span_err(span, "must decorate a struct"),
            },
            _ => cx.span_err(span, "must decorate a struct"),
        }
    }
    pub fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident,
                       fields: &Vec<StructField>, is_tuple: bool,
                       has_lifetime: bool) -> Annotatable {
        let builder = ::aster::AstBuilder::new();
        let field_decoders: Vec<P<Expr>> =
            fields.iter().enumerate().map(|(idx, _field)| {
                                          {
                                              use syntax::ext::quote::rt::*;
                                              let ext_cx = &*cx;
                                              ::syntax::ext::quote::parse_expr_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                              ext_cx.cfg(),
                                                                                                              {
                                                                                                                  let _sp =
                                                                                                                      ext_cx.call_site();
                                                                                                                  let mut tt =
                                                                                                                      ::std::vec::Vec::new();
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Ident(ext_cx.ident_of("try"))));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Not));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::ModSep));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Ident(ext_cx.ident_of("Decoder"))));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::ModSep));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Ident(ext_cx.ident_of("decode"))));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::Ident(ext_cx.ident_of("terms"))));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::OpenDelim(::syntax::parse::token::Bracket)));
                                                                                                                  tt.extend(idx.to_tokens(ext_cx).into_iter());
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::CloseDelim(::syntax::parse::token::Bracket)));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                                  tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                                  ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                                  tt
                                                                                                              }))
                                          } }).collect();
        let struct_def_ast =
            if is_tuple {
                {
                    ::std::rt::begin_panic("not yet implemented",
                                           {
                                               static _FILE_LINE:
                                                      (&'static str, u32) =
                                                   ("/home/hansihe/git/Rustler/rustler_codegen/target/debug/build/rustler_codegen-8a765b9abcb46d14/out/lib.rs",
                                                    309u32);
                                               &_FILE_LINE
                                           })
                };
            } else {
                let fields_def: Vec<(Ident, P<Expr>)> =
                    field_decoders.iter().zip(fields).map(|(decoder, field)| {
                                                          (field.ident.unwrap().clone(),
                                                           decoder.clone())
                                                      }).collect();
                builder.expr().struct_path(struct_name.clone()).with_id_exprs(fields_def).build()
            };
        let struct_typ =
            if has_lifetime {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lt));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Gt));
                                                                                      tt
                                                                                  }))
                }
            } else {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt
                                                                                  }))
                }
            };
        let field_num = field_decoders.len();
        let decoder_ast =
            {
                use syntax::ext::quote::rt::*;
                let ext_cx = &*cx;
                ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                ext_cx.cfg(),
                                                                                {
                                                                                    let _sp =
                                                                                        ext_cx.call_site();
                                                                                    let mut tt =
                                                                                        ::std::vec::Vec::new();
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("impl"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Decoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("for"))));
                                                                                    tt.extend(struct_typ.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("decode"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Colon));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::RArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Result"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Self"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("let"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("terms"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Eq));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("try"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Not));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("tuple"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("get_tuple"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("if"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("terms"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Dot));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("len"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ne));
                                                                                    tt.extend(field_num.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("return"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Err"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifError"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("BadArg"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Ok"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.extend(struct_def_ast.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt
                                                                                }))
            }.unwrap();
        Annotatable::Item(decoder_ast)
    }
    pub fn gen_encoder(cx: &ExtCtxt, struct_name: &Ident,
                       fields: &Vec<StructField>, is_tuple: bool,
                       has_lifetime: bool) -> Annotatable {
        let builder = ::aster::AstBuilder::new();
        let field_encoders: Vec<P<Expr>> =
            fields.iter().map(|field| {
                              let field_source =
                                  if is_tuple {
                                      {
                                          ::std::rt::begin_panic("not yet implemented",
                                                                 {
                                                                     static _FILE_LINE:
                                                                            (&'static str,
                                                                             u32)
                                                                            =
                                                                         ("/home/hansihe/git/Rustler/rustler_codegen/target/debug/build/rustler_codegen-8a765b9abcb46d14/out/lib.rs",
                                                                          343u32);
                                                                     &_FILE_LINE
                                                                 })
                                      };
                                  } else {
                                      let field_ident =
                                          field.ident.unwrap().clone();
                                      {
                                          use syntax::ext::quote::rt::*;
                                          let ext_cx = &*cx;
                                          ::syntax::ext::quote::parse_expr_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                          ext_cx.cfg(),
                                                                                                          {
                                                                                                              let _sp =
                                                                                                                  ext_cx.call_site();
                                                                                                              let mut tt =
                                                                                                                  ::std::vec::Vec::new();
                                                                                                              tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                              ::syntax::parse::token::Ident(ext_cx.ident_of("self"))));
                                                                                                              tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                              ::syntax::parse::token::Dot));
                                                                                                              tt.extend(field_ident.to_tokens(ext_cx).into_iter());
                                                                                                              tt
                                                                                                          }))
                                      }
                                  };
                              {
                                  use syntax::ext::quote::rt::*;
                                  let ext_cx = &*cx;
                                  ::syntax::ext::quote::parse_expr_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                                  ext_cx.cfg(),
                                                                                                  {
                                                                                                      let _sp =
                                                                                                          ext_cx.call_site();
                                                                                                      let mut tt =
                                                                                                          ::std::vec::Vec::new();
                                                                                                      tt.extend(field_source.to_tokens(ext_cx).into_iter());
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Dot));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("encode"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                                      ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                                      tt
                                                                                                  }))
                              } }).collect();
        let arr_expr =
            builder.expr().slice().with_exprs(field_encoders).build();
        let struct_typ =
            if has_lifetime {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lt));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'b"))));
                                                                                      tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                      ::syntax::parse::token::Gt));
                                                                                      tt
                                                                                  }))
                }
            } else {
                {
                    use syntax::ext::quote::rt::*;
                    let ext_cx = &*cx;
                    ::syntax::ext::quote::parse_ty_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                  ext_cx.cfg(),
                                                                                  {
                                                                                      let _sp =
                                                                                          ext_cx.call_site();
                                                                                      let mut tt =
                                                                                          ::std::vec::Vec::new();
                                                                                      tt.extend(struct_name.to_tokens(ext_cx).into_iter());
                                                                                      tt
                                                                                  }))
                }
            };
        let encoder_ast =
            {
                use syntax::ext::quote::rt::*;
                let ext_cx = &*cx;
                ::syntax::ext::quote::parse_item_panic(&mut new_parser_from_tts(ext_cx.parse_sess(),
                                                                                ext_cx.cfg(),
                                                                                {
                                                                                    let _sp =
                                                                                        ext_cx.call_site();
                                                                                    let mut tt =
                                                                                        ::std::vec::Vec::new();
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("impl"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'b"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Encoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("for"))));
                                                                                    tt.extend(struct_typ.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("fn"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("encode"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("self"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Colon));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("NifEnv"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::RArrow));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Term"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Lifetime(ext_cx.ident_of("\'a"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Gt));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("use"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("Encoder"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("let"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("arr"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Eq));
                                                                                    tt.extend(arr_expr.to_tokens(ext_cx).into_iter());
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Semi));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("rustler"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("tuple"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::ModSep));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("make_tuple"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::OpenDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("env"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Comma));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::BinOp(::syntax::parse::token::And)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::Ident(ext_cx.ident_of("arr"))));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Paren)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt.push(::syntax::tokenstream::TokenTree::Token(_sp,
                                                                                                                                    ::syntax::parse::token::CloseDelim(::syntax::parse::token::Brace)));
                                                                                    tt
                                                                                }))
            }.unwrap();
        Annotatable::Item(encoder_ast)
    }
}
#[cfg(feature = "stable")]
pub fn expand<S, D>(src: S, dst: D) -> Result<(), syntex::Error> where
 S: AsRef<Path>, D: AsRef<Path> {
    let mut reg = syntex::Registry::new();
    reg.add_decorator("ExStruct", ex_struct::transcoder_decorator);
    reg.add_decorator("NifTuple", tuple::transcoder_decorator);
    reg.add_decorator("NifResource", resource::resource_struct_def_decorator);
    reg.add_macro("resource_struct_init",
                  resource::resource_struct_init_macro);
    reg.expand("", src.as_ref(), dst.as_ref())
}
