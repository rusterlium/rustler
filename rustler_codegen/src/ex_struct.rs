use ::syntax::ptr::P;
use ::syntax::ast::{MetaItem, MetaItem_, Item_, Ident, StructField, VariantData, Lit_, Expr};
use ::syntax::codemap::{Span, Spanned};
use ::syntax::ext::base::{Annotatable, ExtCtxt};
use ::syntax::ext::build::AstBuilder;

use syntax::attr::AttrMetaMethods;

fn get_meta_item_value(meta_item: &MetaItem, name: &str) -> Option<String> {
    match meta_item {
        &Spanned { node: MetaItem_::MetaList(_, ref items), span: _ } => {
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
        _ => None
    }
}

pub fn transcoder_decorator(
    cx: &mut ExtCtxt,
    span: Span,
    meta_item: &MetaItem,
    annotatable: &Annotatable,
    push: &mut FnMut(Annotatable)
) {
    let ex_module_name = match get_meta_item_value(meta_item, "module") {
        Some(value) => value,
        None => {
            cx.span_err(span, "attribute must have module parameter");
            return;
        }
    };

    match annotatable {
        &Annotatable::Item(ref item) => match &item.node {
            &Item_::ItemStruct(VariantData::Struct(ref fields, _), ref generics) => {
                if generics.lifetimes.len() > 1 {
                    cx.span_err(span, "struct can only have one lifetime argument");
                    return;
                }
                let has_lifetime = generics.lifetimes.len() == 1;

                if fields.len() > 0 && fields[0].node.kind.is_unnamed() {
                    cx.span_err(span, "a tuple struct can't be converted to a elixir struct");
                    return;
                }

                push(gen_decoder(cx, &item.ident, &fields, &ex_module_name, has_lifetime));
                push(gen_encoder(cx, &item.ident, &fields, &ex_module_name));
            },
            _ => cx.span_err(span, "must decorate a struct"),
        },
        _ => cx.span_err(span, "must decorate a struct"),
    }
}

fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, ex_module_name: &str, has_lifetime: bool) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    let field_defs: Vec<(Ident, P<Expr>)> = fields.iter().map(|field| {
        let field_ident = builder.id(field.node.ident().unwrap());
        let field_ident_str = field_ident.name.as_str();
        let field_encoder = quote_expr!(cx, 
            match rustler::NifDecoder::decode(
                match rustler::map::get_map_value(env, term, rustler::atom::get_atom_init($field_ident_str).to_term(env)) {
                    Some(term) => term,
                    None => return Err(rustler::NifError::BadArg),
                    }, env) {
                Ok(res) => res,
                Err(err) => return Err(err),
            }
        );
        (field_ident, field_encoder)
    }).collect();
    let struct_def_ast = builder.expr().struct_path(struct_name.clone()).with_id_exprs(field_defs).build();

    let decoder_ast = quote_item!(cx, 
        impl rustler::NifDecoder for $struct_name {
            fn decode(term: rustler::NifTerm, env: &rustler::NifEnv) -> Result<Self, rustler::NifError> {
                match rustler::map::get_ex_struct_name(env, term) {
                    Some(atom) => {
                        // Potential equality issue?
                        if atom != rustler::atom::get_atom_init($ex_module_name) {
                            return Err(rustler::NifError::BadArg);
                        }
                    },
                    None => return Err(rustler::NifError::BadArg),
                }
                Ok($struct_def_ast)
            }
        }
    ).unwrap();
    Annotatable::Item(decoder_ast)
}

fn gen_encoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, ex_module_name: &str) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    let encoder_ast = quote_item!(cx,
        impl rustler::NifEncoder for $struct_name {
            fn encode<'a>(self, env: &'a rustler::NifEnv) -> rustler::NifTerm<'a> {
                //let map = rustler::wrapper::map::map_new(env.as_c_arg());

                //rustler::NifTerm::new(env, map)
                unimplemented!();
            }
        }
    ).unwrap();
    Annotatable::Item(encoder_ast)
}
