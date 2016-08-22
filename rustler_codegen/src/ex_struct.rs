use ::syntax::ptr::P;
use ::syntax::ast::{MetaItem, ItemKind, Ident, StructField, VariantData, Expr, Stmt};
use ::syntax::codemap::{Span};
use ::syntax::ext::base::{Annotatable, ExtCtxt};

use ::util::{get_meta_item_value};

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
            &ItemKind::Struct(VariantData::Struct(ref fields, _), ref generics) => {
                if generics.lifetimes.len() > 1 {
                    cx.span_err(span, "struct can only have one lifetime argument");
                    return;
                }
                let has_lifetime = generics.lifetimes.len() == 1;

                push(gen_decoder(cx, &item.ident, &fields, &ex_module_name, has_lifetime));
                push(gen_encoder(cx, &item.ident, &fields, &ex_module_name, has_lifetime));
            },
            _ => cx.span_err(span, "must decorate a normal struct (not unit, not tuple)"),
        },
        _ => cx.span_err(span, "must decorate a struct"),
    }
}

fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, ex_module_name: &str, has_lifetime: bool) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    let field_defs: Vec<(Ident, P<Expr>)> = fields.iter().map(|field| {
        let field_ident = builder.id(field.ident.unwrap());
        let field_ident_str_lit = builder.lit().str(field.ident.unwrap());
        let field_encoder = quote_expr!(cx, 
            match rustler::NifDecoder::decode(
                match rustler::map::get_map_value(term, rustler::atom::get_atom_init($field_ident_str_lit).to_term(env)) {
                    Some(term) => term,
                    None => return Err(rustler::NifError::BadArg),
                    }) {
                Ok(res) => res,
                Err(err) => return Err(err),
            }
        );
        (field_ident, field_encoder)
    }).collect();
    let struct_def_ast = builder.expr().struct_path(struct_name.clone()).with_id_exprs(field_defs).build();

    let struct_typ = if has_lifetime { quote_ty!(cx, $struct_name<'a>) } else { quote_ty!(cx, $struct_name) };

    let ex_module_name_str = builder.lit().str(ex_module_name);
    let decoder_ast = quote_item!(cx, 
        impl<'a> rustler::NifDecoder<'a> for $struct_typ {
            fn decode(term: rustler::NifTerm<'a>) -> Result<Self, rustler::NifError> {
                let env = term.get_env();
                match rustler::ex_struct::get_ex_struct_name(term) {
                    Some(atom) => {
                        if atom != rustler::atom::get_atom_init($ex_module_name_str) {
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

fn gen_encoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, ex_module_name: &str, has_lifetime: bool) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    let field_defs: Vec<Stmt> = fields.iter().map(|field| {
        let field_ident = builder.id(field.ident.unwrap());
        let field_str_lit = builder.lit().str(field.ident.unwrap());
        quote_stmt!(cx, map = rustler::map::map_put(map, rustler::atom::get_atom_init($field_str_lit).to_term(env), 
                                                    self.$field_ident.encode(env)).unwrap();).unwrap()
    }).collect();
    let field_defs_block = builder.block().with_stmts(field_defs).build();

    let struct_typ = if has_lifetime { quote_ty!(cx, $struct_name<'b>) } else { quote_ty!(cx, $struct_name) };
    let ex_module_name_str_lit = builder.lit().str(ex_module_name);

    let encoder_ast = quote_item!(cx,
        impl<'b> rustler::NifEncoder for $struct_typ {
            fn encode<'a>(&self, env: &'a rustler::NifEnv) -> rustler::NifTerm<'a> {
                let mut map = rustler::ex_struct::make_ex_struct(env, $ex_module_name_str_lit).expect("issue #1 on github");

                $field_defs_block

                map
            }
        }
    ).unwrap();
    Annotatable::Item(encoder_ast)
}
