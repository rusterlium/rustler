use ::syntax::ast::{MetaItem, Item_, Ident, StructField, VariantData};
use ::syntax::codemap::Span;
use ::syntax::ext::base::{Annotatable, ExtCtxt};
use ::syntax::ext::build::AstBuilder;

pub fn transcoder_decorator(
    cx: &mut ExtCtxt,
    span: Span,
    meta_item: &MetaItem,
    annotatable: &Annotatable,
    push: &mut FnMut(Annotatable)
) {
    match annotatable {
        &Annotatable::Item(ref item) => match &item.node {
            &Item_::ItemStruct(VariantData::Struct(ref fields, _), ref generics) => {
                push(gen_decoder(cx, &item.ident, &fields));
            },
            _ => cx.span_err(span, "must decorate a struct"),
        },
        _ => cx.span_err(span, "must decorate a struct"),
    }
}

fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>) -> Annotatable {
    println!("{:?}", fields);
    let decoder_ast = quote_item!(cx, 
        impl rustler::NifDecoder for $struct_name {
            fn decode(term: rustler::NifTerm, env: &rustler::NifEnv) -> Result<Self, rustler::NifError> {
                unimplemented!();
            }
        }
    ).unwrap();
    Annotatable::Item(decoder_ast)
}
