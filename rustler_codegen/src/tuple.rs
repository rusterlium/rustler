use ::syntax::ptr::P;
use ::syntax::ast::{MetaItem, ItemKind, Ident, StructField, VariantData, Expr};
use ::syntax::codemap::{Span};
use ::syntax::ext::base::{Annotatable, ExtCtxt};

pub fn transcoder_decorator(
    cx: &mut ExtCtxt,
    span: Span,
    _meta_item: &MetaItem,
    annotatable: &Annotatable,
    push: &mut FnMut(Annotatable)
) {
    match annotatable {
        &Annotatable::Item(ref item) => match &item.node {
            &ItemKind::Struct(VariantData::Struct(ref fields, _), ref generics) => {
                if generics.lifetimes.len() > 1 {
                    cx.span_err(span, "struct can only have one lifetime argument");
                    return;
                }
                let has_lifetime = generics.lifetimes.len() == 1;

                push(gen_decoder(cx, &item.ident, &fields, false, has_lifetime));
                push(gen_encoder(cx, &item.ident, &fields, false, has_lifetime));
            },
            _ => cx.span_err(span, "must decorate a struct"),
        },
        _ => cx.span_err(span, "must decorate a struct"),
    }
}

pub fn gen_decoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, is_tuple: bool, has_lifetime: bool) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    // Make a decoder for each of the fields in the struct.
    let field_decoders: Vec<P<Expr>> = fields.iter().enumerate().map(|(idx, _field)| {
        quote_expr!(cx, try!(rustler::NifDecoder::decode(terms[$idx])))
    }).collect();

    // Generate the struct constructor for the correct struct type (either tuple or named)
    let struct_def_ast = if is_tuple {
        unimplemented!();
    } else {
        let fields_def: Vec<(Ident, P<Expr>)> = field_decoders.iter().zip(fields).map(|(decoder, field)| {
            (field.ident.unwrap().clone(), decoder.clone())
        }).collect();
        builder.expr().struct_path(struct_name.clone()).with_id_exprs(fields_def).build()
    };

    // If the struct has a lifetime argument, put that in the struct type.
    let struct_typ = if has_lifetime { 
        quote_ty!(cx, $struct_name<'a>) 
    } else { 
        quote_ty!(cx, $struct_name) 
    };
    let field_num = field_decoders.len();

    // The implementation itself
    let decoder_ast = quote_item!(cx,
        impl<'a> rustler::NifDecoder<'a> for $struct_typ {
            fn decode(term: rustler::NifTerm<'a>) -> Result<Self, rustler::NifError> {
                let terms = try!(rustler::tuple::get_tuple(term));
                if terms.len() != $field_num {
                    return Err(rustler::NifError::BadArg);
                }
                Ok($struct_def_ast)
            }
        }
    ).unwrap();
    Annotatable::Item(decoder_ast)
}

pub fn gen_encoder(cx: &ExtCtxt, struct_name: &Ident, fields: &Vec<StructField>, is_tuple: bool, has_lifetime: bool) -> Annotatable {
    let builder = ::aster::AstBuilder::new();

    // Make a field encoder expression for each of the items in the struct.
    let field_encoders: Vec<P<Expr>> = fields.iter().map(|field| {
        let field_source = if is_tuple {
            unimplemented!();
        } else {
            let field_ident = field.ident.unwrap().clone();
            quote_expr!(cx, self.$field_ident)
        };
        quote_expr!(cx, $field_source.encode(env))
    }).collect();

    // Build a slice ast from the field_encoders
    let arr_expr = builder.expr().slice().with_exprs(field_encoders).build();

    // If the struct has a lifetime argument, put that in the struct type.
    let struct_typ = if has_lifetime { 
        quote_ty!(cx, $struct_name<'b>) 
    } else { 
        quote_ty!(cx, $struct_name) 
    };

    // The implementation itself
    let encoder_ast = quote_item!(cx,
        impl<'b> rustler::NifEncoder for $struct_typ {
            fn encode<'a>(&self, env: &'a rustler::NifEnv) -> rustler::NifTerm<'a> {
                use rustler::NifEncoder;
                let arr = $arr_expr;
                rustler::tuple::make_tuple(env, &arr)
            }
        }
    ).unwrap();
    Annotatable::Item(encoder_ast)
}
