use easy_plugin::{PluginResult};

use ::syntax::codemap::{Span, Spanned};
use ::syntax::ptr::P;
use ::syntax::parse::token::{Token, DelimToken, Lit, IdentStyle};
use ::syntax::ast::{TokenTree, Lit_, Delimited, Ident, Expr, Item};
use ::syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use ::syntax::ext::build::AstBuilder;  // trait for expr_usize
use ::rustc_plugin::Registry;

use ::syntax::util::small_vector::{SmallVector};

use ::std::ascii::AsciiExt;
use ::std::ffi::CString;

fn string_from_spanned_literal(span: &Spanned<Lit_>, ascii_only: bool) -> PluginResult<String> {
    let string = try!(match span {
        &Spanned { node: Lit_::LitStr(ref string, _), span: _ } => Ok(string.to_string()),
        _ => Err((span.span.clone(), "must be string literal".to_string())),
    });
    if ascii_only && !string.is_ascii() {
        return Err((span.span.clone(), "must be ascii only".to_string()));
    }
    Ok(string)
}

fn string_from_token(token: &TokenTree, ascii_only: bool) -> PluginResult<String> {
    let string = try!(match token {
        &TokenTree::Token(_, Token::Literal(Lit::Str_(ref name), _)) => Ok(name.as_str().to_string()),
        _ => Err((token.get_span().clone(), "must be string".to_string())),
    });
    if ascii_only && !string.is_ascii() {
        return Err((token.get_span().clone(), "must be ascii only".to_string()));
    }
    Ok(string)
}
fn num_from_token(token: &TokenTree) -> PluginResult<String> {
    match token {
        &TokenTree::Token(_, Token::Literal(Lit::Integer(ref name), _)) => Ok(name.as_str().to_string()),
        _ => Err((token.get_span().clone(), "must be integer".to_string())),
    }
}
fn ident_from_token(token: &TokenTree) -> PluginResult<String> {
    match token {
        &TokenTree::Token(_, Token::Ident(Ident { ref name, ctxt: _ }, IdentStyle::Plain)) => Ok(name.as_str().to_string()),
        _ => Err((token.get_span().clone(), "must be intent".to_string())),
    }
}

#[derive(Debug)]
struct FunEntry {
    name: String,
    arity: TokenTree,
    ident: String
}

fn parse_delim_fun_entry(entry: &Delimited) -> PluginResult<FunEntry> {
    if entry.delim != DelimToken::Paren {
        return Err((entry.open_span.clone(), "expected tuple".to_string()));
    }
    if entry.tts.len() != 5 {
        return Err((entry.open_span.clone(), "tuple must contain 3 items".to_string()));
    }

    let fun_name = try!(string_from_token(&entry.tts[0], true));
    try!(num_from_token(&entry.tts[2]));
    let rust_fun_ident = try!(ident_from_token(&entry.tts[4]));

    Ok(FunEntry {
        name: fun_name,
        arity: (&entry.tts[2]).clone(),
        ident: rust_fun_ident
    })
}

fn parse_delim_fun_list(list: &Delimited) -> PluginResult<Vec<FunEntry>> {
    if list.delim != DelimToken::Bracket {
        return Err((list.open_span.clone(), "expected bracket".to_string()));
    }

    let mut funs: Vec<FunEntry> = Vec::with_capacity(list.tts.len());
    for entry in list.tts.iter() {
        match entry {
            &TokenTree::Delimited(_, ref delim_entry_rc) => {
                let delim_entry = &delim_entry_rc;
                funs.push(try!(parse_delim_fun_entry(delim_entry)));
            }
            &TokenTree::Token(_, _) => (),
            _ => return Err((entry.get_span().clone(), "expected delimited".to_string())),
        }
    }

    Ok(funs)
}

fn make_nif_int_id(name: &str) -> String {
    ["_rustler_nif_ext_fun_", name].concat()
}

easy_plugin! {
    struct Arguments { $mod_name:lit, $functions:delim, $load_fun:expr }

    pub fn export_nifs_macro(context: &mut ExtCtxt, span: Span, arguments: Arguments) -> PluginResult<Box<MacResult>> {
        let builder = ::aster::AstBuilder::new().span(span);

        let module_name = try!(string_from_spanned_literal(&arguments.mod_name, true));
        let module_name_c_str = CString::new(module_name).unwrap();
        let module_name_bytes = module_name_c_str.as_bytes_with_nul();
        let module_name_expr = builder.expr().lit().byte_str(module_name_bytes.to_vec());

        let vm_variant_expr = builder.expr().lit().byte_str(b"beam.vanilla\0".to_vec());

        let funs = try!(parse_delim_fun_list(&arguments.functions));
        let fun_list_entry_exprs: Vec<P<Expr>> = funs.iter().map(|x| {
            let name_c_str = CString::new((&x.name).clone()).unwrap();
            let name_bytes = name_c_str.as_bytes_with_nul();
            let name_expr = builder.expr().lit().byte_str(name_bytes.to_vec());

            let arity = &x.arity;
            let ident = builder.id(make_nif_int_id(&x.ident));
            quote_expr!(context, rustler::ruster_export::ErlNifFunc {
                name: $name_expr as *const u8,
                arity: $arity,
                function: $ident,
                flags: 0,
            })
        }).collect();
        let fun_list_ast = builder.expr().slice().with_exprs(fun_list_entry_exprs).build();
        let fun_exprs_len = funs.len();

        let nif_entry_item = quote_item!(context,
             static mut NIF_ENTRY: rustler::ruster_export::ErlNifEntry = rustler::ruster_export::ErlNifEntry {
                 major: rustler::ruster_export::NIF_MAJOR_VERSION,
                 minor: rustler::ruster_export::NIF_MINOR_VERSION,
                 name: $module_name_expr as *const u8,//b"test\0" as *const u8,
                 num_of_funcs: $fun_exprs_len as rustler::c_int,
                 funcs: &$fun_list_ast as *const rustler::ruster_export::ErlNifFunc,
                 load: Some(_rustler_nif_load_fun),
                 reload: None,
                 upgrade: None,
                 unload: None,
                 vm_variant: $vm_variant_expr as *const u8,
                 options: 0,
             };
         ).unwrap();

        let nif_load_handler = arguments.load_fun;
        let nif_load_fun_item = quote_item!(context,
            extern "C" fn _rustler_nif_load_fun(env: *mut rustler::ruster_export::ErlNifEnv,
                                                _priv_data: *mut *mut rustler::c_void,
                                                load_info: rustler::ERL_NIF_TERM) -> rustler::c_int {
                rustler::codegen_runtime::handle_nif_init_call($nif_load_handler, env, load_info)
            }
        ).unwrap();

        let nif_entry_fun_item = quote_item!(context,
            #[no_mangle]
            pub extern "C" fn nif_init() -> *const rustler::ruster_export::ErlNifEntry {
                unsafe { &NIF_ENTRY }
            }
        ).unwrap();

        let nif_fun_defs: Vec<P<Item>> = funs.iter().map(|x| {
            let fun_name = &x.ident;
            let fun_name_ident = builder.id(fun_name);
            let int_fun_name_ident = builder.id(make_nif_int_id(&fun_name));
            quote_item!(context,
                extern "C" fn $int_fun_name_ident(env: *mut rustler::ruster_export::ErlNifEnv,
                                     argc: rustler::c_int,
                                     argv: *const rustler::ERL_NIF_TERM) -> rustler::ERL_NIF_TERM {
                    rustler::codegen_runtime::handle_nif_call($fun_name_ident, 0, env, argc, argv)
                };
            ).unwrap()
        }).collect();

        let mut items = vec![nif_entry_item, nif_entry_fun_item, nif_load_fun_item];
        items.extend(nif_fun_defs);
        Ok(MacEager::items(SmallVector::many(items)))
    }
}
