#![crate_type="dylib"]
#![cfg_attr(not(feature = "with-syntex"), plugin(easy_plugin, synthax))]
#![cfg_attr(not(feature = "with-syntex"), feature(plugin, plugin_registrar, quote, rustc_private))]

#[allow(plugin_as_library)]
extern crate easy_plugin;
extern crate aster;
extern crate synthax;

#[cfg(not(feature = "with-syntex"))]
extern crate syntax;
//extern crate rustc;
#[cfg(not(feature = "with-syntex"))]
extern crate rustc_plugin;

#[cfg(feature = "with-syntex")]
extern crate syntex;
#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;
#[cfg(feature = "with-syntex")]
use std::path::Path;

#[cfg(feature = "with-syntex")]
include!(concat!(env!("OUT_DIR"), "/lib.rs"));

#[cfg(not(feature = "with-syntex"))]
include!("lib.rs.in");

#[cfg(feature = "with-syntex")]
pub fn expand<S, D>(src: S, dst: D) -> Result<(), syntex::Error>
    where S: AsRef<Path>, D: AsRef<Path> 
{
    let mut reg = syntex::Registry::new();
    
    reg.add_decorator("ExStruct", ex_struct::transcoder_decorator);
    reg.add_decorator("NifTuple", tuple::transcoder_decorator);

    reg.add_decorator("NifResource", resource::resource_struct_def_decorator);
    use syntax::ext::base::{TTMacroExpander, ExtCtxt, MacResult};
    use syntax::tokenstream::TokenTree;;
    use syntax::codemap::Span;

    struct ResourceStructInitMacroHack {}
    impl TTMacroExpander for ResourceStructInitMacroHack {
        fn expand<'cx>(&self,
                       ecx: &'cx mut ExtCtxt,
                       span: Span,
                       token_tree: &[TokenTree])
            -> Box<MacResult+'cx> {
                resource::resource_struct_init_macro(ecx, span, token_tree)
            }
    }
    reg.add_macro("resource_struct_init", ResourceStructInitMacroHack {});

    reg.expand("", src.as_ref(), dst.as_ref())
}

#[cfg(not(feature = "with-syntex"))]
#[plugin_registrar]
pub fn register(reg: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    reg.register_syntax_extension(
        builder.name("ExStruct"),
        syntax::ext::base::MultiDecorator(Box::new(ex_struct::transcoder_decorator)));
    reg.register_syntax_extension(
        builder.name("NifTuple"),
        syntax::ext::base::MultiDecorator(Box::new(tuple::transcoder_decorator)));

    reg.register_syntax_extension(
        builder.name("NifResource"),
        syntax::ext::base::MultiDecorator(Box::new(resource::resource_struct_def_decorator)));
    reg.register_macro("resource_struct_init", resource::resource_struct_init_macro);
}
