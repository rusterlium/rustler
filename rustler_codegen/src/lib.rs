#![crate_type="dylib"]
#![feature(plugin, plugin_registrar, quote, rustc_private)]
#![plugin(easy_plugin)]

#[allow(plugin_as_library)]
extern crate easy_plugin;
extern crate aster;

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use rustc_plugin::Registry;

mod export;
mod ex_struct;
mod resource;
mod util;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    let builder = aster::AstBuilder::new();
    
    //reg.register_macro("rustler_export_nifs", export::export_nifs_macro);
    reg.register_macro("rustler_export_nifs", export::export_nifs_macro);
    reg.register_syntax_extension(
        builder.name("ExStruct"),
        syntax::ext::base::MultiDecorator(Box::new(ex_struct::transcoder_decorator)));

    reg.register_syntax_extension(
        builder.name("NifResource"),
        syntax::ext::base::MultiDecorator(Box::new(resource::resource_struct_def_decorator)));
    reg.register_macro("resource_struct_init", resource::resource_struct_init_macro);
}
