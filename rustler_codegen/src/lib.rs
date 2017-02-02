extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;

#[macro_use]
extern crate quote;

mod util;
mod tuple;
mod map;

#[proc_macro_derive(NifMap)]
pub fn nif_map(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let gen = map::transcoder_decorator(&ast);
    gen.unwrap().parse().unwrap()
}

#[proc_macro_derive(NifTuple)]
pub fn nif_tuple(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let gen = tuple::transcoder_decorator(&ast);
    gen.unwrap().parse().unwrap()
}
