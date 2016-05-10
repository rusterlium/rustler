#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;

mod test_primitives;
use test_primitives::{add_u32, add_i32, tuple_add};

mod test_list;
use test_list::{sum_list};

rustler_export_nifs!(
    "Elixir.TestNative",
    [("add_u32", 2, add_u32),
     ("add_i32", 2, add_i32),
     ("tuple_add", 1, tuple_add),
     ("sum_list", 1, sum_list)],
    None
);

