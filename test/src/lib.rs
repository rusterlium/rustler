#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
use rustler::{NifEnv, NifTerm};

mod test_primitives;
use test_primitives::{add_u32, add_i32, tuple_add};

mod test_list;
use test_list::{sum_list};

mod test_resource;
use test_resource::on_load as resource_on_load;
use test_resource::{resource_make, resource_set_integer_field, resource_get_integer_field};

mod test_binary;
use test_binary::make_shorter_subbinary;

rustler_export_nifs!(
    "Elixir.TestNative",
    [("add_u32", 2, add_u32),
     ("add_i32", 2, add_i32),
     ("tuple_add", 1, tuple_add),
     ("sum_list", 1, sum_list),
     ("resource_make", 0, resource_make),
     ("resource_set_integer_field", 2, resource_set_integer_field),
     ("resource_get_integer_field", 1, resource_get_integer_field),
     ("make_shorter_subbinary", 1, make_shorter_subbinary)],
    Some(on_load)
);

fn on_load(env: &NifEnv, load_info: NifTerm) -> bool {
    resource_on_load(env);
    true
}
