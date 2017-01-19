use rustler::{NifEnv, NifTerm};

mod test_primitives;
mod test_list;
mod test_map;
mod test_resource;
mod test_binary;
mod test_atom;
mod test_thread;
mod test_env;

rustler_export_nifs!(
    "Elixir.RustlerTest",
    [
        ("add_u32", 2, test_primitives::add_u32),
        ("add_i32", 2, test_primitives::add_i32),
        ("echo_u8", 1, test_primitives::echo_u8),
        ("tuple_add", 1, test_primitives::tuple_add),

        ("sum_list", 1, test_list::sum_list),
        ("make_list", 0, test_list::make_list),

        ("sum_map_values", 1, test_map::sum_map_values),
        ("map_entries_sorted", 1, test_map::map_entries_sorted),

        ("resource_make", 0, test_resource::resource_make),
        ("resource_set_integer_field", 2, test_resource::resource_set_integer_field),
        ("resource_get_integer_field", 1, test_resource::resource_get_integer_field),
        ("resource_make_immutable", 1, test_resource::resource_make_immutable),
        ("resource_immutable_count", 0, test_resource::resource_immutable_count),

        ("atom_to_string", 1, test_atom::atom_to_string),
        ("atom_equals_ok", 1, test_atom::atom_equals_ok),

        ("make_shorter_subbinary", 1, test_binary::make_shorter_subbinary),
        ("parse_integer", 1, test_binary::parse_integer),

        ("threaded_fac", 1, test_thread::threaded_fac),
        ("threaded_sleep", 1, test_thread::threaded_sleep),

        ("send_all", 2, test_env::send_all),
        ("sublists", 1, test_env::sublists)
    ],
    Some(on_load)
);

fn on_load<'a>(env: NifEnv<'a>, _load_info: NifTerm<'a>) -> bool {
    test_resource::on_load(env);
    test_atom::on_load(env);
    true
}
