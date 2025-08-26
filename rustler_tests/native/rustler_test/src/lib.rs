mod test_atom;
mod test_binary;
mod test_codegen;
mod test_dirty;
#[cfg(feature = "nif_version_2_16")]
mod test_dyncall;
mod test_env;
mod test_error;
mod test_list;
mod test_local_pid;
mod test_map;
mod test_nif_attrs;
mod test_panic;
mod test_path;
mod test_primitives;
mod test_range;
mod test_resource;
mod test_term;
mod test_thread;
mod test_tuple;

rustler::init!("Elixir.RustlerTest", load = load);

fn load(env: rustler::Env, _: rustler::Term) -> bool {
    test_resource::on_load(env)
}
