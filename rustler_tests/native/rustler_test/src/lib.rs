mod test_async;
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

// Temporarily add async_add explicitly to debug
rustler::init!("Elixir.RustlerTest", load = load);

fn load(env: rustler::Env, load_info: rustler::Term) -> bool {
    // Configure Tokio runtime from Elixir load_data
    #[cfg(feature = "tokio_rt")]
    {
        if let Ok(config) = load_info.decode::<rustler::tokio::RuntimeConfig>() {
            rustler::tokio::configure(config).ok();
        }
    }

    test_resource::on_load(env)
}
