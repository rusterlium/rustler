use rustler::Atom;
use std::{ffi::OsStr, fs::read_to_string, path::PathBuf};

static mut DATASET: Option<Box<str>> = None;

fn initialize_dataset(mut asset_path: PathBuf) {
    asset_path.push("demo_dataset.txt");

    // https://github.com/elixir-lsp/elixir-ls/issues/604
    eprintln!("Loading dataset from {:?}.", &asset_path);

    let data = read_to_string(asset_path).unwrap().into_boxed_str();
    let data = Some(data);

    // Safety: assumes that this function is being called once when
    // dynamically loading this library.
    // `load()` is being called exactly once and OTP will not allow any other function call
    // before this function returns
    unsafe { DATASET = data };
}

#[rustler::nif]
fn get_dataset() -> &'static str {
    // Safety: see `initialize_dataset()`
    unsafe { DATASET.as_ref() }.expect("Dataset is not initialized")
}

fn load<'a>(env: rustler::Env<'a>, args: rustler::Term<'a>) -> bool {
    let key = Atom::from_str(env, "priv_path").unwrap().to_term(env);
    let priv_path = args.map_get(key).unwrap();
    let priv_path = priv_path.into_binary().unwrap().as_slice();

    let asset_path = build_path_buf(priv_path);

    initialize_dataset(asset_path);

    true
}

#[cfg(unix)]
fn build_path_buf(priv_path: &[u8]) -> PathBuf {
    use std::os::unix::prelude::OsStrExt;

    let priv_path = OsStr::from_bytes(priv_path);
    PathBuf::from(priv_path)
}

#[cfg(windows)]
fn build_path_buf(priv_path: &[u8]) -> PathBuf {
    let string_slice = std::str::from_utf8(priv_path).expect("Data is not valid UTF-8, we could decode it without valid UTF-8 requirements but lets not do that for now because its easier this way");
    let priv_path = OsStr::new(string_slice);
    PathBuf::from(priv_path)
}

rustler::init!("Elixir.DynamicData", [get_dataset], load = load);
