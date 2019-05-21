/// Detect NIF version to build against
use std::env;
use std::process::Command;

extern crate lazy_static;
use lazy_static::lazy_static;

extern crate which;
use which::which;

lazy_static! {
    // keep this sorted by version number
    static ref NIF_VERSION: Vec<&'static str> = vec![
        "2.7", "2.8", "2.9", "2.10", "2.11", "2.12", "2.13", "2.14", "2.15"
    ];
}

fn main() {
    let version = match env::var("RUSTLER_NIF_VERSION") {
        Ok(version) => version,
        Err(_) => get_version_from_erl(),
    };

    activate_versions(&version);
}

fn get_version_from_erl() -> String {
    let erl = which("erl").expect("expected to find 'erl' executable");
    let args = vec![
        "-noshell",
        "-eval",
        r#"io:format("~s~n", [erlang:system_info(nif_version)]), init:stop()."#,
    ];

    let version = Command::new(erl)
        .args(&args)
        .output()
        .expect("failed to execute 'erl'")
        .stdout;

    let version = String::from_utf8(version).expect("convert version to String");

    version.trim().into()
}

fn activate_versions(version: &str) {
    let index = NIF_VERSION
        .iter()
        .position(|&v| v == version)
        .expect(&format!(
            "Erlang version {} not handled, please file a a bug report.",
            version
        ));

    for i in 0..=index {
        println!(
            "cargo:rustc-cfg=nif_version_{}",
            version_feature(NIF_VERSION[i])
        );
    }
}

fn version_feature(version: &str) -> String {
    version.replace(".", "_")
}
