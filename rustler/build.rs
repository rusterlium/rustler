/// Detect NIF version to build against
use std::env;
use std::process::Command;

// keep this sorted by version number
const NIF_VERSION: &[&str] = &[
    "2.7", "2.8", "2.9", "2.10", "2.11", "2.12", "2.13", "2.14", "2.15",
];

fn main() {
    let latest_version = NIF_VERSION.last().unwrap().to_string();
    let version = env::var("RUSTLER_NIF_VERSION")
        .unwrap_or_else(|_| get_version_from_erl().unwrap_or(latest_version));

    activate_versions(&version);
}

fn get_version_from_erl() -> Option<String> {
    let args = vec![
        "-noshell",
        "-eval",
        r#"io:format("~s~n", [erlang:system_info(nif_version)]), init:stop()."#,
    ];

    let version = Command::new("erl").args(&args).output().ok()?.stdout;

    let version = String::from_utf8(version).ok()?;

    Some(version.trim().into())
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
