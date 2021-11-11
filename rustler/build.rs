/// Detect NIF version to build against
/// It reads from the RUSTLER_NIF_VERSION env var.
///
/// If this env var is not present we try to read from the installed Erlang.
/// If the environment doens't have Erlang installed, then we use the latest
/// NIF version and write a warning to stderr.
use std::env;
use std::process::Command;

// keep this sorted by version number
const NIF_VERSION: &[&str] = &["2.14", "2.15", "2.16"];

fn main() {
    let latest_version = NIF_VERSION.last().unwrap().to_string();
    let version = env::var("RUSTLER_NIF_VERSION").unwrap_or_else(|_| {
        match get_version_from_erl() {
            Some(nif_version) if NIF_VERSION.contains(&nif_version.as_str()) => {
                eprintln!("RUSTLER_NIF_VERSION env var is not set. Using version from Erlang: {}", nif_version);
                nif_version
            },
            Some(ref nif_version) => panic!("The NIF version from Erlang is not supported by Rustler: {}", nif_version),
            None => {
                eprintln!("RUSTLER_NIF_VERSION env var is not set and `erl` command is not found. Using version {}", latest_version);
                latest_version
            },
        }
    });

    activate_versions(&version);

    // The following lines are important to tell Cargo to recompile if something changes.
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=RUSTLER_NIF_VERSION");
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
        .unwrap_or_else(|| {
            panic!(
                "Erlang version {} not handled, please file a a bug report.",
                version
            )
        });

    #[allow(clippy::needless_range_loop)]
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
