include!("build_common.rs");

fn main() {
    common::handle_nif_version_from_env();

    // The following lines are important to tell Cargo to recompile if something changes.
    println!("cargo:rerun-if-changed=build.rs");
}
