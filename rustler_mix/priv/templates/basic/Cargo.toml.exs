[package]
name = "<%= library_name %>"
version = "0.1.0"
authors = []
build = "build.rs"

[lib]
name = "<%= library_name %>"
path = "src/lib.rs"
crate-type = ["dylib"]

[dependencies]
rustler = "<%= rustler_version %>"
rustler_codegen = "<%= rustler_version %>"
lazy_static = "0.2"
