[package]
name = "<%= library_name %>"
version = "0.1.0"
authors = []
build = "build.rs"

[lib]
name = "<%= library_name %>"
path = "src/lib.rs"
crate-type = ["dylib"]

[features]

# Compile with syntex by default. This will make the build process work on
# Rust stable in exchange of worse error messages.
#
# You can optionally compile with Rust nightly for better error messages by
# either:
# * If using rustler-mix, add `default_features: false` to the crate config.
# * If using cargo directly, pass the `--no-default-features` flag.
# * Remove `with-syntex` from the default features list below.
default = ["with-syntex"]

with-syntex = [
    "rustler_codegen/with-syntex",
]

[build-dependencies]
# Build dependency is only really required if using syntex.
rustler_codegen = "<%= rustler_version %>"

[dependencies]
rustler = "<%= rustler_version %>"
rustler_codegen = "<%= rustler_version %>"
