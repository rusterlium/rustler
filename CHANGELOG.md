# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changes

- Dependencies have been updated.
- Derive macros have been refactored.
- Macros have been renamed and old ones have been deprecated:
  - `rustler_export_nifs!` is now `rustler::init!`
  - `rustler_atoms!` is now `rustler::atoms!`
  - `resource_struct_init!` is now `rustler::resource!`
- New `rustler::atoms!` macro removed the `atom` prefix from the name:

```rs
# Before

rustler::rustler_atoms! {
    atom ok;
    atom error;
}

# After

rustler::atoms! {
    ok,
    error,
}
```

## [0.21.0] - 2019-09-07

### Added

- Support for OTP22.
- Rust linting with [clippy](https://github.com/rust-lang/rust-clippy).
- Support for decoding IOLists as binaries, `Term::decode_as_binary`.

### Changes

- `rustler_codegen` is now reexported by the `rustler` crate. Depending on the `rustler_codegen` crate is deprecated.
- `erlang_nif-sys` has been renamed to `rustler_sys` and vendored into the rustler repo.
- Replaced the hand-rolled TOML parser in `rustler_mix` with the `toml-elixir` package.
- Improve error messages for derived encoders/decoders.
- Rust `bool` now corresponds only to booleans (`false`, `true`) in Elixir. Previously, `nil` and `false` were both decodable to
  `bool`. To use the previous behaviour, a `Truthy` newtype was introduced.
