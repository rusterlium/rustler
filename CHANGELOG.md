# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Support for OTP22.
- Rust linting with [clippy](https://github.com/rust-lang/rust-clippy).
- Support for decoding IOLists as binaries, `Term::decode_as_binary`.

### Changes
- `rustler_codegen` is now reexported by the `rustler` crate. Depending on the `rustler_codegen` crate is deprecated.
- `erlang_nif-sys` has been renamed to `erl_nif_sys` and vendored into the rustler repo.
- Replaced the hand-rolled TOML parser in `rustler_mix` with the `toml-elixir` package.
- Improve error messages for derived encoders/decoders.
