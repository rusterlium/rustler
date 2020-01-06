# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Simple `Debug` impl for `rustler::Error`
- Support newtype and tuple structs for `NifTuple` and `NifRecord`
- `rustler::Error::Term` encoding an arbitrary boxed encoder, returning `{:error, term}`

### Fixed

- Compilation time of generated decoders has been reduced significantly.
- Fixed a segfault caused by `OwnedEnv::send_and_clear`

### Changes

- Renamed `Pid` to `LocalPid` to clarify that it can't point to a remote process
- Dependencies have been updated.
- Derive macros have been refactored.
- Macros have been renamed and old ones have been deprecated:
  - `rustler_export_nifs!` is now `rustler::init!`
  - `rustler_atoms!` is now `rustler::atoms!`
  - `resource_struct_init!` is now `rustler::resource!`
- New `rustler::atoms!` macro removed the `atom` prefix from the name:

```rust
//
// Before
//
rustler::rustler_atoms! {
    atom ok;
    atom error;
}

//
// After
//
rustler::atoms! {
    ok,
    error,
}
```

- NIF functions can be initialized with a simplified syntax:

```rust
//
// Before
//
rustler::rustler_export_nifs! {
    "Elixir.Math",
    [
        ("add", 2, add)
    ],
    None
}

//
// After
//
rustler::init!("Elixir.Math", [add]);
```

- NIFs can be derived from regular functions, if the arguments implement `Decoder` and the return type implements `Encoder`:

```rust
//
// Before
//
fn add<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let num1: i64 = args[0].decode()?;
    let num2: i64 = args[1].decode()?;

    Ok((atoms::ok(), num1 + num2).encode(env))
}

//
// After
//
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
  a + b
}
```

- `rustler::nif` exposes more options to configure a NIF were the NIF is defined:

```rust

#[rustler::nif(schedule = "DirtyCpu")]
pub fn dirty_cpu() -> Atom {
    let duration = Duration::from_millis(100);
    std::thread::sleep(duration);

    atoms::ok()
}

#[rustler::nif(name = "my_add")]
fn add(a: i64, b: i64) -> i64 {
  a + b
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
