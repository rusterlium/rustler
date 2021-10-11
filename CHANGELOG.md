# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

See [`UPGRADE.md`](./UPGRADE.md) for additional help when upgrading to newer versions.

## [Unreleased]

## [0.22.2] - 2021-10-07

### Fixed

- Fixed a regression introduced with #386: `Rustler.Compiler.Config` called into `cargo` when `skip_compilation?` was set, breaking setups where cargo is not installed. Fixed with #389, thanks @karolsluszniak

## [0.22.1] - 2021-10-05

### Fixed

- [Breaking change] codegen-generated decoders always raise an error instead of
  causing the calling NIF to return an atom in some cases
- Fix codegen problem for untagged enums (#370)
- Fix handling local dependencies with `@external_resources` (#381)

## [0.22.0] - 2021-06-22

### Added

- Simple `Debug` impl for `rustler::Error`
- Support newtype and tuple structs for `NifTuple` and `NifRecord`
- `rustler::Error::Term` encoding an arbitrary boxed encoder, returning `{:error, term}`
- Generic encoder/decoder for `HashMap<T, U>`, where `T: Decoder` and `U: Decoder`

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
    atom renamed_atom = "Renamed";
}

//
// After
//
rustler::atoms! {
    ok,
    error,
    renamed_atom = "Renamed",
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

### Deprecations

The rustler compiler has been deprecated and will be removed with v1.0. NIFs
are no longer defined in `mix.exs`, but are configured with `use Rustler`.  See
the documentation for the `Rustler` module. To migrate to the new
configuration:

* Drop `:rustler` from the `:compilers` key in your `mix.exs` `project/0` function
* Drop `:rustler_crates` from `project/0` and move the configurations into the `use Rustler`
  of your NIF module or application config:

  ```elixir
  # config/dev.exs
  config :my_app, MyApp.Native,
    mode: :debug
  ```

For more information, see [the documentation](https://hexdocs.pm/rustler/0.22.0-rc.1/Rustler.html#module-configuration-options).

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
