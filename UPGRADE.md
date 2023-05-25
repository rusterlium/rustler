# Upgrade

This document is intended to simplify upgrading to newer versions by extending the changelog.

## 0.28 -> 0.29

`RUSTLER_NIF_VERSION` is deprecated and will not be considered anymore for 0.30.
The NIF version will also not be guessed anymore from a potentially available
installed Erlang version. By default, NIF libraries will now be compiled against
NIF version 2.14 which is compatible down to OTP21. The default will be adjusted
along with the supported OTP versions.

If additional features are required that use newer NIF versions, these can be
included explicitly in the project's `Cargo.toml` as, e.g.

```toml
[dependencies]
rustler = { version = "0.30", features = ["nif_version_2_17"] }
```

With this configuration, the resulting NIF library will only work from OTP26
onwards, but will also have access to the largest feature set.

## 0.26 -> 0.27

`MIX_ENV` is no longer considered for determining the build profile. Now, the
profile defaults to `:release`. Use the `:mode` option to pick another profile
explicitly. See #496.

## 0.21 -> 0.22

0.22 changes how to define NIFs. Users upgrading to 0.22 should to do these things:

1. Replace `rustler_atoms!` with `rustler::atoms!`
2. Replace `resource_struct_init!` with `rustler::resource!`
3. Replace `rustler::rustler_export_nifs!` with `rustler::init!`
4. Use the new `rustler::nif` proc_macro to declare NIFs

Replacing `rustler_atoms!` with `rustler::atoms!` is fairly simple and already
sufficiently described in [CHANGELOG.md](./CHANGELOG.md). Similarly, replacing
`resource_struct_init!` with `rustler::resource!` is a simple rename, so this does
not need additional examples here.

### Replace `rustler::rustler_export_nifs!` with `rustler::init!`

`rustler::init!` in combination with the new `rustler::nif` proc_macro
simplifies exporting NIFs. Before, the NIFs and their arity needed to be specified
using tuple syntax:

```rust
rustler::rustler_export_nifs! {
    "Elixir.Math",
    [
        ("add", 2, add),
	("long_running_operation", 0, long_running_operation, SchedulerFlags::DirtyCpu)
    ],
    None
}
```

Now, listing the NIFs directly is sufficient:

```rust
rustler::init!("Elixir.Math", [add, long_running_operation]);
```

With this new macro, defining an `on_load` function (e.g. to set up a resource with
`rustler::resource!`), is done like this:

```rust
rustler::init!("Elixir.Math", [add, long_running_operation], load = a_function);
```

Note that NIF flags such as `SchedulerFlags::DirtyCpu` are not declared in `rustler::init!`, but
using the proc_macro `rustler::nif`. See further below for information on migration NIF flags.

### Use the new `rustler::nif` proc_macro to declare NIFs

0.22 introduces a new `proc_macro` allowing to spell out the parameter of a NIF
directly instead of using an `args: &[Term<'a>]`. Lets consider an example `add()`,
where the Elixir function looks like this:

```elixir
def add(left, right), do: :erlang.nif_error(:not_loaded)
```

Previously, the signature of the corresponding NIF might have looked like this:

```rust
fn add<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error>
```

When calling the NIF from Elixir as `add(1, 2)`, `args` would then contain two
`Term`, one for 1, and one for 2. With 0.22, this becomes more obvious, as the
NIFs signature resembles the Elixir function's signature:

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64
```

Under the hood, this is implemented by the `rustler::nif` proc_macro. For the
new form to work, the parameters' types need to implement `Decoder`, and the
return type needs to implement `Encoder`.

#### What if `Env` is required in the function?

Sometimes, we still need the environment `Env` for the NIF. For example, if
work with `Binary` and `OwnedBinary`, the environment would be needed to create a `Binary`
from an `OwnedBinary`. To allow this, `env: Env<'a>` can be added explicitly as well:

```rust
#[rustler::nif]
pub fn map_entries_sorted<'a>(env: Env<'a>, iter: MapIterator<'a>) -> NifResult<Vec<Term<'a>>>
```

`env` can then be used the same way as before.

#### Migrating Flags and Rename

The `rustler::nif` proc_macro allows setting options directly on a NIF. Assume that we have a
NIF called `_long_running_operation`, which used to be declared prior to Rustler v0.22 like this:

```rust
// Before

rustler::rustler_export_nifs! {
    "Elixir.SomeNif",
    [
	// Note that the function in Rust is long_running_operation, but the NIF is exported as
	// _long_running_operation!
	("_long_running_operation", 0, long_running_operation, SchedulerFlags::DirtyCpu)
    ],
    None
}

fn long_running_operation<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
  // ...
}
```

This definition declares that a function `_long_running_operation` with arity
zero is to be exported, and that this function should be schedules on the
`DirtyCpu` scheduler. With the changes in Rustler v0.22, the function would be declared like
this:

```rust
// Now

rustler::init!("Elixir.SomeNif", [long_running_operation]);

#[rustler::nif(
    rename = "_long_running_operation",
    schedule = "DirtyCpu"
  )]
fn long_running_operation() -> TheProperReturnType {
 // ..
}
```
