# Rustler BigInt

`rustler_bigint` provides support for Erlang's arbitrarily-sized integers.

## Installation

Add this to `Cargo.toml`:

```toml
[dependencies]
rustler_bigint = { version = "0.1" }
```

## Example

Lets assume that we need to handle integers of variable size. Some might fit
into Rust's `i64`, but others might not. For example:

```elixir
large = 2**65 # This does not fit into i64, it is an Erlang big integer
```

In Rust, we can use `rustler_bigint::BigInt` to pass integer values of
different sizes into a NIF. The type `rustler_bigint::BigInt` is a newtype
wrapping `num_bigint::BigInt` and implements `std::ops::Deref`, so functions
from `num_bigint::BigInt` can be called directly.

```rust
/// Simply echo `large` back to the caller.
#[rustler::nif]
pub fn handle_large(large: rustler_bigint::BigInt) -> NifResult<BigInt> {
  Ok(large)
}
```
