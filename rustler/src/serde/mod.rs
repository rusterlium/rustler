/*!
# (Experimental) Serde Support

The `serde` conversion support is derived from [`serde_rustler`](https://github.com/sunny-g/serde_rustler)
and uses mostly the same conventions and API for now. After the initial release,
the conversions will at least be made more configurable (e.g. to support records
in a better way and allow more Erlang-y configurations).

## Example

```rust
use rustler::{self, Encoder, SerdeTerm};
use serde::{Serialize, Deserialize};

rustler::init!("Elixir.SerdeNif");

// NOTE: to serialize to the correct Elixir record, you MUST tell serde to
// rename the variants to the full Elixir record module atom.
#[derive(Debug, Serialize, Deserialize)]
enum AnimalType {
    #[serde(rename = "Elixir.SerdeNif.AnimalType.Cat")]
    Cat(String),
    #[serde(rename = "Elixir.SerdeNif.AnimalType.Dog")]
    Dog(String),
}

// NOTE: to serialize to an actual Elixir struct (rather than a just map with
// a :__struct__ key), you MUST tell serde to rename the struct to the full
// Elixir struct module atom.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename = "Elixir.SerdeNif.Animal")]
struct Animal {
    #[serde(rename = "type")]
    _type: AnimalType,
    name: String,
    age: u8,
    owner: Option<String>,
}

#[rustler::nif]
fn readme(SerdeTerm(animal): SerdeTerm<Animal>) -> impl Encoder {
    println!("serialized animal: {:?}", animal);
    SerdeTerm(animal)
}
```

## Conversion Table

The conversions table is for now identical to the one that `serde_rustler` used,
with added support for 128-bit integers.

| Type Name | Serde (Rust) Values | Elixir Terms (default behaviour) | `deserialize_any` into Elixir Term |
|-----|-----|-----|-----|
| bool | `true` or `false` | `true` or `false` | `true` or `false` |
| <sup>[1](#todo)</sup> number | `i8..i128`, `u8..u128`, `f32`, `f64` | `number` | `number` as `f64`, `i64`, `u64` or larger |
| char | `'A'` | `[u32]` | `[u32]` |
| string | `""` | `bitstring` | `bitstring` |
| byte array | `&[u8]` or `Vec<u8>` | `<<_::_*8>>` | `bitstring` |
| option | `Some(T)` or `None` | `T` or `:nil` | `T` or `:nil` |
| unit | `None` | `:nil` | `:nil` |
| unit struct | `struct Unit` | `:nil` | `:nil` |
| <sup>[3](#atom)</sup> unit variant | `E::A` in `enum UnitVariant { A }` | `:A` | `"A"` |
| <sup>[3](#atom)</sup> newtype struct | `struct Millimeters(u8)` | `{:Millimeters, u8}` | `["Millimeters", u8]` |
| <sup>[3](#atom)</sup> newtype variant | `E::N` in `enum E { N(u8) }` | `{:N, u8}` | `["N", u8]` |
| <sup>[3](#atom)</sup> newtype variant (any `Ok` and `Err` tagged enum) | `enum R<T, E> { Ok(T), Err(E) }` | `{:ok, T}` or `{:error, E}` | `["Ok", T]` or `["Err", E]` |
| seq | `Vec<T>` | `[T,]` | `[T,]` |
| tuple | `(u8,)` | `{u8,}` | `[u8,]` |
| <sup>[3](#atom)</sup> tuple struct | `struct Rgb(u8, u8, u8)` | `{:Rgb, u8, u8, u8}` | `["Rgb", u8, u8, u8]` |
| <sup>[3](#atom)</sup> tuple variant | `E::T` in `enum E { T(u8, u8) }` | `{:T, u8, u8}` | `["T", u8, u8]` |
| <sup>[1](#todo)</sup> map | `HashMap<K, V>` | `%{}` | `%{}` |
| <sup>[3](#atom)</sup> struct | `struct Rgb { r: u8, g: u8, b: u8 }` | `%Rgb{ r: u8, g: u8, b: u8 }` | `%{"r" => u8, "g" => u8, "b" => u8}` |
| <sup>[3](#atom)</sup> struct variant | `E::S` in `enum E { Rgb { r: u8, g: u8, b: u8 } }` | `%Rgb{ r: u8, g: u8, b: u8 }` | `%{"r" => u8, "g" => u8, "b" => u8}` |

<a name="todo">1</a>: API still being decided / implemented.

<a name="atom">2</a>: When serializing unknown input to terms, atoms will not be created and will instead be replaced with Elixir bitstrings. Therefore "records" will be tuples (`{bitstring, ...}`) and "structs" will be maps containing `%{:__struct__ => bitstring}`. The unfortunate consequence of this is that `deserialize_any` will lack the necessary information needed deserialize many terms without type hints, such as `structs`, `enums` and `enum variants`, and `tuples`.
*/

pub mod atoms;
mod de;
mod error;
mod ser;
mod util;

pub use de::{from_term, Deserializer};
pub use error::Error;
pub use ser::{to_term, Serializer};

use crate::{Decoder, Encoder, Env, NifResult, Term};

/// Wrapper type to en- and decode serde terms
///
/// If the wrapped type implements `serde::Serialize`, `SerdeTerm<T>` implements
/// `Encoder` and will thus be transparently converted *to* a BEAM object.
///
/// If the wrapped type implements `serde::Deserialize`, it implements `Decoder`
/// and can thus be transparently converted *from* a BEAM object.
pub struct SerdeTerm<T>(pub T);

impl<T: serde::Serialize> Encoder for SerdeTerm<T> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        let ser = crate::serde::Serializer::from(env);
        self.0.serialize(ser).unwrap()
    }
}

impl<'a, T: serde::Deserialize<'a> + 'a> Decoder<'a> for SerdeTerm<T> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(decoded) = from_term(term) {
            Ok(SerdeTerm(decoded))
        } else {
            panic!("invalid conversion")
        }
    }
}
