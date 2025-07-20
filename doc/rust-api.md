# Crate Documentation

**Version:** 0.37.1

**Format Version:** 56

# Module `rustler`

[Github](https://github.com/rusterlium/rustler)
[Example](https://github.com/rusterlium/NifIo)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means there should be no
ways to crash the BEAM (Erlang VM). The library provides facilities for generating the
boilerplate for interacting with the BEAM, handles encoding and decoding of Erlang terms, and
catches rust panics before they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir is favored as of
now.

This crate provides the entire runtime library for rustler. Code generators are located in the
rustler_codegen library.

# Getting Started
There is a [`:rustler`](https://hex.pm/packages/rustler) package on hex.pm that provides
functionality which makes working with Rustler easier, including project generators, an
automatic NIF compiler for Mix, and utilities for loading the compiled NIF.

For more information about this, see [the documentation for
rustler](https://hexdocs.pm/rustler).

## Modules

## Module `types`

**Attributes:**

- `Other("#[attr = MacroUse {arguments: UseAll}]")`

```rust
pub mod types { /* ... */ }
```

### Modules

## Module `atom`

**Attributes:**

- `Other("#[attr = MacroUse {arguments: UseAll}]")`

```rust
pub mod atom { /* ... */ }
```

### Types

#### Struct `Atom`

```rust
pub struct Atom {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn as_c_arg(self: Self) -> NIF_TERM { /* ... */ }
  ```

- ```rust
  pub fn to_term(self: Self, env: Env<''_>) -> Term<''_> { /* ... */ }
  ```

- ```rust
  pub fn from_term(term: Term<''_>) -> NifResult<Self> { /* ... */ }
  ```

- ```rust
  pub fn from_bytes(env: Env<''_>, bytes: &[u8]) -> NifResult<Atom> { /* ... */ }
  ```
  Return the atom whose text representation is `bytes`, like `erlang:binary_to_atom/2`.

- ```rust
  pub fn try_from_bytes(env: Env<''_>, bytes: &[u8]) -> NifResult<Option<Atom>> { /* ... */ }
  ```
  Return the atom whose text representation is `bytes`, like `erlang:binary_to_existing_atom/2`, if atom with given text representation exists.

- ```rust
  pub fn from_str(env: Env<''_>, string: &str) -> NifResult<Atom> { /* ... */ }
  ```
  Return the atom whose text representation is the given `string`, like `erlang:list_to_atom/2`.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> Atom { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Copy**
- **Debug**
  - ```rust
    fn fmt(self: &Self, f: &mut fmt::Formatter<''_>) -> Result<(), fmt::Error> { /* ... */ }
    ```

- **Decoder**
  - ```rust
    fn decode(term: Term<''a>) -> NifResult<Atom> { /* ... */ }
    ```

- **Encoder**
  - ```rust
    fn encode<''a>(self: &Self, env: Env<''a>) -> Term<''a> { /* ... */ }
    ```

- **Eq**
- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Hash**
  - ```rust
    fn hash<H: Hasher>(self: &Self, state: &mut H) { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **PartialEq**
  - ```rust
    fn eq(self: &Self, other: &Atom) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &Term<''a>) -> bool { /* ... */ }
    ```

- **RefUnwindSafe**
- **Send**
- **StructuralPartialEq**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
### Functions

#### Function `is_truthy`

```rust
pub fn is_truthy(term: crate::Term<''_>) -> bool { /* ... */ }
```

#### Function `nif_panicked`

The `nif_panicked` atom.

```rust
pub fn nif_panicked() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `nil`

The `nil` atom.

```rust
pub fn nil() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `undefined`

The `undefined` atom, commonly used in Erlang libraries to express the
absence of value.

```rust
pub fn undefined() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `ok`

The `ok` atom, commonly used in success tuples.

```rust
pub fn ok() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `error`

The `error` atom, commonly used in error tuples.

```rust
pub fn error() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `badarg`

The `badarg` atom, which Rustler sometimes returns to indicate that a function was
called with incorrect arguments.

```rust
pub fn badarg() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `false_`

The `false` atom. (Trailing underscore because `false` is a keyword in Rust.)

If you're looking to convert between Erlang terms and Rust `bool`
values, use `Encoder` and `Decoder` instead.

```rust
pub fn false_() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `true_`

The `true` atom. (Trailing underscore because `true` is a keyword in Rust.)

If you're looking to convert between Erlang terms and Rust `bool`
values, use `Encoder` and `Decoder` instead.

```rust
pub fn true_() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `__struct__`

The `__struct__` atom used by Elixir.

```rust
pub fn __struct__() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `first`

The `first` atom used by `Elixir.Range`.

```rust
pub fn first() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `last`

The `last` atom used by `Elixir.Range`.

```rust
pub fn last() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `step`

The `step` atom used by `Elixir.Range` vor Elixir >= v1.12

```rust
pub fn step() -> $crate::types::atom::Atom { /* ... */ }
```

## Module `binary`

Safe wrappers around Erlang binaries.

Rustler provides three binary types: [`Binary`], [`NewBinary`] and
[`OwnedBinary`]. All represent a contiguous region `u8`s, and they all use
the Erlang allocator. The primary difference between them is their ownership
semantics.

The _owned_ in `OwnedBinary` refers to the fact that it owns the binary it
wraps. The _owner_ of an `OwnedBinary` is free to modify its contents. Ownership
lasts until it is dropped or consumed by converting it into a regular
`Binary`. An `OwnedBinary` cannot be copied or cloned and is thus always moved.

The `Binary` type is an immutable shared-reference to a binary. `Binary`s are
cheap to copy: all copies of a `Binary` point to the original `Binary`'s
data. Additionally, a `Binary`'s lifetime is tied to that of the NIF's [`Env`],
preventing outstanding references to the data after a NIF returns.

`NewBinary` is a way of creating a `Binary` without going via `OwnedBinary`.
This can improve performance, since `NewBinary`s can be allocated on the
heap if they are small. Unlike `OwnedBinary`, `NewBinary`s lifetime is tied
to that of the NIF's [`Env`]. `NewBinary` must be converted to a `Binary`
or directly to a `Term` before it can be passed to Erlang.

# Examples

Constructing an `OwnedBinary`:

```no_run
# use rustler::OwnedBinary;
{
    let mut bin = OwnedBinary::new(5).expect("allocation failed");
    bin.as_mut_slice().copy_from_slice("hello".as_bytes());
} // <- `bin` is dropped here
```

The following NIF takes a binary as its only parameter and returns a new binary
where each element is exclusive-or'ed with a constant:

```no_run
# use rustler::{Env, OwnedBinary, Binary, NifResult, Error};
#[rustler::nif]
fn xor_example<'a>(env: Env<'a>, bin: Binary<'a>) -> NifResult<Binary<'a>> {
    let mut owned: OwnedBinary = bin.to_owned().ok_or(Error::Term(Box::new("no mem")))?;
    for byte in owned.as_mut_slice() {
        *byte ^= 0xAA;
    }

    // Ownership of `owned`'s data is transferred to `env` on the
    // following line, so no additional heap allocations are incurred.
    Ok(Binary::from_owned(owned, env))
}
```

The contents of a newly-allocated `OwnedBinary` is not initialized to any
particular value. If your usage of the binary requires the it's data to be
zeroed, for example, then you must explicit zero it. In this example, we
manually zeroize the binary before passing it as slice to a third party
function.

```no_run
# fn some_third_party_api(buf: &mut [u8]) {
#     for elem in buf {
#         if *elem == 0 { *elem = 1 } else { panic!("Not a zero!") }
#     }
# }
# use rustler::{Env, OwnedBinary, Binary, NifResult, Error};
#[rustler::nif]
fn wrapper_for_some_<'a>(env: Env<'a>) -> NifResult<Binary<'a>> {
    let mut owned = OwnedBinary::new(100).ok_or(Error::Term(Box::new("no mem")))?;
    for byte in owned.as_mut_slice() {
        *byte = 0;
    }

    // Some third party API which requires the slice to be all zeros on entry.
    some_third_party_api(owned.as_mut_slice());

    // The imaginary API call presumedly filled in our binary with meaningful
    // data, so let's return it.
    Ok(Binary::from_owned(owned, env))
}

```

[`Binary`]: struct.Binary.html
[`Env`]: ../../env/struct.Env.html
[`OwnedBinary`]: struct.OwnedBinary.html

```rust
pub mod binary { /* ... */ }
```

### Types

#### Struct `OwnedBinary`

An mutable smart-pointer to an Erlang binary.

See [module-level doc](index.html) for more information.

```rust
pub struct OwnedBinary(/* private field */);
```

##### Fields

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `private` | *Private field* |

##### Implementations

###### Methods

- ```rust
  pub unsafe fn from_raw(inner: ErlNifBinary) -> OwnedBinary { /* ... */ }
  ```

- ```rust
  pub fn new(size: usize) -> Option<OwnedBinary> { /* ... */ }
  ```
  Allocates a new `OwnedBinary` with size `size`.

- ```rust
  pub fn from_unowned(src: &Binary<''_>) -> Option<OwnedBinary> { /* ... */ }
  ```
  Copies `src`'s data into a new `OwnedBinary`.

- ```rust
  pub fn realloc(self: &mut Self, size: usize) -> bool { /* ... */ }
  ```
  Attempts to reallocate `self` with the new size.

- ```rust
  pub fn realloc_or_copy(self: &mut Self, size: usize) { /* ... */ }
  ```
  Attempts to reallocate `self` with the new size.

- ```rust
  pub fn as_slice(self: &Self) -> &[u8] { /* ... */ }
  ```
  Extracts a slice containing the entire binary.

- ```rust
  pub fn as_mut_slice(self: &mut Self) -> &mut [u8] { /* ... */ }
  ```
  Extracts a mutable slice of the entire binary.

- ```rust
  pub fn release(self: Self, env: Env<''_>) -> Binary<''_> { /* ... */ }
  ```
  Consumes `self` and returns an immutable `Binary`.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

  - ```rust
    fn borrow(self: &Self) -> &[u8] { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut [u8] { /* ... */ }
    ```

- **Deref**
  - ```rust
    fn deref(self: &Self) -> &[u8] { /* ... */ }
    ```

- **DerefMut**
  - ```rust
    fn deref_mut(self: &mut Self) -> &mut [u8] { /* ... */ }
    ```

- **Drop**
  - ```rust
    fn drop(self: &mut Self) { /* ... */ }
    ```

- **Eq**
- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **FromIterator**
  - ```rust
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self { /* ... */ }
    ```

- **Hash**
  - ```rust
    fn hash<H: Hasher>(self: &Self, state: &mut H) { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **PartialEq**
  - ```rust
    fn eq(self: &Self, other: &Self) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &Binary<''_>) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &OwnedBinary) -> bool { /* ... */ }
    ```

- **Receiver**
- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
#### Struct `Binary`

An immutable smart-pointer to an Erlang binary.

See [module-level doc](index.html) for more information.

```rust
pub struct Binary<''a> {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn from_owned(owned: OwnedBinary, env: Env<''a>) -> Self { /* ... */ }
  ```
  Consumes `owned` and returns an immutable `Binary`.

- ```rust
  pub fn to_owned(self: &Self) -> Option<OwnedBinary> { /* ... */ }
  ```
  Copies `self`'s data into a new `OwnedBinary`.

- ```rust
  pub fn from_term(term: Term<''a>) -> Result<Self, Error> { /* ... */ }
  ```
  Creates a `Binary` from `term`.

- ```rust
  pub fn from_iolist(term: Term<''a>) -> Result<Self, Error> { /* ... */ }
  ```
  Creates a `Binary` from `term`.

- ```rust
  pub fn to_term<''b>(self: &Self, env: Env<''b>) -> Term<''b> { /* ... */ }
  ```
  Returns an Erlang term representation of `self`.

- ```rust
  pub fn as_slice(self: &Self) -> &''a [u8] { /* ... */ }
  ```
  Extracts a slice containing the entire binary.

- ```rust
  pub fn make_subbinary(self: &Self, offset: usize, length: usize) -> NifResult<Binary<''a>> { /* ... */ }
  ```
  Returns a new view into the same binary.

- ```rust
  pub unsafe fn make_subbinary_unchecked(self: &Self, offset: usize, length: usize) -> Binary<''a> { /* ... */ }
  ```
  Returns a new view into the same binary.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

  - ```rust
    fn borrow(self: &Self) -> &[u8] { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> Binary<''a> { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Copy**
- **Decoder**
  - ```rust
    fn decode(term: Term<''a>) -> Result<Self, Error> { /* ... */ }
    ```

- **Deref**
  - ```rust
    fn deref(self: &Self) -> &[u8] { /* ... */ }
    ```

- **Encoder**
  - ```rust
    fn encode<''b>(self: &Self, env: Env<''b>) -> Term<''b> { /* ... */ }
    ```

- **Eq**
- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

  - ```rust
    fn from(new_binary: NewBinary<''a>) -> Self { /* ... */ }
    ```

- **Hash**
  - ```rust
    fn hash<H: Hasher>(self: &Self, state: &mut H) { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **PartialEq**
  - ```rust
    fn eq(self: &Self, other: &Binary<''_>) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &Self) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &OwnedBinary) -> bool { /* ... */ }
    ```

- **Receiver**
- **RefUnwindSafe**
- **Send**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
#### Struct `NewBinary`

An newly-created, mutable Erlang binary.

See [module-level doc](index.html) for more information.

```rust
pub struct NewBinary<''a> {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn new(env: Env<''a>, size: usize) -> Self { /* ... */ }
  ```
  Allocates a new `NewBinary`

- ```rust
  pub fn as_slice(self: &Self) -> &[u8] { /* ... */ }
  ```
  Extracts a slice containing the entire binary.

- ```rust
  pub fn as_mut_slice(self: &mut Self) -> &mut [u8] { /* ... */ }
  ```
  Extracts a mutable slice of the entire binary.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Deref**
  - ```rust
    fn deref(self: &Self) -> &[u8] { /* ... */ }
    ```

- **DerefMut**
  - ```rust
    fn deref_mut(self: &mut Self) -> &mut [u8] { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

  - ```rust
    fn from(new_binary: NewBinary<''a>) -> Self { /* ... */ }
    ```

  - ```rust
    fn from(new_binary: NewBinary<''a>) -> Self { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **Receiver**
- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
## Module `big_int`

**Attributes:**

- `Other("#[<cfg>(feature = \"big_integer\")]")`

```rust
pub mod big_int { /* ... */ }
```

### Functions

#### Function `big_int_encoder_invalid_bytes`

```rust
pub fn big_int_encoder_invalid_bytes() -> $crate::types::atom::Atom { /* ... */ }
```

## Module `tuple`

```rust
pub mod tuple { /* ... */ }
```

### Functions

#### Function `get_tuple`

Convert an Erlang tuple to a Rust vector. (To convert to a Rust tuple, use `term.decode()`
instead.)

# Errors
`badarg` if `term` is not a tuple.

```rust
pub fn get_tuple(term: crate::Term<''_>) -> Result<Vec<crate::Term<''_>>, crate::Error> { /* ... */ }
```

#### Function `make_tuple`

Convert a vector of terms to an Erlang tuple. (To convert from a Rust tuple to an Erlang tuple,
use `Encoder` instead.)

```rust
pub fn make_tuple<''a>(env: crate::Env<''a>, terms: &[crate::Term<''_>]) -> crate::Term<''a> { /* ... */ }
```

## Module `i128`

```rust
pub mod i128 { /* ... */ }
```

## Module `path`

```rust
pub mod path { /* ... */ }
```

## Module `truthy`


A type to represent a truthy value.

In Elixir, a term which does not equal `:false` or `:nil` is considered to be
truthy. This does not cleanly map to Rust's `bool` type. To distinguish between
`bool` and a truthy value, the newtype `Truthy` can be used.


```rust
pub mod truthy { /* ... */ }
```

### Types

#### Struct `Truthy`

```rust
pub struct Truthy(/* private field */);
```

##### Fields

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `private` | *Private field* |

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Decoder**
  - ```rust
    fn decode(term: Term<''a>) -> NifResult<Truthy> { /* ... */ }
    ```

- **Encoder**
  - ```rust
    fn encode<''a>(self: &Self, env: Env<''a>) -> Term<''a> { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
## Module `elixir_struct`

Utilities used to create and access data specific to Elixir structs. Keep in mind that an
Elixir struct is a normal Erlang map, and functions from the `map` module can be used.

# Elixir struct transcoders
The compiler plugin has functionality for automatically generating a transcoder that can decode
and encode a Rust struct to an Elixir struct. To do so, simply annotate a struct with
`#[derive(NifStruct)]
`#[module = "Elixir.TheStructModule"]`.

```rust
pub mod elixir_struct { /* ... */ }
```

### Functions

#### Function `get_ex_struct_name`

```rust
pub fn get_ex_struct_name(map: crate::Term<''_>) -> crate::NifResult<super::atom::Atom> { /* ... */ }
```

#### Function `make_ex_struct`

```rust
pub fn make_ex_struct<''a>(env: crate::Env<''a>, struct_module: &str) -> crate::NifResult<crate::Term<''a>> { /* ... */ }
```

## Module `erlang_option`

```rust
pub mod erlang_option { /* ... */ }
```

### Types

#### Struct `ErlOption`

A wrapper type for [`Option<T>`][option] to provide Erlang style encoding. It
uses `undefined` atom instead of `nil` when the enclosing value is `None`.

Useful for interacting with Erlang libraries as `undefined` is commonly used in
Erlang to represent the absence of a value.

[option]: https://doc.rust-lang.org/stable/core/option/enum.Option.html

# Examples

`ErlOption<T>` provides methods to convert to/from `Option<T>`.

```rust
use rustler::ErlOption;

// Create new `ErlOption<i32>` values via convenient functions.
let _ = ErlOption::some(1); // Wraps `Some(1)`.
let _ = ErlOption::<i32>::none();

// Convert Option<i32> values to ErlOption<i32> values.
let _ = ErlOption::from(Some(2));
let _: ErlOption<_> = Some(3).into();
let _: ErlOption<i32> = None.into();

// Get a reference of enclosing Option<T> from an ErlOption<T>.
let _: &Option<i32> = ErlOption::some(4).as_ref();

// Get a mutable reference of enclosing Option<T> from an ErlOption<T>.
let _: &mut Option<i32> = ErlOption::some(5).as_mut();

// Convert an ErlOption<i32> value to an Option<i32> value.
let _: Option<i32> = ErlOption::some(6).into();

// Compare ErlOption<T> with Option<T>.
assert_eq!(ErlOption::some(7), Some(7));
assert!(ErlOption::some(8) > Some(7));

// Call Option<T>'s methods on an ErlOption<T> via Deref and DerefMut.
assert!(ErlOption::some(9).is_some());
assert_eq!(ErlOption::some(10).unwrap(), 10);
assert_eq!(ErlOption::some(12).map(|v| v + 1), ErlOption::some(13));
```


```rust
pub struct ErlOption<T>(/* private field */);
```

##### Fields

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `private` | *Private field* |

##### Implementations

###### Methods

- ```rust
  pub fn some(v: T) -> Self { /* ... */ }
  ```
  A convenience function to create an `ErlOption<T>` from a `Some(T)` value.

- ```rust
  pub fn none() -> Self { /* ... */ }
  ```
  A convenience function to create an `ErlOption<T>` enclosing the `None`

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **AsMut**
  - ```rust
    fn as_mut(self: &mut Self) -> &mut Option<T> { /* ... */ }
    ```

- **AsRef**
  - ```rust
    fn as_ref(self: &Self) -> &Option<T> { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> ErlOption<T> { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Copy**
- **Debug**
  - ```rust
    fn fmt(self: &Self, f: &mut $crate::fmt::Formatter<''_>) -> $crate::fmt::Result { /* ... */ }
    ```

- **Decoder**
  - ```rust
    fn decode(term: Term<''a>) -> NifResult<Self> { /* ... */ }
    ```

- **Default**
  - ```rust
    fn default() -> Self { /* ... */ }
    ```

- **Deref**
  - ```rust
    fn deref(self: &Self) -> &<Self as >::Target { /* ... */ }
    ```

- **DerefMut**
  - ```rust
    fn deref_mut(self: &mut Self) -> &mut <Self as >::Target { /* ... */ }
    ```

- **Encoder**
  - ```rust
    fn encode<''c>(self: &Self, env: Env<''c>) -> Term<''c> { /* ... */ }
    ```

- **Eq**
- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

  - ```rust
    fn from(v: Option<T>) -> Self { /* ... */ }
    ```

  - ```rust
    fn from(v: ErlOption<T>) -> Self { /* ... */ }
    ```

- **Hash**
  - ```rust
    fn hash<__H: $crate::hash::Hasher>(self: &Self, state: &mut __H) { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **Ord**
  - ```rust
    fn cmp(self: &Self, other: &ErlOption<T>) -> $crate::cmp::Ordering { /* ... */ }
    ```

- **PartialEq**
  - ```rust
    fn eq(self: &Self, other: &ErlOption<T>) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &Option<T>) -> bool { /* ... */ }
    ```

  - ```rust
    fn eq(self: &Self, other: &ErlOption<T>) -> bool { /* ... */ }
    ```

- **PartialOrd**
  - ```rust
    fn partial_cmp(self: &Self, other: &ErlOption<T>) -> $crate::option::Option<$crate::cmp::Ordering> { /* ... */ }
    ```

  - ```rust
    fn partial_cmp(self: &Self, other: &Option<T>) -> Option<std::cmp::Ordering> { /* ... */ }
    ```

  - ```rust
    fn partial_cmp(self: &Self, other: &ErlOption<T>) -> Option<std::cmp::Ordering> { /* ... */ }
    ```

- **Receiver**
- **RefUnwindSafe**
- **Send**
- **StructuralPartialEq**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
### Traits

#### Trait `Encoder`

```rust
pub trait Encoder {
    /* Associated items */
}
```

##### Required Items

###### Required Methods

- `encode`

##### Implementations

This trait is implemented for the following types:

- `Atom`
- `Binary<''_>`
- `num_bigint::BigInt`
- `Vec<T>` with <T>
- `[T]` with <T>
- `&[T]` with <T>
- `i32`
- `u32`
- `i64`
- `u64`
- `f64`
- `i8`
- `u8`
- `i16`
- `u16`
- `usize`
- `isize`
- `f32`
- `bool`
- `&str`
- `str`
- `String`
- `()`
- `(A)` with <A: Encoder>
- `(A, B)` with <A: Encoder, B: Encoder>
- `(A, B, C)` with <A: Encoder, B: Encoder, C: Encoder>
- `(A, B, C, D)` with <A: Encoder, B: Encoder, C: Encoder, D: Encoder>
- `(A, B, C, D, E)` with <A: Encoder, B: Encoder, C: Encoder, D: Encoder, E: Encoder>
- `(A, B, C, D, E, F)` with <A: Encoder, B: Encoder, C: Encoder, D: Encoder, E: Encoder, F: Encoder>
- `(A, B, C, D, E, F, G)` with <A: Encoder, B: Encoder, C: Encoder, D: Encoder, E: Encoder, F: Encoder, G: Encoder>
- `i128`
- `u128`
- `std::path::Path`
- `std::path::PathBuf`
- `Truthy`
- `ErlOption<T>` with <T>
- `crate::Term<''_>`
- `&T` with <T>
- `Box<T>` with <T>
- `Option<T>` with <T>
- `Result<T, E>` with <T, E>
- `std::collections::HashMap<K, V>` with <K, V>
- `ResourceArc<T>` with <T>
- `SerdeTerm<T>` with <T: serde::Serialize>

#### Trait `Decoder`

```rust
pub trait Decoder<''a>: Sized + ''a {
    /* Associated items */
}
```

> This trait is not object-safe and cannot be used in dynamic trait objects.

##### Required Items

###### Required Methods

- `decode`

##### Implementations

This trait is implemented for the following types:

- `Atom` with <''a>
- `Binary<''a>` with <''a>
- `num_bigint::BigInt` with <''a>
- `Vec<T>` with <''a, T>
- `std::ops::RangeInclusive<T>` with <''a, T>
- `i32` with <''a>
- `u32` with <''a>
- `i64` with <''a>
- `u64` with <''a>
- `i8` with <''a>
- `u8` with <''a>
- `i16` with <''a>
- `u16` with <''a>
- `usize` with <''a>
- `isize` with <''a>
- `f64`
- `f32`
- `bool` with <''a>
- `String` with <''a>
- `&''a str` with <''a>
- `()` with <''a>
- `(A)` with <''a, A: Decoder<''a>>
- `(A, B)` with <''a, A: Decoder<''a>, B: Decoder<''a>>
- `(A, B, C)` with <''a, A: Decoder<''a>, B: Decoder<''a>, C: Decoder<''a>>
- `(A, B, C, D)` with <''a, A: Decoder<''a>, B: Decoder<''a>, C: Decoder<''a>, D: Decoder<''a>>
- `(A, B, C, D, E)` with <''a, A: Decoder<''a>, B: Decoder<''a>, C: Decoder<''a>, D: Decoder<''a>, E: Decoder<''a>>
- `(A, B, C, D, E, F)` with <''a, A: Decoder<''a>, B: Decoder<''a>, C: Decoder<''a>, D: Decoder<''a>, E: Decoder<''a>, F: Decoder<''a>>
- `(A, B, C, D, E, F, G)` with <''a, A: Decoder<''a>, B: Decoder<''a>, C: Decoder<''a>, D: Decoder<''a>, E: Decoder<''a>, F: Decoder<''a>, G: Decoder<''a>>
- `i128` with <''a>
- `u128` with <''a>
- `&''a std::path::Path` with <''a>
- `std::path::PathBuf` with <''a>
- `Truthy` with <''a>
- `ErlOption<T>` with <''a, T>
- `crate::Term<''a>` with <''a>
- `Box<T>` with <''a, T>
- `Option<T>` with <''a, T>
- `Result<T, E>` with <''a, T, E>
- `std::collections::HashMap<K, V>` with <''a, K, V>
- `ResourceArc<T>` with <''a, T>
- `&''a T` with <''a, T>
- `SerdeTerm<T>` with <''a, T: serde::Deserialize<''a> + ''a>

### Re-exports

#### Re-export `Atom`

```rust
pub use crate::types::atom::Atom;
```

#### Re-export `Binary`

```rust
pub use crate::types::binary::Binary;
```

#### Re-export `NewBinary`

```rust
pub use crate::types::binary::NewBinary;
```

#### Re-export `OwnedBinary`

```rust
pub use crate::types::binary::OwnedBinary;
```

#### Re-export `BigInt`

**Attributes:**

- `Other("#[<cfg>(feature = \"big_integer\")]")`

```rust
pub use num_bigint::BigInt;
```

#### Re-export `ListIterator`

```rust
pub use crate::types::list::ListIterator;
```

#### Re-export `MapIterator`

```rust
pub use self::map::MapIterator;
```

#### Re-export `LocalPid`

```rust
pub use self::local_pid::LocalPid;
```

#### Re-export `Reference`

```rust
pub use self::reference::Reference;
```

#### Re-export `ErlOption`

```rust
pub use self::erlang_option::ErlOption;
```

## Module `schedule`

```rust
pub mod schedule { /* ... */ }
```

### Types

#### Enum `SchedulerFlags`

```rust
pub enum SchedulerFlags {
    Normal = { _ },
    DirtyCpu = { _ },
    DirtyIo = { _ },
}
```

##### Variants

###### `Normal`

Discriminant: `{ _ }`

Discriminant value: `0`

###### `DirtyCpu`

Discriminant: `{ _ }`

Discriminant value: `1`

###### `DirtyIo`

Discriminant: `{ _ }`

Discriminant value: `2`

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
### Functions

#### Function `consume_timeslice`

```rust
pub fn consume_timeslice(env: crate::Env<''_>, percent: i32) -> bool { /* ... */ }
```

## Module `env`

```rust
pub mod env { /* ... */ }
```

### Types

#### Struct `Env`

On each NIF call, a Env is passed in. The Env is used for most operations that involve
communicating with the BEAM, like decoding and encoding terms.

There is no way to allocate a Env at the moment, but this may be possible in the future.

```rust
pub struct Env<''a> {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn pid(self: Self) -> LocalPid { /* ... */ }
  ```
  Return the calling process's pid.

- ```rust
  pub fn is_process_alive(self: Self, pid: LocalPid) -> bool { /* ... */ }
  ```
  Checks whether the given process is alive

- ```rust
  pub fn make_ref(self: Self) -> Reference<''a> { /* ... */ }
  ```
  Create a new reference in this environment

- ```rust
  pub fn monitor<T: Resource>(self: &Self, resource: &ResourceArc<T>, pid: &LocalPid) -> Option<Monitor> { /* ... */ }
  ```

- ```rust
  pub fn demonitor<T: Resource>(self: &Self, resource: &ResourceArc<T>, mon: &Monitor) -> bool { /* ... */ }
  ```

- ```rust
  pub unsafe fn dynamic_resource_call(self: Self, module: crate::Atom, name: crate::Atom, resource: Term<''a>, call_data: *mut c_void) -> Result<(), super::DynamicResourceCallError> { /* ... */ }
  ```

- ```rust
  pub fn register<T: Resource>(self: &Self) -> Result<(), ResourceInitError> { /* ... */ }
  ```
  Register a resource type, see `Registration::register`.

- ```rust
  pub unsafe fn new<T>(_lifetime_marker: &''a T, env: NIF_ENV) -> Env<''a> { /* ... */ }
  ```
  Create a new Env. For the `_lifetime_marker` argument, pass a

- ```rust
  pub fn as_c_arg(self: Self) -> NIF_ENV { /* ... */ }
  ```

- ```rust
  pub fn error_tuple</* synthetic */ impl Encoder: Encoder>(self: Self, reason: impl Encoder) -> Term<''a> { /* ... */ }
  ```
  Convenience method for building a tuple `{error, Reason}`.

- ```rust
  pub fn send</* synthetic */ impl Encoder: Encoder>(self: Self, pid: &LocalPid, message: impl Encoder) -> Result<(), SendError> { /* ... */ }
  ```
  Send a message to a process.

- ```rust
  pub fn whereis_pid</* synthetic */ impl Encoder: Encoder>(self: Self, name_or_pid: impl Encoder) -> Option<LocalPid> { /* ... */ }
  ```
  Attempts to find the PID of a process registered by `name_or_pid`

- ```rust
  pub fn binary_to_term(self: Self, data: &[u8]) -> Option<(Term<''a>, usize)> { /* ... */ }
  ```
  Decodes binary data to a term.

- ```rust
  pub unsafe fn binary_to_term_trusted(self: Self, data: &[u8]) -> Option<(Term<''a>, usize)> { /* ... */ }
  ```
  Like `binary_to_term`, but can only be called on valid

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> Env<''a> { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Copy**
- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

  - ```rust
    fn from(env: Env<''a>) -> Serializer<''a> { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **PartialEq**
  - ```rust
    fn eq(self: &Self, other: &Env<''b>) -> bool { /* ... */ }
    ```

- **RefUnwindSafe**
- **Send**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
#### Struct `SendError`

Indicates that a send failed, see
[enif\_send](https://www.erlang.org/doc/man/erl_nif.html#enif_send).

```rust
pub struct SendError;
```

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> SendError { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Copy**
- **Debug**
  - ```rust
    fn fmt(self: &Self, f: &mut $crate::fmt::Formatter<''_>) -> $crate::fmt::Result { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
#### Struct `OwnedEnv`

A process-independent environment, a place where Erlang terms can be created outside of a NIF
call.

Rust code can use an owned environment to build a message and send it to an
Erlang process.

    use rustler::env::OwnedEnv;
    use rustler::types::LocalPid;
    use rustler::Encoder;

    fn send_string_to_pid(data: &str, pid: &LocalPid) {
        let mut msg_env = OwnedEnv::new();
        let _ = msg_env.send_and_clear(pid, |env| data.encode(env));
    }

There's no way to run Erlang code in an `OwnedEnv`. It's not a process. It's just a workspace
for building terms.

```rust
pub struct OwnedEnv {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn monitor<T: Resource>(self: &Self, resource: &ResourceArc<T>, pid: &LocalPid) -> Option<Monitor> { /* ... */ }
  ```

- ```rust
  pub fn demonitor<T: Resource>(self: &Self, resource: &ResourceArc<T>, mon: &Monitor) -> bool { /* ... */ }
  ```

- ```rust
  pub fn new() -> OwnedEnv { /* ... */ }
  ```
  Allocates a new process-independent environment.

- ```rust
  pub fn run<''a, F, R>(self: &Self, closure: F) -> R
where
    F: FnOnce(Env<''a>) -> R { /* ... */ }
  ```
  Run some code in this environment.

- ```rust
  pub fn send_and_clear<''a, F, T>(self: &mut Self, recipient: &LocalPid, closure: F) -> Result<(), SendError>
where
    F: FnOnce(Env<''a>) -> T,
    T: Encoder { /* ... */ }
  ```
  Send a message from a Rust thread to an Erlang process.

- ```rust
  pub fn clear(self: &mut Self) { /* ... */ }
  ```
  Free all terms in this environment and clear it for reuse.

- ```rust
  pub fn save</* synthetic */ impl Encoder: Encoder>(self: &Self, term: impl Encoder) -> SavedTerm { /* ... */ }
  ```
  Save a term for use in a later call to `.run()` or `.send()`.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Default**
  - ```rust
    fn default() -> Self { /* ... */ }
    ```

- **Drop**
  - ```rust
    fn drop(self: &mut Self) { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
#### Struct `SavedTerm`

A term that was created in an `OwnedEnv` and saved for later use.

These are created by calling `OwnedEnv::save()`. See that method's documentation for an
example.

```rust
pub struct SavedTerm {
    // Some fields omitted
}
```

##### Fields

| Name | Type | Documentation |
|------|------|---------------|
| *private fields* | ... | *Some fields have been omitted* |

##### Implementations

###### Methods

- ```rust
  pub fn load<''a>(self: &Self, env: Env<''a>) -> Term<''a> { /* ... */ }
  ```
  Load this saved term back into its environment.

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Clone**
  - ```rust
    fn clone(self: &Self) -> SavedTerm { /* ... */ }
    ```

- **CloneToUninit**
  - ```rust
    unsafe fn clone_to_uninit(self: &Self, dest: *mut u8) { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **ToOwned**
  - ```rust
    fn to_owned(self: &Self) -> T { /* ... */ }
    ```

  - ```rust
    fn clone_into(self: &Self, target: &mut T) { /* ... */ }
    ```

- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
## Module `thread`

```rust
pub mod thread { /* ... */ }
```

### Types

#### Struct `ThreadSpawner`

A `JobSpawner` that uses a separate system thread for each job.

```rust
pub struct ThreadSpawner;
```

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **JobSpawner**
  - ```rust
    fn spawn<F: FnOnce() + Send + panic::UnwindSafe + ''static>(job: F) { /* ... */ }
    ```
    This delegates to `std::thread::spawn()`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
### Traits

#### Trait `JobSpawner`

A `JobSpawner` is a value that can run Rust code on non-Erlang system threads.
Abstracts away details of thread management for `spawn()`.

Note: Implementations of `spawn()` must call the closure on a thread that is **not** managed by
the Erlang VM's scheduler. Otherwise, `rustler::thread::spawn()` would try to send a message
from an `OwnedEnv` on an Erlang thread, which would panic.

```rust
pub trait JobSpawner {
    /* Associated items */
}
```

> This trait is not object-safe and cannot be used in dynamic trait objects.

##### Required Items

###### Required Methods

- `spawn`: Run the given closure on another thread.

##### Implementations

This trait is implemented for the following types:

- `ThreadSpawner`

### Functions

#### Function `spawn`

Implements threaded NIFs.

This spawns a thread that calls the given closure `thread_fn`. When the closure returns, the
thread sends its return value back to the calling process.  If the closure panics, an `{error,
Reason}` tuple is sent instead.

Note that the thread creates a new `Env` and passes it to the closure, so the closure
runs under a separate environment, not under `env`.


```rust
pub fn spawn<''a, S, F>(env: crate::Env<''a>, thread_fn: F)
where
    F: for<''b> FnOnce(crate::Env<''b>) -> crate::Term<''b> + Send + panic::UnwindSafe + ''static,
    S: JobSpawner { /* ... */ }
```

#### Function `is_scheduler_thread`

Check if the currently running thread is managed by the ERTS.

This is relevant for (e.g.) `enif_send` or `enif_monitor_process` as

```rust
pub fn is_scheduler_thread() -> bool { /* ... */ }
```

## Module `error`

```rust
pub mod error { /* ... */ }
```

### Types

#### Enum `Error`

Represents usual errors that can happen in a nif. This enables you
to return an error from anywhere, even places where you don't have
an Env available.

```rust
pub enum Error {
    BadArg,
    Atom(&''static str),
    RaiseAtom(&''static str),
    RaiseTerm(Box<dyn Encoder>),
    Term(Box<dyn Encoder>),
}
```

##### Variants

###### `BadArg`

Returned when the NIF has been called with the wrong number or type of
arguments.

###### `Atom`

Encodes the string into an atom and returns it from the NIF.

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `&''static str` |  |

###### `RaiseAtom`

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `&''static str` |  |

###### `RaiseTerm`

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `Box<dyn Encoder>` |  |

###### `Term`

Encodes an arbitrary Boxed Encoder and returns `{:error, term}` from
the NIF. Very useful for returning descriptive, context-full errors.

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `Box<dyn Encoder>` |  |

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Debug**
  - ```rust
    fn fmt(self: &Self, fmt: &mut fmt::Formatter<''_>) -> Result<(), fmt::Error> { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

  - ```rust
    fn from(err: Error) -> NifError { /* ... */ }
    ```

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
## Module `return`

```rust
pub mod return { /* ... */ }
```

### Types

#### Enum `Return`

```rust
pub enum Return<''a> {
    Term(crate::Term<''a>),
    Error(crate::error::Error),
}
```

##### Variants

###### `Term`

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `crate::Term<''a>` |  |

###### `Error`

Fields:

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `crate::error::Error` |  |

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
## Module `serde`

**Attributes:**

- `Other("#[<cfg>(feature = \"serde\")]")`

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
| <sup>[3](#atom)</sup> newtype variant (any `Ok` and `Err` tagged enum) | `enum R<T, E> { Ok(T), Err(E) }` | `{:ok, T}` or `{:error, E}` | `["ok", T]` or `["error", E]` |
| seq | `Vec<T>` | `[T,]` | `[T,]` |
| tuple | `(u8,)` | `{u8,}` | `[u8,]` |
| <sup>[3](#atom)</sup> tuple struct | `struct Rgb(u8, u8, u8)` | `{:Rgb, u8, u8, u8}` | `["Rgb", u8, u8, u8]` |
| <sup>[3](#atom)</sup> tuple variant | `E::T` in `enum E { T(u8, u8) }` | `{:T, u8, u8}` | `["T", u8, u8]` |
| <sup>[1](#todo)</sup> map | `HashMap<K, V>` | `%{}` | `%{}` |
| <sup>[3](#atom)</sup> struct | `struct Rgb { r: u8, g: u8, b: u8 }` | `%Rgb{ r: u8, g: u8, b: u8 }` | `%{"r" => u8, "g" => u8, "b" => u8}` |
| <sup>[3](#atom)</sup> struct variant | `E::S` in `enum E { Rgb { r: u8, g: u8, b: u8 } }` | `%Rgb{ r: u8, g: u8, b: u8 }` | `%{"r" => u8, "g" => u8, "b" => u8}` |

<a name="todo">1</a>: API still being decided / implemented.

<a name="atom">2</a>: When serializing unknown input to terms, atoms will not be created and will instead be replaced with Elixir bitstrings. Therefore "records" will be tuples (`{bitstring, ...}`) and "structs" will be maps containing `%{:__struct__ => bitstring}`. The unfortunate consequence of this is that `deserialize_any` will lack the necessary information needed deserialize many terms without type hints, such as `structs`, `enums` and `enum variants`, and `tuples`.

```rust
pub mod serde { /* ... */ }
```

### Modules

## Module `atoms`

Constants and utilities for conversion between Rust string-likes and Elixir atoms.

```rust
pub mod atoms { /* ... */ }
```

### Functions

#### Function `nil`

```rust
pub fn nil() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `ok`

```rust
pub fn ok() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `error`

```rust
pub fn error() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `true_`

```rust
pub fn true_() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `false_`

```rust
pub fn false_() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `undefined`

```rust
pub fn undefined() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `nan`

```rust
pub fn nan() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `inf`

```rust
pub fn inf() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `neg_inf`

```rust
pub fn neg_inf() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `__struct__`

```rust
pub fn __struct__() -> $crate::types::atom::Atom { /* ... */ }
```

#### Function `str_to_term`

Attempts to create an atom term from the provided string (if the atom already exists in the atom table). If not, returns a string term.

```rust
pub fn str_to_term<''a>(env: &crate::Env<''a>, string: &str) -> Result<crate::Term<''a>, crate::serde::Error> { /* ... */ }
```

#### Function `term_to_string`

Attempts to create a `String` from the term.

```rust
pub fn term_to_string(term: &crate::Term<''_>) -> Result<String, crate::serde::Error> { /* ... */ }
```

### Types

#### Struct `SerdeTerm`

Wrapper type to en- and decode serde terms

If the wrapped type implements `serde::Serialize`, `SerdeTerm<T>` implements
`Encoder` and will thus be transparently converted *to* a BEAM object.

If the wrapped type implements `serde::Deserialize`, it implements `Decoder`
and can thus be transparently converted *from* a BEAM object.

```rust
pub struct SerdeTerm<T>(pub T);
```

##### Fields

| Index | Type | Documentation |
|-------|------|---------------|
| 0 | `T` |  |

##### Implementations

###### Trait Implementations

- **Any**
  - ```rust
    fn type_id(self: &Self) -> TypeId { /* ... */ }
    ```

- **Borrow**
  - ```rust
    fn borrow(self: &Self) -> &T { /* ... */ }
    ```

- **BorrowMut**
  - ```rust
    fn borrow_mut(self: &mut Self) -> &mut T { /* ... */ }
    ```

- **Decoder**
  - ```rust
    fn decode(term: Term<''a>) -> NifResult<Self> { /* ... */ }
    ```

- **Encoder**
  - ```rust
    fn encode<''a>(self: &Self, env: Env<''a>) -> Term<''a> { /* ... */ }
    ```

- **Freeze**
- **From**
  - ```rust
    fn from(t: T) -> T { /* ... */ }
    ```
    Returns the argument unchanged.

- **Into**
  - ```rust
    fn into(self: Self) -> U { /* ... */ }
    ```
    Calls `U::from(self)`.

- **RefUnwindSafe**
- **Send**
- **Sync**
- **TryFrom**
  - ```rust
    fn try_from(value: U) -> Result<T, <T as TryFrom<U>>::Error> { /* ... */ }
    ```

- **TryInto**
  - ```rust
    fn try_into(self: Self) -> Result<U, <U as TryFrom<T>>::Error> { /* ... */ }
    ```

- **Unpin**
- **UnwindSafe**
### Re-exports

#### Re-export `from_term`

```rust
pub use de::from_term;
```

#### Re-export `Deserializer`

```rust
pub use de::Deserializer;
```

#### Re-export `Error`

```rust
pub use error::Error;
```

#### Re-export `to_term`

```rust
pub use ser::to_term;
```

#### Re-export `Serializer`

```rust
pub use ser::Serializer;
```

## Module `sys`

**Attributes:**

- `Other("#[allow(non_camel_case_types)]")`
- `Other("#[allow(clippy::missing_safety_doc)]")`

Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).

```rust
pub mod sys { /* ... */ }
```

### Re-exports

#### Re-export `self::functions::*`

```rust
pub use self::functions::*;
```

#### Re-export `self::types::*`

```rust
pub use self::types::*;
```

## Types

### Type Alias `NifResult`

```rust
pub type NifResult<T> = Result<T, Error>;
```

## Macros

### Macro `atoms`

**Attributes:**

- `MacroExport`

Macro for defining Rust functions that return Erlang atoms.

For example, this code:

    mod my_atoms {
        rustler::atoms! {
            jpeg,
        }
    }
    # fn main() {}

defines a public function `my_atoms::jpeg()` that returns the `Atom` for the `jpeg` atom.

Multiple atoms can be defined. Each one can have its own doc comment and other attributes.

    rustler::atoms! {
        /// The `jpeg` atom.
        jpeg,

        /// The `png` atom.
        png,

        #[allow(non_snake_case)]
        WebP,
    }
    # fn main() {}

When you need an atom that's not a legal Rust function name, write `NAME = "ATOM"`, like
this:

    rustler::atoms! {
        /// The `mod` atom. The function isn't called `mod` because that's
        /// a Rust keyword.
        mod_atom = "mod",

        /// The atom `'hello world'`. Obviously this function can't be
        /// called `hello world` because there's a space in it.
        hello_world = "hello world",
    }
    # fn main() {}

# Performance

These functions are faster than `get_atom` and `get_atom_init`. The first time you call one, it
creates atoms for all its sibling functions and caches them, so that all later calls are fast.
The only overhead is checking that the atoms have been created (an atomic integer read).


```rust
pub macro_rules! atoms {
    /* macro_rules! atoms {
    {
        $(
            $( #[$attr:meta] )*
            $name:ident $( = $str:expr )?
        ),*$(,)?
    } => { ... };
    { @internal_make_atom($env:ident, $name:ident) } => { ... };
    { @internal_make_atom($env:ident, $name:ident = $str:expr) } => { ... };
} */
}
```

### Macro `resource`

**Attributes:**

- `MacroExport`

Deprecated resource registration method

This macro will create a local `impl Resource` for the passed type and is thus incompatible
with upcoming Rust language changes. Please implement the `Resource` trait directly and
register it either using the `resource_impl` attribute or using the `Env::register` method:
```ignore
fn on_load(env: Env) -> bool {
    env.register::<ResourceType>().is_ok()
}
```

```rust
pub macro_rules! resource {
    /* macro_rules! resource {
    ($struct_name:ty, $env: ident) => { ... };
} */
}
```

### Macro `enif_make_tuple`

**Attributes:**

- `MacroExport`

```rust
pub macro_rules! enif_make_tuple {
    /* macro_rules! enif_make_tuple {
    ( $( $arg:expr ),* ) => { ... };
    ( $( $arg:expr ),+, ) => { ... };
} */
}
```

### Macro `enif_make_list`

**Attributes:**

- `MacroExport`

```rust
pub macro_rules! enif_make_list {
    /* macro_rules! enif_make_list {
    ( $( $arg:expr ),* ) => { ... };
    ( $( $arg:expr ),+, ) => { ... };
} */
}
```

### Macro `enif_fprintf`

**Attributes:**

- `MacroExport`

```rust
pub macro_rules! enif_fprintf {
    /* macro_rules! enif_fprintf {
    ( $( $arg:expr ),* ) => { ... };
    ( $( $arg:expr ),+, ) => { ... };
} */
}
```

### Macro `enif_snprintf`

**Attributes:**

- `MacroExport`

```rust
pub macro_rules! enif_snprintf {
    /* macro_rules! enif_snprintf {
    ( $( $arg:expr ),* ) => { ... };
    ( $( $arg:expr ),+, ) => { ... };
} */
}
```

## Re-exports

### Re-export `EnifAllocator`

```rust
pub use crate::alloc::EnifAllocator;
```

### Re-export `Term`

```rust
pub use crate::term::Term;
```

### Re-export `Atom`

```rust
pub use crate::types::Atom;
```

### Re-export `Binary`

```rust
pub use crate::types::Binary;
```

### Re-export `Decoder`

```rust
pub use crate::types::Decoder;
```

### Re-export `Encoder`

```rust
pub use crate::types::Encoder;
```

### Re-export `ErlOption`

```rust
pub use crate::types::ErlOption;
```

### Re-export `ListIterator`

```rust
pub use crate::types::ListIterator;
```

### Re-export `LocalPid`

```rust
pub use crate::types::LocalPid;
```

### Re-export `MapIterator`

```rust
pub use crate::types::MapIterator;
```

### Re-export `NewBinary`

```rust
pub use crate::types::NewBinary;
```

### Re-export `OwnedBinary`

```rust
pub use crate::types::OwnedBinary;
```

### Re-export `Reference`

```rust
pub use crate::types::Reference;
```

### Re-export `BigInt`

**Attributes:**

- `Other("#[<cfg>(feature = \"big_integer\")]")`

```rust
pub use crate::types::BigInt;
```

### Re-export `Monitor`

```rust
pub use crate::resource::Monitor;
```

### Re-export `Resource`

```rust
pub use crate::resource::Resource;
```

### Re-export `ResourceArc`

```rust
pub use crate::resource::ResourceArc;
```

### Re-export `ResourceInitError`

```rust
pub use crate::resource::ResourceInitError;
```

### Re-export `TermType`

```rust
pub use crate::dynamic::TermType;
```

### Re-export `SchedulerFlags`

```rust
pub use crate::schedule::SchedulerFlags;
```

### Re-export `Env`

```rust
pub use crate::env::Env;
```

### Re-export `OwnedEnv`

```rust
pub use crate::env::OwnedEnv;
```

### Re-export `spawn`

```rust
pub use crate::thread::spawn;
```

### Re-export `JobSpawner`

```rust
pub use crate::thread::JobSpawner;
```

### Re-export `ThreadSpawner`

```rust
pub use crate::thread::ThreadSpawner;
```

### Re-export `Error`

```rust
pub use crate::error::Error;
```

### Re-export `Return`

```rust
pub use crate::return::Return;
```

### Re-export `Nif`

```rust
pub use nif::Nif;
```

### Re-export `init`

```rust
pub use rustler_codegen::init;
```

### Re-export `nif`

```rust
pub use rustler_codegen::nif;
```

### Re-export `resource_impl`

```rust
pub use rustler_codegen::resource_impl;
```

### Re-export `NifException`

```rust
pub use rustler_codegen::NifException;
```

### Re-export `NifMap`

```rust
pub use rustler_codegen::NifMap;
```

### Re-export `NifRecord`

```rust
pub use rustler_codegen::NifRecord;
```

### Re-export `NifStruct`

```rust
pub use rustler_codegen::NifStruct;
```

### Re-export `NifTaggedEnum`

```rust
pub use rustler_codegen::NifTaggedEnum;
```

### Re-export `NifTuple`

```rust
pub use rustler_codegen::NifTuple;
```

### Re-export `NifUnitEnum`

```rust
pub use rustler_codegen::NifUnitEnum;
```

### Re-export `NifUntaggedEnum`

```rust
pub use rustler_codegen::NifUntaggedEnum;
```

### Re-export `SerdeTerm`

**Attributes:**

- `Other("#[<cfg>(feature = \"serde\")]")`

```rust
pub use crate::serde::SerdeTerm;
```

