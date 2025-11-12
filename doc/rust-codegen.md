# Crate Documentation

**Version:** 0.37.1

**Format Version:** 56

# Module `rustler_codegen`

## Macros

### Procedural Macro `init`

**Attributes:**

- `Other("#[attr = ProcMacro]")`

Initialise the Native Implemented Function (NIF) environment
and register NIF functions in an Elixir module.

```ignore
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn sub(a: i64, b: i64) -> i64 {
    a - b
}

#[rustler::nif]
fn mul(a: i64, b: i64) -> i64 {
    a * b
}

#[rustler::nif]
fn div(a: i64, b: i64) -> i64 {
    a / b
}

fn load(env: Env, _term: Term) -> bool {
    true
}

rustler::init!("Elixir.Math", load = load);
```

```rust
pub #[proc_macro]
pub fn init(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `nif`

**Attributes:**

- `Other("#[attr = ProcMacroAttribute]")`

Wrap a function in a Native Implemented Function (NIF) implementation,
so that it can be called from Elixir,
with all encoding and decoding steps done automatically.

```ignore
#[nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}
```

For functions that may take some time to return - let's say more than 1 millisecond - it is
recommended to use the `schedule` flag. This tells the BEAM to allocate that NIF call
to a special scheduler. These special schedulers are called "dirty" schedulers.

We can have two types of "lengthy work" functions: those that are CPU intensive
and those that are IO intensive. They should be flagged with "DirtyCpu" and "DirtyIo",
respectively.

See: <https://www.erlang.org/doc/man/erl_nif.html#lengthy_work>

```ignore
#[nif(schedule = "DirtyCpu")]
pub fn my_lengthy_work() -> i64 {
    let duration = Duration::from_millis(100);
    std::thread::sleep(duration);

    42
}
```

```rust
pub #[proc_macro_attribute]
pub fn nif(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifStruct`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifStruct\", helper_attrs: [\"module\",\n\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between an Elixir struct and a Rust struct.

For example, annotate the following Rust struct:

```ignore
#[derive(Debug, NifStruct)]
#[module = "AddStruct"]
struct AddStruct {
   lhs: i32,
   rhs: i32,
}
```

Write the following corresponding Elixir struct definition:

```elixir
defmodule AddStruct do
  defstruct lhs: 0, rhs: 0
end
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that you can use the Elixir struct definition for it.

```rust
pub #[proc_macro_derive]
// Helpers: #[module], #[rustler]
pub fn NifStruct(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifException`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifException\", helper_attrs: [\"module\",\n\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between an Elixir exception and a Rust struct.

For example, annotate the following struct:

```ignore
#[derive(Debug, NifException)]
#[module = "AddException"]
pub struct AddException {
    message: String,
}
```

Write the corresponding Elixir exception definition:

```elixir
defmodule AddException do
  defexception message: ""
end
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that you can use the Elixir exception definition for it.

```rust
pub #[proc_macro_derive]
// Helpers: #[module], #[rustler]
pub fn NifException(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifMap`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifMap\", helper_attrs: [\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between Rust struct and an Elixir map.

For example, annotate the following struct:

```ignore
#[derive(NifMap)]
struct AddMap {
    lhs: i32,
    rhs: i32,
}
```

Create a value of that type:

```ignore
let value = AddMap { lhs: 33, rhs: 21 };
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that encoding `value` would result in an elixir
map with two elements like:

```elixir
%{lhs: 33, rhs: 21}
```

And vice versa, decoding this map would result in `value`.

```rust
pub #[proc_macro_derive]
// Helpers: #[rustler]
pub fn NifMap(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifTuple`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifTuple\", helper_attrs: [\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between a Rust struct and an Elixir tuple.

For example, annotate the following struct:

```ignore
#[derive(NifTuple)]
struct AddTuple {
    lhs: i32,
    rhs: i32,
}
```

Create a value of that type:

```ignore
let value = AddMap { lhs: 33, rhs: 21 };
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that encoding `value` would result in an elixir
tuple with two elements like:

```elixir
{33, 21}
```

And vice versa, decoding this map would result in `value`.

The size of the tuple will depend on the number of elements in the struct.

```rust
pub #[proc_macro_derive]
// Helpers: #[rustler]
pub fn NifTuple(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifRecord`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifRecord\", helper_attrs: [\"tag\",\n\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between a Rust struct and an Elixir record.

For example, annotate the following struct:

```ignore
#[derive(Debug, NifRecord)]
#[tag = "record"]
struct AddRecord {
   lhs: i32,
   rhs: i32,
}
```

Create a value of that type:

```ignore
let value = AddRecord { lhs: 33, rhs: 21 };
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that `value` would be encoded into the following elixir value:

```elixir
{:record, 33, 21}
```

If you supply the following matching Elixir record definition:

```elixir
defmodule AddRecord do
  import Record
  defrecord :record, [lhs: 1, rhs: 2]
end
```

Then you can use record functions such as `AddRecord.record/0`, `AddRecord.record/1`, `AddRecord.record/2`,
to work with the encoded data,
and to create data that can be decoded back into your Rust struct.

```rust
pub #[proc_macro_derive]
// Helpers: #[tag], #[rustler]
pub fn NifRecord(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifUnitEnum`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifUnitEnum\",\nhelper_attrs: [\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between an enum and a union of elixir atoms.

For example:

```ignore
#[derive(NifUnitEnum)]
enum UnitEnum {
   FooBar,
   Baz,
}
```

Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
such that `FooBar` is encoded to, and decoded from, `:foo_bar`.
- The variant name is translated from camel case to snake case for the atom name.
- Each constructor is required not to have arguments, i.e. to be of unit type.

An example usage in Rust and Elixir would look like the following.

```ignore
#[rustler::nif]
pub fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
    unit_enum
}
```

(We are leaving out some boiler plate code to connect the rust code to elixir functions.)

```elixir
test "unit enum transcoder" do
  assert :foo_bar == unit_enum_echo(:foo_bar)
  assert :baz == unit_enum_echo(:baz)
  assert :invalid_variant == unit_enum_echo(:somethingelse)
end
```

Note that the `:invalid_variant` atom is returned if the user tries to encode something
that isn't in the Rust enum.

```rust
pub #[proc_macro_derive]
// Helpers: #[rustler]
pub fn NifUnitEnum(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifTaggedEnum`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifTaggedEnum\",\nhelper_attrs: [\"rustler\"]}]")`

Implementation of the `NifTaggedEnum` macro that lets the user annotate an enum that will
generate elixir values when encoded. This can be used for any rust enums and will generate
three types of values based on the kind of the enum. For example from the test code:

```ignore
#[derive(NifTaggedEnum)]
enum TaggedEnum {
    Foo,
    Bar(String),
    Baz{ a: i32, b: i32 },
}

pub fn tagged_enum_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let tagged_enum: TaggedEnum = args[0].decode()?;
    Ok(tagged_enum.encode(env))
}
```

This can be used from elixir in the following manner.

```elixir
test "tagged enum transcoder" do
  assert :foo == RustlerTest.tagged_enum_echo(:foo)
  assert {:bar, "Hello"} == RustlerTest.tagged_enum_echo(:bar, "Hello")
  assert {:baz, %{a: 33, b: 21}} == RustlerTest.tagged_enum_echo({:baz, %{a: 33, b: 21}})
end
```

```rust
pub #[proc_macro_derive]
// Helpers: #[rustler]
pub fn NifTaggedEnum(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `NifUntaggedEnum`

**Attributes:**

- `Other("#[attr = ProcMacroDerive {trait_name: \"NifUntaggedEnum\",\nhelper_attrs: [\"rustler\"]}]")`

Derives implementations for the `Encoder` and `Decoder` traits
which convert between a Rust enum and a union of Elixir types.

This can be used for Rust enums that contain several constructors containing different types of data,
each implementing the `Encoder` and `Decoder` traits.
An enum value will be encoded based on the constructor used,
and an Elixir value will be decoded based on the value.

For example from the test code:

```ignore
#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
    Baz(AddStruct),
}

#[rustler::nif]
pub fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
    untagged_enum
}
```

Adding boiler plate code to connect Rust code to elixir functions,
this can be used from elixir in the following manner.

```elixir
test "untagged enum transcoder" do
  assert 123 == untagged_enum_echo(123)
  assert "Hello" == untagged_enum_echo("Hello")
  assert %AddStruct{lhs: 45, rhs: 123} = untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})
  assert :invalid_variant == untagged_enum_echo([1,2,3,4])
end
```

Note that the type of elixir return is dependent on the data in the enum and the actual enum
type is lost in the translation because Elixir has no such concept.

```rust
pub #[proc_macro_derive]
// Helpers: #[rustler]
pub fn NifUntaggedEnum(/* ... */) -> /* ... */ {
    /* ... */
}
```

### Procedural Macro `resource_impl`

**Attributes:**

- `Other("#[attr = ProcMacroAttribute]")`

Helper attribute for `Resource` implementations

When an `impl Resource for Type` block is annotated with this attribute, it will automatically
set the `IMPLEMENTS_...` associated constants for all implemented callback methods. Thus,
instead of

```ignore
struct ResourceType {}

impl Resource for ResourceType
{
    const IMPLEMENTS_DESTRUCTOR: bool = true;

    fn destructor(...) { ... }
}
```
it is enough to provide the implementation:
```ignore
#[rustler::resource_impl]
impl Resource for ResourceType
{
    fn destructor(...) { ... }
}
```

The resource type is also automatically registered by default, it does not have to be manually
registered in a `load` callback. The automatic registration can be disabled with the `register`
parameter:

```ignore
#[rustler::resource_impl]
impl Resource for ResourceType
{
    ...
}

// no load callback necessary
```

If registration is disabled, the resource type has to be registered manually. It is not
possible to use the old `resource!` macro for this, as that injects another `impl Resource`
block.
```ignore
#[rustler::resource_impl(register = false)]
impl Resource for ResourceType
{
    ...
}

pub fn on_load(env: Env) -> bool {
    env.register::<ResourceType>().is_ok()
}
```

```rust
pub #[proc_macro_attribute]
pub fn resource_impl(/* ... */) -> /* ... */ {
    /* ... */
}
```

