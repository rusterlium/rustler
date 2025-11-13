#![deny(warnings)]
#![recursion_limit = "128"]

use proc_macro::TokenStream;

mod context;
mod encode_decode_templates;
mod ex_struct;
mod init;
mod map;
mod nif;
mod record;
mod resource_impl;
mod tagged_enum;
mod tuple;
mod unit_enum;
mod untagged_enum;

#[derive(Debug)]
enum RustlerAttr {
    Encode,
    Decode,
    Module(String),
    Tag(String),
}

/// Initialise the Native Implemented Function (NIF) environment
/// and register NIF functions in an Elixir module.
///
/// ```ignore
/// #[rustler::nif]
/// fn add(a: i64, b: i64) -> i64 {
///     a + b
/// }
///
/// #[rustler::nif]
/// fn sub(a: i64, b: i64) -> i64 {
///     a - b
/// }
///
/// #[rustler::nif]
/// fn mul(a: i64, b: i64) -> i64 {
///     a * b
/// }
///
/// #[rustler::nif]
/// fn div(a: i64, b: i64) -> i64 {
///     a / b
/// }
///
/// fn load(env: Env, _term: Term) -> bool {
///     true
/// }
///
/// rustler::init!("Elixir.Math", load = load);
/// ```
#[proc_macro]
pub fn init(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as init::InitMacroInput);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}

/// Wrap a function in a Native Implemented Function (NIF) implementation,
/// so that it can be called from Elixir,
/// with all encoding and decoding steps done automatically.
///
/// ```ignore
/// #[nif]
/// fn add(a: i64, b: i64) -> i64 {
///     a + b
/// }
/// ```
///
/// For functions that may take some time to return - let's say more than 1 millisecond - it is
/// recommended to use the `schedule` flag. This tells the BEAM to allocate that NIF call
/// to a special scheduler. These special schedulers are called "dirty" schedulers.
///
/// We can have two types of "lengthy work" functions: those that are CPU intensive
/// and those that are IO intensive. They should be flagged with "DirtyCpu" and "DirtyIo",
/// respectively.
///
/// See: <https://www.erlang.org/doc/man/erl_nif.html#lengthy_work>
///
/// ```ignore
/// #[nif(schedule = "DirtyCpu")]
/// pub fn my_lengthy_work() -> i64 {
///     let duration = Duration::from_millis(100);
///     std::thread::sleep(duration);
///
///     42
/// }
/// ```
#[proc_macro_attribute]
pub fn nif(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut nif_attributes = nif::NifAttributes::default();

    if !args.is_empty() {
        let nif_macro_parser = syn::meta::parser(|meta| nif_attributes.parse(meta));

        syn::parse_macro_input!(args with nif_macro_parser);
    }

    let input = syn::parse_macro_input!(input as syn::ItemFn);

    nif::transcoder_decorator(nif_attributes, input, false).into()
}

/// Wrap an async function as a spawned task that returns a reference.
///
/// The task is spawned onto the configured async runtime and returns a unique
/// reference immediately. When the task completes, a message `{ref, result}` is
/// sent to the calling process.
///
/// ```ignore
/// #[rustler::task]
/// async fn fetch_data(url: String) -> Result<String, String> {
///     // Long-running async operation
///     tokio::time::sleep(Duration::from_secs(1)).await;
///     Ok("data".to_string())
/// }
/// ```
///
/// From Elixir:
/// ```elixir
/// ref = MyNIF.fetch_data("https://example.com")
/// receive do
///   {^ref, result} -> IO.puts("Got result: #{inspect(result)}")
/// after
///   5000 -> :timeout
/// end
/// ```
#[proc_macro_attribute]
pub fn task(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut nif_attributes = nif::NifAttributes::default();

    if !args.is_empty() {
        let nif_macro_parser = syn::meta::parser(|meta| nif_attributes.parse(meta));

        syn::parse_macro_input!(args with nif_macro_parser);
    }

    let input = syn::parse_macro_input!(input as syn::ItemFn);

    // Require async functions for #[rustler::task]
    if input.sig.asyncness.is_none() {
        return syn::Error::new_spanned(
            input.sig.fn_token,
            "#[rustler::task] requires an async function",
        )
        .to_compile_error()
        .into();
    }

    nif::transcoder_decorator(nif_attributes, input, true).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between an Elixir struct and a Rust struct.
///
/// For example, annotate the following Rust struct:
///
/// ```ignore
/// #[derive(Debug, NifStruct)]
/// #[module = "AddStruct"]
/// struct AddStruct {
///    lhs: i32,
///    rhs: i32,
/// }
/// ```
///
/// Write the following corresponding Elixir struct definition:
///
/// ```elixir
/// defmodule AddStruct do
///   defstruct lhs: 0, rhs: 0
/// end
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that you can use the Elixir struct definition for it.
#[proc_macro_derive(NifStruct, attributes(module, rustler))]
pub fn nif_struct(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    ex_struct::transcoder_decorator(&ast, false).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between an Elixir exception and a Rust struct.
///
/// For example, annotate the following struct:
///
/// ```ignore
/// #[derive(Debug, NifException)]
/// #[module = "AddException"]
/// pub struct AddException {
///     message: String,
/// }
/// ```
///
/// Write the corresponding Elixir exception definition:
///
/// ```elixir
/// defmodule AddException do
///   defexception message: ""
/// end
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that you can use the Elixir exception definition for it.
#[proc_macro_derive(NifException, attributes(module, rustler))]
pub fn nif_exception(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    ex_struct::transcoder_decorator(&ast, true).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between Rust struct and an Elixir map.
///
/// For example, annotate the following struct:
///
/// ```ignore
/// #[derive(NifMap)]
/// struct AddMap {
///     lhs: i32,
///     rhs: i32,
/// }
/// ```
///
/// Create a value of that type:
///
/// ```ignore
/// let value = AddMap { lhs: 33, rhs: 21 };
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that encoding `value` would result in an elixir
/// map with two elements like:
///
/// ```elixir
/// %{lhs: 33, rhs: 21}
/// ```
///
/// And vice versa, decoding this map would result in `value`.
#[proc_macro_derive(NifMap, attributes(rustler))]
pub fn nif_map(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    map::transcoder_decorator(&ast).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between a Rust struct and an Elixir tuple.
///
/// For example, annotate the following struct:
///
/// ```ignore
/// #[derive(NifTuple)]
/// struct AddTuple {
///     lhs: i32,
///     rhs: i32,
/// }
/// ```
///
/// Create a value of that type:
///
/// ```ignore
/// let value = AddMap { lhs: 33, rhs: 21 };
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that encoding `value` would result in an elixir
/// tuple with two elements like:
///
/// ```elixir
/// {33, 21}
/// ```
///
/// And vice versa, decoding this map would result in `value`.
///
/// The size of the tuple will depend on the number of elements in the struct.
#[proc_macro_derive(NifTuple, attributes(rustler))]
pub fn nif_tuple(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    tuple::transcoder_decorator(&ast).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between a Rust struct and an Elixir record.
///
/// For example, annotate the following struct:
///
/// ```ignore
/// #[derive(Debug, NifRecord)]
/// #[tag = "record"]
/// struct AddRecord {
///    lhs: i32,
///    rhs: i32,
/// }
/// ```
///
/// Create a value of that type:
///
/// ```ignore
/// let value = AddRecord { lhs: 33, rhs: 21 };
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that `value` would be encoded into the following elixir value:
///
/// ```elixir
/// {:record, 33, 21}
/// ```
///
/// If you supply the following matching Elixir record definition:
///
/// ```elixir
/// defmodule AddRecord do
///   import Record
///   defrecord :record, [lhs: 1, rhs: 2]
/// end
/// ```
///
/// Then you can use record functions such as `AddRecord.record/0`, `AddRecord.record/1`, `AddRecord.record/2`,
/// to work with the encoded data,
/// and to create data that can be decoded back into your Rust struct.
#[proc_macro_derive(NifRecord, attributes(tag, rustler))]
pub fn nif_record(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    record::transcoder_decorator(&ast).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between an enum and a union of elixir atoms.
///
/// For example:
///
/// ```ignore
/// #[derive(NifUnitEnum)]
/// enum UnitEnum {
///    FooBar,
///    Baz,
/// }
/// ```
///
/// Then the traits `Encoder` and `Decoder` are derived automatically for your Rust struct
/// such that `FooBar` is encoded to, and decoded from, `:foo_bar`.
/// - The variant name is translated from camel case to snake case for the atom name.
/// - Each constructor is required not to have arguments, i.e. to be of unit type.
///
/// An example usage in Rust and Elixir would look like the following.
///
/// ```ignore
/// #[rustler::nif]
/// pub fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
///     unit_enum
/// }
/// ```
///
/// (We are leaving out some boiler plate code to connect the rust code to elixir functions.)
///
/// ```elixir
/// test "unit enum transcoder" do
///   assert :foo_bar == unit_enum_echo(:foo_bar)
///   assert :baz == unit_enum_echo(:baz)
///   assert :invalid_variant == unit_enum_echo(:somethingelse)
/// end
/// ```
///
/// Note that the `:invalid_variant` atom is returned if the user tries to encode something
/// that isn't in the Rust enum.
#[proc_macro_derive(NifUnitEnum, attributes(rustler))]
pub fn nif_unit_enum(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    unit_enum::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifTaggedEnum` macro that lets the user annotate an enum that will
/// generate elixir values when encoded. This can be used for any rust enums and will generate
/// three types of values based on the kind of the enum. For example from the test code:
///
/// ```ignore
/// #[derive(NifTaggedEnum)]
/// enum TaggedEnum {
///     Foo,
///     Bar(String),
///     Baz{ a: i32, b: i32 },
/// }
///
/// pub fn tagged_enum_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
///     let tagged_enum: TaggedEnum = args[0].decode()?;
///     Ok(tagged_enum.encode(env))
/// }
/// ```
///
/// This can be used from elixir in the following manner.
///
/// ```elixir
/// test "tagged enum transcoder" do
///   assert :foo == RustlerTest.tagged_enum_echo(:foo)
///   assert {:bar, "Hello"} == RustlerTest.tagged_enum_echo(:bar, "Hello")
///   assert {:baz, %{a: 33, b: 21}} == RustlerTest.tagged_enum_echo({:baz, %{a: 33, b: 21}})
/// end
/// ```
#[proc_macro_derive(NifTaggedEnum, attributes(rustler))]
pub fn nif_tagged_enum(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    tagged_enum::transcoder_decorator(&ast).into()
}

/// Derives implementations for the `Encoder` and `Decoder` traits
/// which convert between a Rust enum and a union of Elixir types.
///
/// This can be used for Rust enums that contain several constructors containing different types of data,
/// each implementing the `Encoder` and `Decoder` traits.
/// An enum value will be encoded based on the constructor used,
/// and an Elixir value will be decoded based on the value.
///
/// For example from the test code:
///
/// ```ignore
/// #[derive(NifUntaggedEnum)]
/// enum UntaggedEnum {
///     Foo(u32),
///     Bar(String),
///     Baz(AddStruct),
/// }
///
/// #[rustler::nif]
/// pub fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
///     untagged_enum
/// }
/// ```
///
/// Adding boiler plate code to connect Rust code to elixir functions,
/// this can be used from elixir in the following manner.
///
/// ```elixir
/// test "untagged enum transcoder" do
///   assert 123 == untagged_enum_echo(123)
///   assert "Hello" == untagged_enum_echo("Hello")
///   assert %AddStruct{lhs: 45, rhs: 123} = untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})
///   assert :invalid_variant == untagged_enum_echo([1,2,3,4])
/// end
/// ```
///
/// Note that the type of elixir return is dependent on the data in the enum and the actual enum
/// type is lost in the translation because Elixir has no such concept.
#[proc_macro_derive(NifUntaggedEnum, attributes(rustler))]
pub fn nif_untagged_enum(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    untagged_enum::transcoder_decorator(&ast).into()
}

/// Helper attribute for `Resource` implementations
///
/// When an `impl Resource for Type` block is annotated with this attribute, it will automatically
/// set the `IMPLEMENTS_...` associated constants for all implemented callback methods. Thus,
/// instead of
///
/// ```ignore
/// struct ResourceType {}
///
/// impl Resource for ResourceType
/// {
///     const IMPLEMENTS_DESTRUCTOR: bool = true;
///
///     fn destructor(...) { ... }
/// }
/// ```
/// it is enough to provide the implementation:
/// ```ignore
/// #[rustler::resource_impl]
/// impl Resource for ResourceType
/// {
///     fn destructor(...) { ... }
/// }
/// ```
///
/// The resource type is also automatically registered by default, it does not have to be manually
/// registered in a `load` callback. The automatic registration can be disabled with the `register`
/// parameter:
///
/// ```ignore
/// #[rustler::resource_impl]
/// impl Resource for ResourceType
/// {
///     ...
/// }
///
/// // no load callback necessary
/// ```
///
/// If registration is disabled, the resource type has to be registered manually. It is not
/// possible to use the old `resource!` macro for this, as that injects another `impl Resource`
/// block.
/// ```ignore
/// #[rustler::resource_impl(register = false)]
/// impl Resource for ResourceType
/// {
///     ...
/// }
///
/// pub fn on_load(env: Env) -> bool {
///     env.register::<ResourceType>().is_ok()
/// }
/// ```
#[proc_macro_attribute]
pub fn resource_impl(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut attributes = resource_impl::Attributes::default();

    if !args.is_empty() {
        let parser = syn::meta::parser(|meta| attributes.parse(meta));

        syn::parse_macro_input!(args with parser);
    }
    let input = syn::parse_macro_input!(item as syn::ItemImpl);

    resource_impl::transcoder_decorator(attributes, input).into()
}
