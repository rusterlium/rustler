#![recursion_limit = "128"]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate heck;
extern crate proc_macro2;
extern crate syn;

#[macro_use]
extern crate quote;

mod context;
mod ex_struct;
mod init;
mod map;
mod nif;
mod record;
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

/// Implementation of a Native Implementated Function (NIF) macro that lets the user annotate
/// a function that will be wrapped in higer-level NIF implementation.
///
/// ```ignore
/// #[rustler::nif]
/// fn add(a: i64, b: i64) -> i64 {
///     a + b
/// }
///
/// #[rustler::nif]
/// fn add(a: i64, b: i64) -> i64 {
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
/// rustler::init!("Elixir.Math", [add, sub, mul, div], Some(load));
/// ```
#[proc_macro]
pub fn init(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as init::InitMacroInput);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}

/// Implementation of a Native Implementated Function (NIF) macro that lets the user annotate
/// a function that will be wrapped in higer-level NIF implementation.
///
/// ```ignore
/// #[nif]
/// fn add(a: i64, b: i64) -> i64 {
///     a + b
/// }
/// ```
#[proc_macro_attribute]
pub fn nif(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(args as syn::AttributeArgs);
    let input = syn::parse_macro_input!(input as syn::ItemFn);

    nif::transcoder_decorator(args, input).into()
}

/// Implementation of the `NifStruct` macro that lets the user annotate a struct that will
/// be translated directly from an Elixir struct to a Rust struct. For example, the following
/// struct, annotated as such:
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
/// This would be translated by Rustler into:
///
/// ```elixir
/// defmodule AddStruct do
///   defstruct lhs: 0, rhs: 0
/// end
/// ```
#[proc_macro_derive(NifStruct, attributes(module, rustler))]
pub fn nif_struct(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    ex_struct::transcoder_decorator(&ast).into()
}

/// Implementation of a macro that lets the user annotate a struct with `NifMap` so that the
/// struct can be encoded or decoded from an Elixir map. For example, the following struct
/// annotated as such:
///
/// ```ignore
/// #[derive(NifMap)]
/// struct AddMap {
///     lhs: i32,
///     rhs: i32,
/// }
/// ```
///
/// Given the values 33 and 21 for this struct, this would result, when encoded, in an elixir
/// map with two elements like:
///
/// ```elixir
/// %{lhs: 33, rhs: 21}
/// ```
#[proc_macro_derive(NifMap, attributes(rustler))]
pub fn nif_map(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    map::transcoder_decorator(&ast).into()
}

/// Implementation of a macro that lets the user annotate a struct with `NifTuple` so that the
/// struct can be encoded or decoded from an Elixir tuple. For example, the following struct
/// annotated as such:
///
/// ```ignore
/// #[derive(NifTuple)]
/// struct AddTuple {
///     lhs: i32,
///     rhs: i32,
/// }
/// ```
///
/// Given the values 33 and 21 for this struct, this would result, when encoded, in an elixir
/// tuple with two elements like:
///
/// ```elixir
/// {33, 21}
/// ```
///
/// The size of the tuple will depend on the number of elements in the struct.
#[proc_macro_derive(NifTuple, attributes(rustler))]
pub fn nif_tuple(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    tuple::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifRecord` macro that lets the user annotate a struct that will
/// be translated directly from an Elixir struct to a Rust struct. For example, the following
/// struct, annotated as such:
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
/// This would be translated by Rustler into:
///
/// ```elixir
/// defmodule AddRecord do
///   import Record
///   defrecord :record, [lhs: 1, rhs: 2]
/// end
/// ```
#[proc_macro_derive(NifRecord, attributes(tag, rustler))]
pub fn nif_record(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    record::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifUnitEnum` macro that lets the user annotate an enum with a unit type
/// that will generate elixir atoms when encoded
///
/// ```ignore
/// #[derive(NifUnitEnum)]
/// enum UnitEnum {
///    FooBar,
///    Baz,
/// }
/// ```
///
/// An example usage in elixir would look like the following.
///
/// ```elixir
/// test "unit enum transcoder" do
///   assert :foo_bar == RustlerTest.unit_enum_echo(:foo_bar)
///   assert :baz == RustlerTest.unit_enum_echo(:baz)
///   assert :invalid_variant == RustlerTest.unit_enum_echo(:somethingelse)
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

/// Implementation of the `NifUntaggedEnum` macro that lets the user annotate an enum that will
/// generate elixir values when decoded. This can be used for rust enums that contain data and
/// will generate a value based on the kind of data encoded. For example from the test code:
///
/// ```ignore
/// #[derive(NifUntaggedEnum)]
/// enum UntaggedEnum {
///     Foo(u32),
///     Bar(String),
///     Baz(AddStruct),
/// }
///
/// pub fn untagged_enum_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
///     let untagged_enum: UntaggedEnum = args[0].decode()?;
///     Ok(untagged_enum.encode(env))
/// }
/// ```
///
/// This can be used from elixir in the following manner.
///
/// ```elixir
/// test "untagged enum transcoder" do
///   assert 123 == RustlerTest.untagged_enum_echo(123)
///   assert "Hello" == RustlerTest.untagged_enum_echo("Hello")
///   assert %AddStruct{lhs: 45, rhs: 123} = RustlerTest.untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})
///   assert :invalid_variant == RustlerTest.untagged_enum_echo([1,2,3,4])
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
