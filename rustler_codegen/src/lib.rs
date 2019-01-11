#![recursion_limit = "128"]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate heck;
extern crate syn;
extern crate proc_macro2;

#[macro_use]
extern crate quote;

mod util;
mod tuple;
mod record;
mod map;
mod ex_struct;
mod unit_enum;
mod untagged_enum;

/// Implementation of the `NifStruct` macro that lets the user annotate a struct that will
/// be translated directly from an Elixir struct to a Rust struct. For example, the following
/// struct, annotated as such:
///
/// ```text
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
/// ```text
/// defmodule AddStruct do
///     defstruct lhs: 0, rhs: 0
/// end
/// ```
#[proc_macro_derive(NifStruct, attributes(module))]
pub fn nif_struct(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    ex_struct::transcoder_decorator(&ast).into()
}

/// Implementation of a macro that lets the user annotate a struct with `NifMap` so that the
/// struct can be encoded or decoded from an Elixir map. For example, the following struct
/// annotated as such:
///
/// ```text
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
/// ```text
/// %{lhs: 33, rhs: 21}
/// ```
#[proc_macro_derive(NifMap)]
pub fn nif_map(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    map::transcoder_decorator(&ast).into()
}

/// Implementation of a macro that lets the user annotate a struct with `NifTuple` so that the
/// struct can be encoded or decoded from an Elixir map. For example, the following struct
/// annotated as such:
///
/// ```text
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
/// ```text
/// {33, 21}
/// ```
///
/// The size of the tuple will depend on the number of elements in the struct.
#[proc_macro_derive(NifTuple)]
pub fn nif_tuple(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    tuple::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifRecord` macro that lets the user annotate a struct that will
/// be translated directly from an Elixir struct to a Rust struct. For example, the following
/// struct, annotated as such:
///
/// ```text
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
/// ```text
/// defmodule AddRecord do
///     import Record
///     defrecord :record, [lhs: 1, rhs: 2]
/// end
/// ```
#[proc_macro_derive(NifRecord, attributes(tag))]
pub fn nif_record(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    record::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifUnitEnum` macro that lets the user annotate an enum with a unit type
/// that will generate elixir atoms when encoded
///
/// ```text
/// #[derive(NifUnitEnum)]
/// enum UnitEnum {
///    FooBar,
///    Baz,
/// }
/// ```
///
/// An example usage in elixir would look like the following.
///
/// ```text
/// test "unit enum transcoder" do
///    assert :foo_bar == RustlerTest.unit_enum_echo(:foo_bar)
///    assert :baz == RustlerTest.unit_enum_echo(:baz)
///    assert :invalid_variant == RustlerTest.unit_enum_echo(:somethingelse)
/// end
/// ```
///
/// Note that the `:invalid_variant` atom is returned if the user tries to encode something
/// that isn't in the Rust enum.
#[proc_macro_derive(NifUnitEnum)]
pub fn nif_unit_enum(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    unit_enum::transcoder_decorator(&ast).into()
}

/// Implementation of the `NifUntaggedEnum` macro that lets the user annotate an enum that will
/// generate elixir values when decoded. This can be used for rust enums that contain data and
/// will generate a value based on the kind of data encoded. For example from the test code:
///
/// ```text
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
/// ```text
///   test "untagged enum transcoder" do
///    assert 123 == RustlerTest.untagged_enum_echo(123)
///    assert "Hello" == RustlerTest.untagged_enum_echo("Hello")
///    assert %AddStruct{lhs: 45, rhs: 123} = RustlerTest.untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})
///    assert :invalid_variant == RustlerTest.untagged_enum_echo([1,2,3,4])
///  end
/// ```
///
/// Note that the type of elixir return is dependent on the data in the enum and the actual enum
/// type is lost in the translation because Elixir has no such concept.
#[proc_macro_derive(NifUntaggedEnum)]
pub fn nif_untagged_enum(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    untagged_enum::transcoder_decorator(&ast).into()
}
