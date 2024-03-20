use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum AnimalType {
    #[serde(rename = "Elixir.SerdeRustlerTests.Readme.AnimalType.Cat")]
    Cat(String),
    #[serde(rename = "Elixir.SerdeRustlerTests.Readme.AnimalType.Dog")]
    Dog(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename = "Elixir.SerdeRustlerTests.Readme.Animal")]
pub struct Animal {
    #[serde(rename = "type")]
    _type: AnimalType,
    name: String,
    age: u8,
    owner: Option<String>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Unit;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UnitVariant {
    #[serde(rename = "UnitVariant::A")]
    A,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum NewtypeVariant {
    #[serde(rename = "Elixir.SerdeRustlerTests.NifTest.NewtypeVariant.N")]
    N(u8),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename = "Elixir.SerdeRustlerTests.NifTest.NewtypeStruct")]
pub struct NewtypeStruct(pub u8);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename = "Elixir.SerdeRustlerTests.NifTest.TupleStruct")]
pub struct TupleStruct(pub u8, pub u8, pub u8);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TupleVariant {
    #[serde(rename = "Elixir.SerdeRustlerTests.NifTest.TupleVariant.T")]
    T(u8, u8),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename = "Elixir.SerdeRustlerTests.NifTest.Struct")]
pub struct Struct {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum StructVariant {
    #[serde(rename = "Elixir.SerdeRustlerTests.NifTest.StructVariant.S")]
    S { r: u8, g: u8, b: u8 },
}
