use crate::Error as NifError;
use serde::{de, ser};
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum Error {
    DeserializationError(String),
    TypeHintsRequired,
    InvalidAtom,
    InvalidBoolean,
    InvalidNumber,
    InvalidStringable,
    InvalidList,
    InvalidTuple,
    InvalidSequenceElement,
    ExpectedAtom,
    ExpectedBoolean,
    ExpectedBinary,
    ExpectedNumber,
    ExpectedChar,
    ExpectedStringable,
    ExpectedNil,
    ExpectedList,
    ExpectedTuple,
    ExpectedEnum,
    ExpectedMap,
    ExpectedStruct,
    ExpectedStructName,
    ExpectedStructValue,
    ExpectedUnitVariant,
    ExpectedNewtypeStruct,
    ExpectedNewtypeVariant,
    ExpectedTupleVariant,
    ExpectedStructVariant,

    SerializationError(String),
    InvalidVariantName,
    InvalidStructName,
    InvalidBinary,
    InvalidMap,
    InvalidStruct,
    InvalidStructKey,

    NonFiniteFloat,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self)
    }
}

impl From<Error> for NifError {
    fn from(err: Error) -> NifError {
        NifError::RaiseTerm(Box::new(err.to_string()))
    }
}

impl std::error::Error for Error {
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error::SerializationError(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error::DeserializationError(msg.to_string())
    }
}
