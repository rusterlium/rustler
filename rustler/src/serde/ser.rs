use std::io::Write;

use crate::serde::{atoms, error::Error, util};
use crate::wrapper::list::make_list;
use crate::{types::tuple, Encoder, Env, OwnedBinary, Term};
use serde::ser::{self, Serialize};

#[inline]
/// Converts a native Rust type into a native Elixir term. See [conversion table](https://github.com/sunny-g/serde_rustler/tree/master/serde_rustler#conversion-table) for details about serialization behavior.
///
pub fn to_term<T>(env: Env, value: T) -> Result<Term, Error>
where
    T: Serialize,
{
    value.serialize(Serializer::from(env))
}

#[derive(Clone, Copy)]
pub struct Serializer<'a> {
    env: Env<'a>,
    non_finite_float_as_atom: bool,
}

impl<'a> From<Env<'a>> for Serializer<'a> {
    fn from(env: Env<'a>) -> Serializer<'a> {
        Serializer {
            env,
            non_finite_float_as_atom: false,
        }
    }
}

impl<'a> ser::Serializer for Serializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    type SerializeSeq = SequenceSerializer<'a>;
    type SerializeTuple = SequenceSerializer<'a>;
    type SerializeTupleStruct = SequenceSerializer<'a>;
    type SerializeTupleVariant = SequenceSerializer<'a>;
    type SerializeMap = MapSerializer<'a>;
    type SerializeStruct = MapSerializer<'a>;
    type SerializeStructVariant = MapSerializer<'a>;

    #[inline]
    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(atoms::nil().encode(self.env))
    }

    #[inline]
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        if !v.is_finite() {
            return if self.non_finite_float_as_atom {
                if v.is_nan() {
                    Ok(atoms::nan().encode(self.env))
                } else if v.is_sign_positive() {
                    Ok(atoms::inf().encode(self.env))
                } else {
                    Ok(atoms::neg_inf().encode(self.env))
                }
            } else {
                Err(Error::NonFiniteFloat)
            };
        }

        Ok(v.encode(self.env))
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        Ok(v.encode(self.env))
    }

    #[inline]
    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let mut ser: SequenceSerializer = self.serialize_seq(Some(1))?;
        ser::SerializeSeq::serialize_element(&mut ser, &(v as u32))?;
        ser::SerializeSeq::end(ser)
    }

    #[inline]
    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let env = self.env;
        let str_len = v.len();
        let mut bin = match OwnedBinary::new(str_len) {
            Some(bin) => bin,
            None => panic!("binary term allocation fail"),
        };
        bin.as_mut_slice()
            .write_all(v.as_bytes())
            .expect("memory copy of string failed");
        Ok(bin.release(env).to_term(env))
    }

    #[inline]
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        let mut binary = OwnedBinary::new(v.len()).unwrap();
        binary
            .as_mut_slice()
            .write_all(v)
            .or(Err(Error::InvalidBinary))?;
        Ok(binary.release(self.env).to_term(self.env))
    }

    /// Serializes unit (empty tuple) as `nil`.
    #[inline]
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(atoms::nil().to_term(self.env))
    }

    #[inline]
    /// Serializes `struct Unit` as `nil`.
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    #[inline]
    /// Serializes `E::A` in `enum E { A, B }` as `:A` or `"A"`, depending on
    /// if the atom `:A` has already been created.
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        atoms::str_to_term(&self.env, variant).or(Err(Error::InvalidVariantName))
    }

    #[inline]
    /// Serializes `struct Millimeters(u8)` as a tagged tuple:
    /// `{:Millimeters, u8}` or `{"Millimeters", u8}`, depending on if the atom
    /// `:Millimeters` has already been created.
    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        let name_term = atoms::str_to_term(&self.env, name).or(Err(Error::InvalidVariantName))?;
        let mut ser = SequenceSerializer::new(self, Some(2), Some(name_term));
        ser.add(value.serialize(self)?);
        ser.to_tuple()
    }

    #[inline]
    /// Serializes `E::N` in `enum E { N(u8) }` as a tagged tuple: `{:N, u8}`
    /// or `{"N", u8}`, depending on if the atom `:N` has already been created.
    /// Serializes `Result::Ok` and `Result::Err` of
    /// `enum Result { Ok(u8), Err(_) }` into `{:ok, u8}` or `{:err, _}`.
    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        match variant {
            "Ok" => self.serialize_newtype_struct("ok", value),
            "Err" => self.serialize_newtype_struct("error", value),
            _ => self.serialize_newtype_struct(variant, value),
        }
    }

    #[inline]
    /// Serializes sequences as a Elixir lists.
    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SequenceSerializer::new(self, len, None))
    }

    #[inline]
    /// Serializes tuples as Elixir tuples.
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(SequenceSerializer::new(self, Some(len), None))
    }

    #[inline]
    /// Serializes `struct Rgb(u8, u8, u8)` as an Elixir Record or Record-like
    /// tuple: `{:Rgb, u8, u8, u8}` or `{"Rgb", u8, u8, u8}`.
    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        let name_term = atoms::str_to_term(&self.env, name).or(Err(Error::InvalidVariantName))?;
        Ok(SequenceSerializer::new(self, Some(len), Some(name_term)))
    }

    #[inline]
    /// Serializes `E::T` of `enum E { T(u8, u8) }` as an Elixir Record or Record-like tuple: `{:T, u8, u8}` or `{"T", u8, u8}`.
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.serialize_tuple_struct(variant, len)
    }

    #[inline]
    /// Serializes map as Elixir map. Keys *will not* serialize into atoms.
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapSerializer::new(self, len, None))
    }

    #[inline]
    /// Serializes as map, but attempts to include
    /// `%{:__struct__ => :STRUCT_NAME}` or `${:__struct__ => "STRUCT_NAME"}`,
    /// if the atom `:STRUCT_NAME` has not already been created, and will also
    /// attempt to serialize keys as atoms.
    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        let name_term = util::str_to_term(&self.env, name).or(Err(Error::InvalidStructName))?;
        Ok(MapSerializer::new(self, Some(len), Some(name_term)))
    }

    #[inline]
    /// Serializes the same as we serialize a struct: `E::S` of
    /// `enum E { S { r: u8, g: u8, b: u8 } }` is a map including
    /// `%{:__struct__ => :S}` or `${:__struct__ => "S"}`.
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.serialize_struct(variant, len)
    }
}

/// SequenceSerializer
pub struct SequenceSerializer<'a> {
    ser: Serializer<'a>,
    items: Vec<Term<'a>>,
}
impl<'a> SequenceSerializer<'a> {
    #[inline]
    fn new(ser: Serializer<'a>, len: Option<usize>, name: Option<Term<'a>>) -> Self {
        let mut items = match len {
            None => Vec::new(),
            Some(length) => Vec::with_capacity(length),
        };

        if let Some(name_term) = name {
            items.push(name_term);
        }

        SequenceSerializer { ser, items }
    }

    #[inline]
    fn add(&mut self, term: Term<'a>) {
        self.items.push(term)
    }

    #[inline]
    fn to_list(&self) -> Result<Term<'a>, Error> {
        let env = self.ser.env;
        let term_array: Vec<_> = self
            .items
            .iter()
            .map(|x| x.encode(env).as_c_arg())
            .collect();
        unsafe { Ok(Term::new(env, make_list(env.as_c_arg(), &term_array))) }
    }

    #[inline]
    fn to_tuple(&self) -> Result<Term<'a>, Error> {
        Ok(tuple::make_tuple(self.ser.env, &self.items))
    }
}

impl<'a> ser::SerializeSeq for SequenceSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.add(value.serialize(self.ser)?);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Term<'a>, Error> {
        self.to_list()
    }
}

impl<'a> ser::SerializeTuple for SequenceSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.add(value.serialize(self.ser)?);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Term<'a>, Error> {
        self.to_tuple()
    }
}

impl<'a> ser::SerializeTupleStruct for SequenceSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<Term<'a>, Error> {
        ser::SerializeTuple::end(self)
    }
}

impl<'a> ser::SerializeTupleVariant for SequenceSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<Term<'a>, Error> {
        ser::SerializeTuple::end(self)
    }
}

/// MapSerializer
pub struct MapSerializer<'a> {
    ser: Serializer<'a>,
    name: Option<Term<'a>>,
    keys: Vec<Term<'a>>,
    values: Vec<Term<'a>>,
}
impl<'a> MapSerializer<'a> {
    #[inline]
    fn new(ser: Serializer<'a>, len: Option<usize>, name: Option<Term<'a>>) -> Self {
        match len {
            None => MapSerializer {
                ser,
                name,
                keys: Vec::new(),
                values: Vec::new(),
            },
            Some(length) => MapSerializer {
                ser,
                name,
                keys: Vec::with_capacity(length),
                values: Vec::with_capacity(length),
            },
        }
    }

    #[inline]
    fn add_key(&mut self, term: Term<'a>) {
        if self.keys.len() == self.values.len() {
            self.keys.push(term)
        } else {
            panic!("MapSerializer.serialize_key was called twice in a row")
        }
    }

    #[inline]
    fn add_val(&mut self, term: Term<'a>) {
        if self.keys.len() == self.values.len() + 1 {
            self.values.push(term)
        } else {
            panic!("MapSerializer.serialize_value was called incorrectly")
        }
    }

    #[inline]
    fn to_map(&self) -> Result<Term<'a>, Error> {
        Term::map_from_arrays(self.ser.env, &self.keys, &self.values).or(Err(Error::InvalidMap))
    }

    #[inline]
    fn to_struct(&self) -> Result<Term<'a>, Error> {
        let struct_atom = atoms::__struct__().to_term(self.ser.env);
        let module_term = self.name.ok_or(Error::ExpectedStructName)?;
        self.to_map()
            .or(Err(Error::InvalidStruct))?
            .map_put(struct_atom, module_term)
            .or(Err(Error::InvalidStruct))
    }
}

impl<'a> ser::SerializeMap for MapSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.add_key(key.serialize(self.ser)?);
        Ok(())
    }

    #[inline]
    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        self.add_val(value.serialize(self.ser)?);
        Ok(())
    }

    fn end(self) -> Result<Term<'a>, Error> {
        self.to_map()
    }
}

impl<'a> ser::SerializeStruct for MapSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        let key_term = atoms::str_to_term(&self.ser.env, key).or(Err(Error::InvalidStructKey))?;
        self.add_key(key_term);
        self.add_val(value.serialize(self.ser)?);
        Ok(())
    }

    fn end(self) -> Result<Term<'a>, Error> {
        self.to_struct()
    }
}

impl<'a> ser::SerializeStructVariant for MapSerializer<'a> {
    type Ok = Term<'a>;
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Term<'a>, Error> {
        ser::SerializeStruct::end(self)
    }
}
