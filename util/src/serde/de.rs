use std::fmt;

use piccolo::{table::NextValue, Table, Value};
use serde::de;
use thiserror::Error;

use super::markers::{is_none, is_unit};

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Message(String),
    #[error("expected {expected}, found {found}")]
    TypeError {
        expected: &'static str,
        found: &'static str,
    },
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

pub fn from_value<'gc, T: de::Deserialize<'gc>>(value: Value<'gc>) -> Result<T, Error> {
    T::deserialize(Deserializer::from_value(value))
}

pub struct Deserializer<'gc> {
    value: Value<'gc>,
}

impl<'gc> Deserializer<'gc> {
    pub fn from_value(value: Value<'gc>) -> Self {
        Self { value }
    }
}

impl<'gc> de::Deserializer<'gc> for Deserializer<'gc> {
    type Error = Error;

    fn deserialize_any<V: de::Visitor<'gc>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.value {
            Value::Nil => self.deserialize_unit(visitor),
            Value::Boolean(_) => self.deserialize_bool(visitor),
            Value::Integer(_) => self.deserialize_i64(visitor),
            Value::Number(_) => self.deserialize_f64(visitor),
            Value::String(_) => self.deserialize_bytes(visitor),
            Value::Table(t) => {
                if is_sequence(t) {
                    self.deserialize_seq(visitor)
                } else {
                    self.deserialize_map(visitor)
                }
            }
            Value::Function(_) => Err(de::Error::custom("cannot deserialize from function")),
            Value::Thread(_) => Err(de::Error::custom("cannot deserialize from thread")),
            Value::UserData(_) => Err(de::Error::custom("cannot deserialize from userdata")),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        visitor.visit_bool(self.value.to_bool())
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Some(i) = self.value.to_integer() {
            visitor.visit_i64(i)
        } else {
            Err(Error::TypeError {
                expected: "integer",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Some(f) = self.value.to_number() {
            visitor.visit_f64(f)
        } else {
            Err(Error::TypeError {
                expected: "number",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Value::String(s) = self.value {
            if let Ok(s) = s.to_str() {
                visitor.visit_borrowed_str(s)
            } else {
                Err(Error::TypeError {
                    expected: "utf8 string",
                    found: "non-utf8 string",
                })
            }
        } else if self.value.is_implicit_string() {
            visitor.visit_string(self.value.display().to_string())
        } else {
            Err(Error::TypeError {
                expected: "utf8 string",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Value::String(s) = self.value {
            if let Ok(utf8) = std::str::from_utf8(s.as_bytes()) {
                visitor.visit_borrowed_str(utf8)
            } else {
                visitor.visit_borrowed_bytes(s.as_bytes())
            }
        } else {
            Err(Error::TypeError {
                expected: "string",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        match self.value {
            Value::Nil => visitor.visit_none(),
            Value::UserData(ud) if is_none(ud) => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        match self.value {
            Value::Nil => visitor.visit_unit(),
            Value::UserData(ud) if is_unit(ud) => visitor.visit_unit(),
            v => Err(Error::TypeError {
                expected: "nil or unit",
                found: v.type_name(),
            }),
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Value::Table(table) = self.value {
            visitor.visit_seq(Seq::new(table))
        } else {
            Err(Error::TypeError {
                expected: "table",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Value::Table(table) = self.value {
            visitor.visit_seq(Tuple::new(
                table,
                len.try_into()
                    .map_err(|_| de::Error::custom("tuple length out of range"))?,
            ))
        } else {
            Err(Error::TypeError {
                expected: "table",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        if let Value::Table(table) = self.value {
            visitor.visit_map(Map::new(table))
        } else {
            Err(Error::TypeError {
                expected: "table",
                found: self.value.type_name(),
            })
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        match self.value {
            Value::Table(table) => match table.next(Value::Nil) {
                NextValue::Found { key, value } => visitor.visit_enum(Enum::new(key, value)),
                NextValue::Last => Err(de::Error::custom("enum table has no entries")),
                NextValue::NotFound => unreachable!(),
            },
            v => visitor.visit_enum(UnitEnum::new(v)),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        self.deserialize_any(visitor)
    }
}

pub struct Seq<'gc> {
    table: Table<'gc>,
    ind: i64,
}

impl<'gc> Seq<'gc> {
    fn new(table: Table<'gc>) -> Self {
        Self { table, ind: 1 }
    }
}

impl<'gc> de::SeqAccess<'gc> for Seq<'gc> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: de::DeserializeSeed<'gc>,
    {
        let v = self.table.get_value(Value::Integer(self.ind));
        if v.is_nil() {
            Ok(None)
        } else {
            let res = Some(seed.deserialize(Deserializer::from_value(v))?);
            self.ind = self
                .ind
                .checked_add(1)
                .ok_or(de::Error::custom("index overflow"))?;
            Ok(res)
        }
    }
}

pub struct Tuple<'gc> {
    table: Table<'gc>,
    len: i64,
    ind: i64,
}

impl<'gc> Tuple<'gc> {
    fn new(table: Table<'gc>, len: i64) -> Self {
        Self { table, len, ind: 1 }
    }
}

impl<'gc> de::SeqAccess<'gc> for Tuple<'gc> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: de::DeserializeSeed<'gc>,
    {
        if self.ind > self.len {
            Ok(None)
        } else {
            let v = self.table.get_value(Value::Integer(self.ind));
            let res = Some(seed.deserialize(Deserializer::from_value(v))?);
            self.ind += 1;
            Ok(res)
        }
    }
}

pub struct Map<'gc> {
    table: Table<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
}

impl<'gc> Map<'gc> {
    fn new(table: Table<'gc>) -> Self {
        Self {
            table,
            key: Value::Nil,
            value: Value::Nil,
        }
    }
}

impl<'gc> de::MapAccess<'gc> for Map<'gc> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'gc>,
    {
        match self.table.next(self.key) {
            NextValue::Found { key, value } => {
                self.key = key;
                self.value = value;
                seed.deserialize(Deserializer::from_value(self.key))
                    .map(Some)
            }
            NextValue::Last => Ok(None),
            NextValue::NotFound => unreachable!(),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'gc>,
    {
        seed.deserialize(Deserializer::from_value(self.value))
    }
}

pub struct Enum<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
}

impl<'gc> Enum<'gc> {
    fn new(key: Value<'gc>, value: Value<'gc>) -> Self {
        Self { key, value }
    }
}

impl<'gc> de::EnumAccess<'gc> for Enum<'gc> {
    type Error = Error;
    type Variant = Variant<'gc>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Variant<'gc>), Error>
    where
        V: de::DeserializeSeed<'gc>,
    {
        Ok((
            seed.deserialize(Deserializer::from_value(self.key))?,
            Variant::new(self.value),
        ))
    }
}

pub struct Variant<'gc> {
    value: Value<'gc>,
}

impl<'gc> Variant<'gc> {
    fn new(value: Value<'gc>) -> Self {
        Self { value }
    }
}

impl<'gc> de::VariantAccess<'gc> for Variant<'gc> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        de::Deserialize::deserialize(Deserializer::from_value(self.value))
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error>
    where
        T: de::DeserializeSeed<'gc>,
    {
        seed.deserialize(Deserializer::from_value(self.value))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        de::Deserializer::deserialize_tuple(Deserializer::from_value(self.value), len, visitor)
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'gc>,
    {
        de::Deserializer::deserialize_map(Deserializer::from_value(self.value), visitor)
    }
}

pub struct UnitEnum<'gc> {
    key: Value<'gc>,
}

impl<'gc> UnitEnum<'gc> {
    fn new(key: Value<'gc>) -> Self {
        Self { key }
    }
}

impl<'gc> de::EnumAccess<'gc> for UnitEnum<'gc> {
    type Error = Error;
    type Variant = UnitVariant;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, UnitVariant), Error>
    where
        V: de::DeserializeSeed<'gc>,
    {
        Ok((
            seed.deserialize(Deserializer::from_value(self.key))?,
            UnitVariant::new(),
        ))
    }
}

pub struct UnitVariant {}

impl UnitVariant {
    fn new() -> Self {
        Self {}
    }
}

impl<'de> de::VariantAccess<'de> for UnitVariant {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }
}

fn is_sequence<'gc>(table: Table<'gc>) -> bool {
    let mut key = match table.next(Value::Nil) {
        NextValue::Found { key, value: _ } => key,
        NextValue::Last => return true,
        NextValue::NotFound => unreachable!(),
    };

    let mut ind = 1;
    loop {
        if !matches!(key, Value::Integer(i) if i == ind) {
            return false;
        }

        ind = if let Some(i) = ind.checked_add(1) {
            i
        } else {
            return false;
        };

        key = match table.next(key) {
            NextValue::Found { key, value: _ } => key,
            NextValue::Last => return true,
            NextValue::NotFound => unreachable!(),
        };
    }
}
