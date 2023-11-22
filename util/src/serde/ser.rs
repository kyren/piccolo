use std::fmt;

use piccolo as lua;
use serde::ser;
use thiserror::Error;

use super::markers::{none, unit};

#[derive(Debug, Error)]
#[error("{0}")]
pub struct Error(String);

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error(msg.to_string())
    }
}

pub fn to_value<'gc, T: ser::Serialize>(
    ctx: lua::Context<'gc>,
    value: &T,
) -> Result<lua::Value<'gc>, Error> {
    value.serialize(Serializer::new(ctx))
}

#[derive(Copy, Clone)]
pub struct Serializer<'gc> {
    ctx: lua::Context<'gc>,
}

impl<'gc> Serializer<'gc> {
    pub fn new(ctx: lua::Context<'gc>) -> Self {
        Self { ctx }
    }
}

impl<'gc> ser::Serializer for Serializer<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    type SerializeSeq = SerializeSeq<'gc>;
    type SerializeTuple = SerializeSeq<'gc>;
    type SerializeTupleStruct = SerializeSeq<'gc>;
    type SerializeTupleVariant = SerializeTupleVariant<'gc>;
    type SerializeMap = SerializeMap<'gc>;
    type SerializeStruct = SerializeStruct<'gc>;
    type SerializeStructVariant = SerializeStructVariant<'gc>;

    fn serialize_bool(self, v: bool) -> Result<lua::Value<'gc>, Error> {
        Ok(lua::Value::Boolean(v))
    }

    fn serialize_i8(self, v: i8) -> Result<lua::Value<'gc>, Error> {
        self.serialize_i64(v.into())
    }

    fn serialize_i16(self, v: i16) -> Result<lua::Value<'gc>, Error> {
        self.serialize_i64(v.into())
    }

    fn serialize_i32(self, v: i32) -> Result<lua::Value<'gc>, Error> {
        self.serialize_i64(v.into())
    }

    fn serialize_i64(self, v: i64) -> Result<lua::Value<'gc>, Error> {
        Ok(lua::Value::Integer(v))
    }

    fn serialize_u8(self, v: u8) -> Result<lua::Value<'gc>, Error> {
        self.serialize_u64(v.into())
    }

    fn serialize_u16(self, v: u16) -> Result<lua::Value<'gc>, Error> {
        self.serialize_u64(v.into())
    }

    fn serialize_u32(self, v: u32) -> Result<lua::Value<'gc>, Error> {
        self.serialize_u64(v.into())
    }

    fn serialize_u64(self, v: u64) -> Result<lua::Value<'gc>, Error> {
        if let Ok(i) = i64::try_from(v) {
            Ok(lua::Value::Integer(i))
        } else {
            self.serialize_f64(v as f64)
        }
    }

    fn serialize_f32(self, v: f32) -> Result<lua::Value<'gc>, Error> {
        self.serialize_f64(v.into())
    }

    fn serialize_f64(self, v: f64) -> Result<lua::Value<'gc>, Error> {
        Ok(lua::Value::Number(v))
    }

    fn serialize_char(self, v: char) -> Result<lua::Value<'gc>, Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<lua::Value<'gc>, Error> {
        self.serialize_bytes(v.as_bytes())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<lua::Value<'gc>, Error> {
        Ok(self.ctx.state.strings.intern(&self.ctx, v).into())
    }

    fn serialize_none(self) -> Result<lua::Value<'gc>, Error> {
        Ok(none(self.ctx).into())
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<lua::Value<'gc>, Error>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<lua::Value<'gc>, Error> {
        Ok(unit(self.ctx).into())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<lua::Value<'gc>, Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<lua::Value<'gc>, Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<lua::Value<'gc>, Error>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<lua::Value<'gc>, Error>
    where
        T: serde::Serialize,
    {
        let value = value.serialize(self)?;
        let table = lua::Table::new(&self.ctx);
        table.set(self.ctx, variant, value).unwrap();
        Ok(table.into())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<SerializeSeq<'gc>, Error> {
        Ok(SerializeSeq::new(self.ctx))
    }

    fn serialize_tuple(self, len: usize) -> Result<SerializeSeq<'gc>, Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<SerializeSeq<'gc>, Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Error> {
        Ok(SerializeTupleVariant::new(self.ctx, variant))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Error> {
        Ok(SerializeMap::new(self.ctx))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Error> {
        Ok(SerializeStruct::new(self.ctx))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Error> {
        Ok(SerializeStructVariant::new(self.ctx, variant))
    }
}

pub struct SerializeSeq<'gc> {
    ctx: lua::Context<'gc>,
    table: lua::Table<'gc>,
    ind: i64,
}

impl<'gc> SerializeSeq<'gc> {
    pub fn new(ctx: lua::Context<'gc>) -> Self {
        Self {
            ctx,
            table: lua::Table::new(&ctx),
            ind: 1,
        }
    }
}

impl<'gc> ser::SerializeSeq for SerializeSeq<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Error>
    where
        T: serde::Serialize,
    {
        self.table
            .set(
                self.ctx,
                self.ind,
                value.serialize(Serializer::new(self.ctx))?,
            )
            .unwrap();
        self.ind = self
            .ind
            .checked_add(1)
            .ok_or(ser::Error::custom("index overflow"))?;
        Ok(())
    }

    fn end(self) -> Result<lua::Value<'gc>, Error> {
        Ok(self.table.into())
    }
}

impl<'gc> ser::SerializeTuple for SerializeSeq<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Error>
    where
        T: serde::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<lua::Value<'gc>, Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'gc> ser::SerializeTupleStruct for SerializeSeq<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Error>
    where
        T: serde::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<lua::Value<'gc>, Error> {
        ser::SerializeSeq::end(self)
    }
}

pub struct SerializeMap<'gc> {
    ctx: lua::Context<'gc>,
    table: lua::Table<'gc>,
    next_key: lua::Value<'gc>,
}

impl<'gc> SerializeMap<'gc> {
    pub fn new(ctx: lua::Context<'gc>) -> Self {
        Self {
            ctx,
            table: lua::Table::new(&ctx),
            next_key: lua::Value::Nil,
        }
    }
}

impl<'gc> ser::SerializeMap for SerializeMap<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.next_key = key.serialize(Serializer::new(self.ctx))?;
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.table
            .set(
                self.ctx,
                self.next_key,
                value.serialize(Serializer::new(self.ctx))?,
            )
            .map_err(|_| {
                ser::Error::custom("key in map / struct must not serialize to Nil / NaN")
            })?;
        self.next_key = lua::Value::Nil;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.table.into())
    }
}

pub struct SerializeStruct<'gc> {
    ctx: lua::Context<'gc>,
    table: lua::Table<'gc>,
}

impl<'gc> SerializeStruct<'gc> {
    pub fn new(ctx: lua::Context<'gc>) -> Self {
        Self {
            ctx,
            table: lua::Table::new(&ctx),
        }
    }
}

impl<'gc> ser::SerializeStruct for SerializeStruct<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.table
            .set(self.ctx, key, value.serialize(Serializer::new(self.ctx))?)
            .unwrap();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.table.into())
    }
}

pub struct SerializeTupleVariant<'gc> {
    ctx: lua::Context<'gc>,
    variant: &'static str,
    table: lua::Table<'gc>,
    ind: i64,
}

impl<'gc> SerializeTupleVariant<'gc> {
    pub fn new(ctx: lua::Context<'gc>, variant: &'static str) -> Self {
        Self {
            ctx,
            variant,
            table: lua::Table::new(&ctx),
            ind: 1,
        }
    }
}

impl<'gc> ser::SerializeTupleVariant for SerializeTupleVariant<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.table
            .set(
                self.ctx,
                self.ind,
                value.serialize(Serializer::new(self.ctx))?,
            )
            .unwrap();
        self.ind = self
            .ind
            .checked_add(1)
            .ok_or(ser::Error::custom("index overflow"))?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let enclosing = lua::Table::new(&self.ctx);
        enclosing.set(self.ctx, self.variant, self.table).unwrap();
        Ok(enclosing.into())
    }
}

pub struct SerializeStructVariant<'gc> {
    ctx: lua::Context<'gc>,
    variant: &'static str,
    table: lua::Table<'gc>,
}

impl<'gc> SerializeStructVariant<'gc> {
    pub fn new(ctx: lua::Context<'gc>, variant: &'static str) -> Self {
        Self {
            ctx,
            variant,
            table: lua::Table::new(&ctx),
        }
    }
}

impl<'gc> ser::SerializeStructVariant for SerializeStructVariant<'gc> {
    type Ok = lua::Value<'gc>;
    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.table
            .set(self.ctx, key, value.serialize(Serializer::new(self.ctx))?)
            .unwrap();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let enclosing = lua::Table::new(&self.ctx);
        enclosing.set(self.ctx, self.variant, self.table).unwrap();
        Ok(enclosing.into())
    }
}
