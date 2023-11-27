pub mod de;
pub mod markers;
pub mod ser;

use piccolo::Lua;

pub use self::{
    de::from_value,
    ser::{to_value, to_value_with, Options as SerOptions},
};

pub trait LuaSerdeExt {
    fn load_serde(&mut self);
}

impl LuaSerdeExt for Lua {
    fn load_serde(&mut self) {
        self.run(|ctx| markers::set_globals(ctx));
    }
}
