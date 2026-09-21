pub mod de;
pub mod markers;
pub mod ser;

use piccolo::Lua;

pub use self::{
    de::from_value,
    ser::{Options as SerOptions, to_value, to_value_with},
};

pub trait LuaSerdeExt {
    fn load_serde(&mut self);
}

impl LuaSerdeExt for Lua {
    fn load_serde(&mut self) {
        self.enter(|ctx| markers::set_globals(ctx));
    }
}
