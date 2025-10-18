pub mod de;
pub mod markers;
pub mod ser;

pub use self::de::from_value;
pub use self::ser::{to_value, to_value_with, Options as SerOptions};
use piccolo::Lua;

pub trait LuaSerdeExt {
    fn load_serde(&mut self);
}

impl LuaSerdeExt for Lua {
    fn load_serde(&mut self) {
        self.enter(|ctx| markers::set_globals(ctx));
    }
}
