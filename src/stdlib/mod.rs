mod base;
mod coroutine;
mod debug;
mod io;
mod math;
mod string;
mod table;

pub use self::{
    base::load_base, coroutine::load_coroutine, debug::load_debug, io::load_io, math::load_math,
    string::load_string, table::load_table,
};
