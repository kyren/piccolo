mod base;
mod coroutine;
mod io;
mod math;
mod string;

pub use self::{
    base::load_base, coroutine::load_coroutine, io::load_io, math::load_math, string::load_string,
};
