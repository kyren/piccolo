## piccolo-util - Helper crate that makes using the `piccolo` crate easier.

This crate is not necessary to use `piccolo`, and is a place for things that are
useful but may be more opinionated or limited than the core `piccolo` library.

## Features

* Adds support for "freezing" Rust values, which allows you to safely erase
  lifetimes from them and check at runtime that those values are not accessed
  past their actual lifetime.
* Adds `serde` support for easy conversion of Rust data types to and from Lua.
* Adds a way to quickly make a metatable of simple userdata methods and bind it
  to userdata.
