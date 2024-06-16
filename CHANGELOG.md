## [0.3.2]

* Bugfix for tail-calling uncallable values. Fixes internal panics.
* Major bugfix for finalization, make sure to transition the collector
  immediately to `Collecting` after finalization is done. Fixes lost `Thread`
  finalization and unclosed upvalues.
* Make the `type` builtin match PUC-Rio Lua by @Jengamon.
* Fix Lua stack corruption during tail calls with less arguments than expected.
* Make function statements act like local / upvalue assignment when appropriate.
* Fix `math.random` and `math.log` to better match PUC-Rio Lua by @Jengamon.
* Fix `select` to better match PUC-Rio Lua by @Jengamon.
* Implement `math.randomseed` by @Jengamon.
* Let `__index` and `__newindex` chain through `UserData` in addition to
  `Table`.
* Implement "dead keys" to make table iteration behavior match PUC-Rio Lua.
* Implement `gc_arena::Collect` for `piccolo_util::UserDataMethods` and
  `piccolo_util::StaticUserDataMethods`.
* Implement `string.sub`, `string.lower`, `string.upper`, `string.reverse` by
  @Jengamon.
* Better match PUC-Rio Lua behavior with longstring newlines.

## [0.3.1]

Small fixups from 0.3

* Actually export `ExecutorInner`
* Add a missing `#[doc(hidden)]` around an internal macro.

## [0.3]

Huge release! Much safer `Executor` API that no longer requires recursion
from Rust -> Lua -> Rust for Rust callbacks to call Lua functions, eliminating
problems with unrestricted Rust stack usage. The `Executor` API also has a
bunch more weird powers that other implementations of Lua can't have, like "tail
resuming" other coroutines and "tail yield".

There is a new `piccolo-util` crate that adds support for some very common use
cases that are not trivial to do in `piccolo` proper:

* Serde support for convenient conversion between Rust types and Lua tables.
* "Freeze" system to safely support the common case where you need to pass
  a non-'static (and non-'gc) value into Lua. Not specific to piccolo, it is
  actually a general way of safely erasing a single lifetime parameter from a
  type (and replacing it with a runtime check).
* Super quick and simple way to wrap Rust types into a Lua userdata with
  methods.

`piccolo-util` will always be an **optional** dependency, and it may contain
code that is more opinionated or limited than vanilla `piccolo` should be.
`piccolo-util` will have opionions about things, and those opinions may be
different than yours... if it is in your way or incomplete for your use, you can
always use it as a starting point for something better.

Also includes a lot of quality of life API improvements, error message
improvements, and more!

- New `Executor` API that enables safe thread recursion and "tail resume" /
  "tail yield".
- New `piccolo-util` crate with very commonly requested, useful features that
  are too opinionated or limited to belong in `piccolo` proper.
- API changes to `Stack` to support a single, unified thread stack shared
  between Lua and callbacks, similar to PUC-Rio Lua et al.
- Upvalues no longer keep entire threads alive and instead use new gc-arena
  finalization support to become closed when threads are garbage collected.
- `IntoMultiValue` / `FromMultiValue` conversion for tuples now allows every
  element to be multi-converted rather than just the last element.
- Support the `__eq` metamethod.
- Error message improvements in lexer / parser errors (they now have line
  numbers at least!).
- API changes to second callback parameter, now an `Execution` type with `Fuel`
  access *and* also calling thread information.
- Add "chunk name" information to compiled chunks for future use in runtime
  errors / tracebacks.
- Simplified `ctx` access, most methods are now directly implemented on `Context`.
- Lots of type renames for clarity, `AnyCallback` -> `Callback`, `AnyUserData`
  -> `UserData`, `AnyValue` -> `Any`, and others.
- Add line number annotations to opcodes for future tracebacks.
- Clean up general ptr handling and allow the user to access internal `Gc`
  pointers in all cases, allows for weak pointers to all pointer types.

## [0.2]
- Allow `Thread` to be forcibly reset to a stopped state.
- Improve the `Table` API, add functions that skip `IntoValue` conversion and
  simplify `Table::next`.
- Support `__newindex`.
- Auto conversion improvements, add a `Variadic` wrapper type to indicate
  variadic multi-values instead of bare arrays.
- Add `Function::compose` and `Function::bind` for easier generic function
  handling from Rust.
- Completely track used memory within interpreter instances. Tracks both
  `gc-arena` allocated `Gc` pointers as well as all normal heap allocations
  using `gc-arena` external allocation tracking.
- `Fuel` system to limit the execution time of Lua code.
- Properly handle `...` in table constructors.
- Implement `table.select('#')`, `table.pack`, and `table.unpack`.
- Fix local function declarations to be visible in their own function body.
- Guard against arbitrary recursion depth of callbacks (only ever a risk for
  Threads calling callbacks on *other* Threads, aka Lua coroutines).

## [0.1.1]
- Initial crates.io release
