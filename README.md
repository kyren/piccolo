[![Build Status](https://img.shields.io/circleci/project/github/kyren/deimos.svg)](https://circleci.com/gh/kyren/deimos)

## deimos - An experimental Lua VM implemented in pure Rust ##

(formerly known as `luster`)

**(After *four* years, now UN-paused!)**

Project Goals:
  * Be an arguably working, useful Lua interpreter.
  * Be an easy way to confidently sandbox untrusted Lua scripts.
  * Be somewhat resilient against DoS from untrusted scripts (scripts should not
    be able to cause the interpreter to panic and should be guaranteed to pause
    in some reasonable bounded amount of time).
  * Be an easy way to bind Rust APIs to Lua safely, with a bindings system that is
    resilient against weirdness and edge cases, and with user types that can
    safely participate in runtime garbage collection.
  * Be pragmatically compatible with some version(s) of PUC-Rio Lua.
  * Don't be obnoxiously slow (for example, avoid abstractions that would make
    the interpreter fundamentally slower than PUC-Rio Lua).

Since the focus here is so much on resiliency and safety, `deimos` is written in
(almost) entirely *safe* Rust. This is a *slight* copout as much of the unsafe
code that normally is involved in a language runtime actually lives in `gc-arena`,
but since we have a safe garbage collection abstraction, (almost) the
entire VM can be written in safe code.

*(`deimos` makes no attempt yet to guard against side channel attacks like
spectre, so even *if* the VM is memory safe, running untrusted scripts has
additional risk)*.

**This project is currently very WIP** Right now, the short term goal is to get
some usable subset of Lua working, and to have a robust bindings story. `deimos`
is being worked on again to use in a separate game project, and my immediate
goals are going to be whatever that project requires.

## A unique system for Rust <-> GC interaction ##

*The garbage collector system for `deimos` is now in its [own repo](
https://github.com/kyren/gc-arena), and also on crates.io. See the
README in the linked repo for more detail about the GC design.*

`deimos` has a real, cycle detecting, incremental garbage collector with zero-
cost `Gc` pointers (they are machine pointer sized and implement `Copy`) that
are usable from safe Rust. It achieves this by combining two things:

1) An unsafe `Collect` trait which allows tracing through garbage collected
   types that, despite being unsafe, can be implemented safely using procedural
   macros.
2) Branding `Gc` pointers by unique, invariant "generative" lifetimes to ensure
   that such pointers are isolated to a single root object, and to guarantee
   that, outside an active call to `mutate`, all such pointers are either
   reachable from the root object or are safe to collect.
   
## Stackless VM ##

The `mutate` based GC api means that long running calls to `mutate` can be
problematic. No garbage collection can take place during a call to `mutate`, so
we have to make sure to regularly return from the `mutate` call to allow garbage
collection to take place.

The VM in `deimos` is thus written in what is called "stackless" or "trampoline"
style. It does not rely on the rust stack for Lua -> Rust and Rust -> Lua
nesting, instead callbacks can do one of three things:

  * Return results immediately as a fast path.
  * Return a type implementing a trait called `Sequence`, which the VM
    will drive to completion, similar to how `Future` works. In between
    `Sequence::step` calls, the VM can return from `mutate` and drive garbage
    collection.
  * Return a function (either Rust or Lua) to tail call, and a continuation. The
    function will be called as normal, and after finishing, the results of this
    function (either success or failure) will be passed to the continuation,
    which can then itself do any of the three things that any callback can do,
    return immediately, schedule a sequence, or do yet another tail call.

For example, it is of course possible for Lua to call a Rust callback, which
then in turn creates a new Lua coroutine and runs it. In order to do so, a
callback would take a Lua function as a parameter, then create a new coroutine
Lua thread and return a `Sequence` impl that will run it. The outer main Lua
thread will step the created `Sequence`, which will in turn step the inner
Lua thread. This is exactly how the `coroutine.resume` Lua stdlib function is
implemented.

As another example, `pcall` is easy to implement here, a callback can call the
provided function as a tail call with a continuation, and the continuation can
catch the error and return the error status.

Yet another example, imagine Rust code calling a Lua coroutine thread which
calls more Rust code which calls yet more Lua code which then yields. Our stack
will look something like this:

```
[Rust] -> [Lua Coroutine] -> [Rust Sequence callback] -> [Lua code that yields]
```

This is no problem with this VM style, the inner Rust callback must be a
pausable `Sequence`, so the inner yield will return the value all the way to
the top level Rust code, and when the coroutine thread is resumed, the running
`Sequence` will also be resumed.

With any number of nested Lua threads and `Sequence`s, control will always
continuously return outside the GC arena and to the outer Rust code driving
everything. This is the "trampoline" here, when using this interpreter,
somewhere there is a loop that is continuously calling `Arena::mutate` and
`Thread::step`, and it can stop or pause or change tasks at any time, not
requiring unwinding the Rust stack.

This "stackless" style has many benefits, it allows for concurrency patterns
that are difficult in some other VMs (like tasklets), and makes the VM much more
resilient against untrusted script DoS.

The downside of the "stackless" style is that sometimes writing things as a
`Sequence` is much more difficult than writing in normal, straight control
flow. It would be great if async Rust / generators could help here someday, to
allow for painlessly implementing `Sequence`, but there are *several* current
compiler limitations that make this currently infeasible or so unergonomic that
it is no longer worth it.

## What currently works ##

* An actual cycle detecting, incremental GC similar to the one in PUC-Rio Lua
  5.3
* A basic Lua bytecode compiler
* Lua source code is compiled to a VM bytecode similar to PUC-Rio Lua's, and
  there are a complete set of VM instructions implemented
* Almost all of the core Lua language works. Some tricky Lua features that
  currently actually work:
  * Real closures with proper upvalue handling
  * Proper tail calls
  * Variable arguments and returns
  * Coroutines, including yielding that is transparent to Rust callbacks
  * Gotos with label handling that matches Lua 5.3
  * Proper _ENV handling
  * Metatables and metamethods (only `__call` and `__index` right now, but the
    infrastructure exists, metamethods can even (safely!) be fully recursive,
    triggering any number of other metamethods. `__gc` is an entire separate can
    of worms and doesn't count.)
* A robust Rust callback system that allows for sequencing callbacks that don't
  block the interpreter and reduced stack usage by safely tail calling back
  into Lua.
* Garbage collected "userdata" with safe downcasting.
* A few bits of the stdlib (`print`, `error`, `pcall`, `math`, and the hard bits
  from `coroutine`)
* A simple REPL (try it with `cargo run deimos`)

## What currently doesn't work ##

* Most of the stdlib is not implemented, `io`, `os`, `package`, `string`,
  `table`, `utf8`, `debug`, and most top-level functions are unimplemented.
* Garbage collector finalization. Being compatible with PUC-Rio Lua would
  require object finalization *with failure*, and even having finalization, let
  alone finalization with some kind of failure story is extremely low priority.
  Userdata types can currently implement `Drop` (just like any other rust type)
  to get something equivalent to finaliazation for userdata.
* Other magic garbage collector stuff that PUC-Rio Lua has, like tables with
  weak keys / values, "ephemeron" tables, finalization with object resurrection,
  etc...
* The compiled VM code is in a couple of ways worse than what PUC-Rio Lua will
  generate. Notably, there is a JMP chaining optimization that is not yet
  implemented that makes most loops much slower than in PUC-Rio Lua.
* Error messages that don't make you want to cry
* Stack traces
* Debugger
* Optimized `Value` type that is a reasonable size.
* Actual optimization and real effort towards matching PUC-Rio Lua's performance
* Probably much more I've forgotten about

## What will probably never be implemented ##

This is not an exhaustive list, but these are some things which I currently
consider *almost definite* non-goals.

* An API compatible with the PUC-Rio Lua C API. It would be amazingly difficult
  to implement and would be very slow, and some of it would be basically
  impossible (longjmp error handling and adjacent behavior).
* Perfect compatibility with certain classes of behavior in PUC-Rio Lua:
  * PUC-Rio Lua behaves differently on systems depending on the OS, environment,
    compilation settings, system locale (lexing numbers changes depending on the
    system locale!), etc.  `deimos` is more or less aiming to emulate PUC-Rio
    Lua behavior with the "C" locale set with the default settings in
    `luaconf.h` on 64-bit Linux.
  * The specific format of error messages.
  * The specific iteration order of tables, and the specific behavior of the
    length operator (the length operator currently functions correctly and will
    always return a table "border", but for tables that are not sequences,
    the choice of border that is returned may differ).
* Probably many things in the `debug` library.
* Compatibility with PUC-Rio Lua bytecode
* `os.setlocale` and other weirdness inherited from C
* `package.loadlib` and all functionality which allows loading C libraries.
* Perfectly matching all of the (sometimes exotic and weird!) garbage collector
  behavior in PUC-Rio Lua.

## Why is it called 'deimos'? ##

Lua is the portugese word for "moon", Deimos is the smallest moon of Mars.

Deimos is also the greek god of dread and terror, both of which contribute
highly to this project's existence.

## License ##

`deimos` is licensed under either of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* Creative Commons CC0 1.0 Universal Public Domain Dedication
  [LICENSE-CC0](LICENSE-CC0) or
  https://creativecommons.org/publicdomain/zero/1.0/

at your option.
