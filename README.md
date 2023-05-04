[![Build Status](https://img.shields.io/circleci/project/github/kyren/luster.svg)](https://circleci.com/gh/kyren/luster)

## luster - An experimental Lua VM implemented in pure Rust ##

**(After *four* years, now UN-paused!)**

Project Goals:
  * Be an arguably working, useful Lua interpreter.
  * Be extremely easy to confidently sandbox untrusted scripts.
  * Be somewhat resilient against DoS from untrusted scripts (scripts should not
    be able to cause the interpreter to panic and should be guaranteed to pause
    in some reasonable bounded amount of time).
  * Be easy to bind Rust APIs to Lua safely, with a bindings system that is
    resilient against weirdness and edge cases, and with user types that can
    safely participate in runtime garbage collection.
  * Be pragmatically compatible with some version(s) of PUC-Rio Lua.
  * Don't be obnoxiously slow (for example, avoid abstractions that would make
    the interpreter fundamentally slower than PUC-Rio Lua).

Since the focus here is so much on resiliency and safety, Luster is written in
(almost) entirely *safe* Rust. This is a *slight* copout as most of the unsafe
code that normally is involved in a language runtime actually lives in `gc-
arena`, but since we have a safe garbage collection abstraction, (almost) the
entire VM can be written in safe code.

*(Luster makes no attempt yet to guard against side channel attacks like
spectre, so even *if* the VM is extremely safe, running untrusted scripts has
additional risk)*.

**This project is currently very WIP** Right now, the short term goal is to get
some usable subset of Lua working, and to have a robust bindings story. `luster`
is being worked on again to use in a separate game project, and my immediate
goals are going to be whatever that project requires.

## A unique system for Rust <-> GC interaction ##

*The garbage collector system for luster is now in its [own repo](
https://github.com/kyren/gc-arena), and also on crates.io. See the
README in the linked repo for more detail about the GC design.*

`luster` has a real, cycle detecting, incremental garbage collector with zero-
cost `Gc` pointers (they are machine pointer sized and implement `Copy`) that
are usable from safe Rust. It achieves this by combining three techniques:

1) An unsafe `Collect` trait which allows tracing through garbage collected
   types that, despite being unsafe, can be implemented safely using procedural
   macros.
2) Branding `Gc` pointers by unique, invariant "generative" lifetimes to ensure
   that such pointers are isolated to a single root object, and to guarantee
   that, outside an active call to `mutate`, all such pointers are either
   reachable from the root object or are safe to collect.
3) The mutation API, while being safe via "generativity", does not make it easy
   to allow garbage collection to take place continuously. Since no garbage
   collection at all can take place during a call to `mutate`, long running
   mutations are problematic. By using a `futures`-like combinator based
   "sequencing" system, we can recover the ability for garbage collect to take
   place with as fine of a granularity as necessary, with garbage collection
   taking place in-between the "sequence" steps.
   
The last point has benefits beyond safe garbage collection: it means that the
entire VM *including* sequences of Lua -> Rust and Rust -> Lua callbacks is
expressed in a sort of "stackless" or what is sometimes called "trampoline"
style. Rather than implementing the VM or callbacks with recursion and the Rust
stack, VM executions and callbacks are constructed as `Sequence` state machines
via combinators. The interpreter receives this `Sequence` to execute and simply
loops, calling `Sequence::step` until the operation is finished (and garbage
collecting in-between the `step` calls).

This "stackless" style has many benefits, it allows for concurrency patterns
that are difficult or impossible in other Lua interpreters (like tasklets), and
will also hopefully make the VM much more resilient against untrusted script
DoS.

The downside of the "stackless" style is that sometimes writing things as a
`Sequence` is much more difficult than writing in normal, straight control flow.
It would be great if async Rust / generators could help here someday, but there
are *several* current compiler limitations that make this currently infeasible,
so for now `Sequence` combinators are what I have.

## What currently works ##

* An actual cycle detecting, incremental GC similar to the one in PUC-Rio Lua
  5.3
* A basic Lua bytecode compiler
* Lua source code is compiled to a VM bytecode similar to PUC-Rio Lua's, and
  there are a complete set of VM instructions implemented
* Almost all of the core Lua language (minus metatables) works. Some tricky Lua
  features that currently actually work:
  * Real closures with proper upvalue handling
  * Tail calls
  * Variable arguments and returns
  * Coroutines, including yielding through Rust callbacks (like through `pcall`)
  * Gotos with label handling that matches Lua 5.3
  * Proper _ENV handling
* A few bits of the stdlib (`print`, `error`, `pcall`, `math`, and the hard bits
  from `coroutine`)
* Basic support for Rust callbacks
* Garbage collected "userdata" with safe downcasting.
* A simple REPL (try it with `cargo run luster`)

## What currently doesn't work ##

* Most of the stdlib is not implemented (`debug` which will probably never be
  implemented), `io`, `os`, `package`, `string`, `table`, `utf8`, most top-level
  functions are unimplemented.
* Metatables and metamethods. Most of this should not be terribly hard to
  implement, and this is the highest priority on the TODO list (*not including*
  `__gc`, that will be separate and is extremely low priority).
* Garbage collector finalization. Being compatible with PUC-Rio Lua would
  require object finalization *with failure*, and even having finalization let
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
* Actual optimization and real effort towards matching PUC-Rio Lua's performance
* Probably much more I've forgotten about

## What may never be implemented ##

This is not an exhaustive list, but these are some things which I currently
consider *almost definite* non-goals.

* An API compatible with the PUC-Rio Lua C API. It would be amazingly difficult
  to implement and would be very slow, and some of it would be basically
  impossible (longjmp error handling and adjacent behavior).
* Perfect compatibility with certain classes of behavior in PUC-Rio Lua:
  * PUC-Rio Lua behaves differently on systems depending on the OS, environment,
    compilation settings, system locale (lexing numbers changes depending on the
    system locale!), etc.  `luster` is more or less aiming to emulate PUC-Rio
    Lua behavior with the "C" locale set with the default settings in
    `luaconf.h` on 64-bit Linux.
  * The specific format of error messages.
  * The specific iteration order of tables, and the specific behavior of the
    length operator (the length operator currently functions correctly and will
    always return a table "border", but for tables that are not sequences,
    the choice of border that is returned may differ).
* Anything in the `debug` library.
* Compatibility with PUC-Rio Lua bytecode
* `os.setlocale` and other weirdness inherited from C
* `package.loadlib` and all functionality which allows loading C libraries.
* Perfectly matching all of the (sometimes exotic and weird!) garbage collector
  behavior in PUC-Rio Lua.

## License ##

`luster` is licensed under either of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* Creative Commons CC0 1.0 Universal Public Domain Dedication
  [LICENSE-CC0](LICENSE-CC0) or
  https://creativecommons.org/publicdomain/zero/1.0/

at your option.
