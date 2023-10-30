[![crates.io](https://img.shields.io/crates/v/piccolo)](https://crates.io/crates/piccolo)
[![docs.rs](https://docs.rs/piccolo/badge.svg)](https://docs.rs/piccolo)
[![Build Status](https://img.shields.io/circleci/project/github/kyren/piccolo.svg)](https://circleci.com/gh/kyren/piccolo)

## piccolo - An experimental stackless Lua VM implemented in pure Rust

**(After *four* years, now UN-paused!)**

Project Goals, in roughly descending priority:
  * Be an arguably working, useful Lua interpreter.
  * Be an easy way to *confidently* sandbox untrusted Lua scripts.
  * Be resilient against DoS from untrusted scripts (scripts should not be able
    to cause the interpreter to panic or use an unbounded amount of memory and
    should be guaranteed to return control to the caller in some bounded amount
    of time).
  * Be an easy way to bind Rust APIs to Lua safely, with a bindings system that is
    resilient against weirdness and edge cases, and with user types that can
    safely participate in runtime garbage collection.
  * Be pragmatically compatible with some version(s) of PUC-Rio Lua.
  * Don't be obnoxiously slow (for example, avoid abstractions that would make
    the interpreter fundamentally slower than PUC-Rio Lua).

## API Instability

Expect *frequent* pre-1.0 API breakage, this crate is still very experimental.
All API incompatible changes will be accompanied by minor version bumps, but
these will be very common.

## Safety

The goal with `piccolo` is to have the majority of it written in safe Rust.
Currently, there are a few sources of unsafety, but crucially these sources
of unsafety are *isolated*. `piccolo` will avoid at all costs relying on
abstractions which *leak* unsafety, it should always be possible to interact
with even low level details of `piccolo` without using `unsafe`.

The current primary sources of unsafety:
  * The particularly weird requirements of Lua tables require using hashbrown's
    low level RawTable API.
  * Userdata requires a very delicate unsafe lifetime dance to deal with
    downcasting non-'static userdata with a safe interface.
  * Unsafe code is required to avoid fat pointers in several Lua types, to keep
    `Value` as small as possible and allow potential future smaller `Value`
    representations.

*(`piccolo` makes no attempt yet to guard against side channel attacks like
spectre, so even if the VM is memory safe, running untrusted scripts may carry
additional risk. With no JIT or callback API to accurately measure time, this
might be practically impossible anwyay.)*.

## A unique system for Rust <-> GC interaction

*The garbage collector system for `piccolo` is now in its [own repo](
https://github.com/kyren/gc-arena), and also on crates.io. See the
README in the linked repo for more detail about the GC design.*

`piccolo` has a real, cycle detecting, incremental garbage collector with
zero-cost `Gc` pointers (they are machine pointer sized and implement `Copy`)
that are usable from safe Rust. It achieves this by combining two things:

1) An unsafe `Collect` trait which allows tracing through garbage collected
   types that, despite being unsafe, can be implemented safely using procedural
   macros.
2) Branding `Gc` pointers by unique, invariant "generative" lifetimes to ensure
   that such pointers are isolated to a single root object, and to guarantee
   that, outside an active call to `mutate`, all such pointers are either
   reachable from the root object or are safe to collect.
   
## Stackless VM

The `mutate` based GC api means that long running calls to `mutate` can be
problematic. No garbage collection can take place during a call to `mutate`, so
we have to make sure to regularly return from the `mutate` call to allow garbage
collection to take place.

The VM in `piccolo` is thus written in what is sometimes called "stackless"
or "trampoline" style. It does not rely on the rust stack for Lua -> Rust and
Rust -> Lua nesting, instead callbacks can either have some kind of immediate
result (return values, yield values from a coroutine, error), or they can
produce a `Sequence`. A `Sequence` is a bit like a `Future` in that it is a
multi-step operation that the parent `Thread` will drive to completion. `Thread`
will repeatedly call `Sequence::poll` until the sequence is complete, and the
`Sequence` can yield values and call arbitrary Lua functions while it is being
polled.

As an example, it is of course possible for Lua to call a Rust callback, which
then in turn creates a new Lua coroutine and runs it. In order to do so, a
callback would take a Lua function as a parameter, then create a new coroutine
Lua thread and return a `Sequence` impl that will run it. The outer main Lua
thread will step the created `Sequence`, which will in turn step the inner
Lua thread. This is exactly how the `coroutine.resume` Lua stdlib function is
implemented.

As another example, `pcall` is easy to implement here, a callback can call the
provided function with a `Sequence` underneath it, and the sequence can catch
the error and return the error status.

Yet another example, imagine Rust code calling a Lua coroutine thread which
calls a Rust `Sequence` which calls yet more Lua code which then yields. Our
stack will look something like this:

```
[Rust] -> [Lua Coroutine] -> [Rust Sequence] -> [Lua code that yields]
```

This is no problem with this VM style, the inner Rust callback is paused as a
`Sequence`, and the inner yield will return the value all the way to the top
level Rust code. When the coroutine thread is resumed and eventually returns,
the Rust `Sequence` will be resumed.

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

## Thread "fuel" and VM memory tracking

The stackless VM style "periodically" returns control to the outer Rust code
driving everything, and how often this happens can be controlled using the
"fuel" system.

Lua and Lua driven callback code *always* happens within some call to
`Thread::step` (either directly or recursively through an outer `Thread::step`
in the case of coroutines). `Thread::step` has a `fuel` parameter which controls
how long the VM should run before pausing, with fuel measured (roughly) in units
of VM instructions.

Different amounts of fuel provided to `Thread::step` bound the amount of Lua
execution that can occur, bounding both the CPU time used and also the amount of
memory allocation that can occur within a single `Thread::step` call (assuming
certain rules are followed w.r.t. provided callbacks).

The VM also now accurately tracks all memory allocated within its inner
`gc-arena::Arena` using `gc-arena` memory tracking features. This can extend
to userdata and userdata APIs, and assuming the correct rules are follwed in
exposed userdata and callbacks, allows for accurate memory reporting and memory
limits.

*Assuming* that both of these mechanisms work correctly, and *assuming* that all
callback / userdata APIs also follow the same rules, this allows for completely
sandboxing untrusted scripts not only in memory safety and API access but also
in CPU and RAM usage. These are big assumptions though, and `piccolo` is still
very much WIP, so ensuring this is done correctly is an ongoing effort.

## What currently works

* An actual cycle detecting, incremental GC similar to the incremental collector
  in PUC-Rio Lua 5.3 / 5.4
* Lua source code is compiled to a VM bytecode similar to PUC-Rio Lua's, and
  there are a complete set of VM instructions implemented
* Almost all of the core Lua language works. Some tricky Lua features that
  currently actually work:
  * Real closures with proper upvalue handling
  * Proper tail calls
  * Variable arguments and returns and generally proper vararg (`...`) handling
  * Coroutines, including yielding that is transparent to Rust callbacks
  * Gotos with label handling that matches Lua 5.3 / 5.4
  * Proper _ENV handling
  * Metatables and metamethods, including fully recursive metamethods that
    trigger other metamethods (Not all metamethods implemented yet, and `__gc`
    is an entirely separate can of worms).
* A robust Rust callback system that allows for sequencing callbacks that don't
  block the interpreter and reduced stack usage by safely tail calling back
  into Lua.
* Garbage collected "userdata" with safe downcasting.
* Some of the stdlib (most of the more core, fundamental parts of the stdlib are
  implemented, e.g. things like the `coroutine` library, `pcall`, `error`, most
  everything that exposes some fundamental runtime feature is implemented).
* A simple REPL (try it with `cargo run --example interpreter`)

## What currently doesn't work

* A large amount of the stdlib is not implemented yet. Most "peripheral" parts
  of the stdlib are this way, the `io`, `file`, `os`, `package`, `string`,
  `table`, and `utf8` libs are either missing or very sparsely implemented.
* The garbage collector has no finalization support. Being compatible with
  PUC-Rio Lua would require object finalization *with failure*, and even having
  finalization, let alone finalization with some kind of failure story is
  extremely low priority. Userdata types can currently implement `Drop` (just
  like any other rust type) to get something equivalent to finaliazation for
  userdata. This means the `__gc` metamethod currently has no effect.
* There is no support for other magic garbage collector stuff that PUC-Rio Lua
  has, like tables with weak keys / values, "ephemeron" tables, finalization
  with object resurrection, etc...
* The compiled VM code is in a couple of ways worse than what PUC-Rio Lua will
  generate. Notably, there is a JMP chaining optimization that is not yet
  implemented that makes most loops much slower than in PUC-Rio Lua.
* Error messages that don't make you want to cry
* Stack traces
* Debugger
* Aggressive optimization and *real* effort towards ensuring that it matches
  PUC-Rio Lua's performance in all cases.
* Probably much more I've forgotten about

## What will probably never be implemented

This is not an exhaustive list, but these are some things which I currently
consider *almost definite* non-goals.

* An API compatible with the PUC-Rio Lua C API. It would be amazingly difficult
  to implement and would be very slow, and some of it would be basically
  impossible (longjmp error handling and adjacent behavior).
* Perfect compatibility with certain classes of behavior in PUC-Rio Lua:
  * PUC-Rio Lua behaves differently on systems depending on the OS, environment,
    compilation settings, system locale, etc. (In certain versions of PUC-Rio Lua,
    even the behavior of the *lexer* changes depending on the system locale!)
    `piccolo` is more or less aiming to emulate PUC-Rio Lua behavior with the
    "C" locale set with the default settings in `luaconf.h` on 64-bit Linux.
  * The specific format of error messages.
  * The specific iteration order of tables, and the specific behavior of the
    length operator (the length operator currently functions correctly and will
    always return a table "border", but for tables that are not sequences,
    the choice of border that is returned may differ).
* The `debug` library is unimplemented and much of it will probably never be
  implemented due to fundamental VM differences.
* Compatibility with PUC-Rio Lua bytecode
* `os.setlocale` and other weirdness inherited from C
* `package.loadlib` and all functionality which allows loading C libraries.
* Perfectly matching all of the (sometimes exotic and weird!) garbage collector
  behavior in PUC-Rio Lua.

## Why is it called 'piccolo'?

It's a cute little "pico" Lua, get it?

It's not really all that "pico", but it's still a cute little instrument you can
safely carry with you anywhere!

## Wasn't this project called something else? Luster? Deimos?

There was an embarassing naming kerfluffle where I somehow ended up with other
people's project names *twice*. They're all the same project. I promise I'm done
renaming it.

## License

`piccolo` is licensed under either of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* Creative Commons CC0 1.0 Universal Public Domain Dedication
  [LICENSE-CC0](LICENSE-CC0) or
  https://creativecommons.org/publicdomain/zero/1.0/

at your option.
