# This is an unorganized list of both immediate and long term tasks #

**It may be updated haphazardly and without warning**

## Speed improvements ##

The following test program is currently 6x slower than PUC-Rio Lua, this is a
good starting point for optimization work.  Need to investigate specifically why
this is 6x slower: (Update, now only 3x slower just from changing LTO settings).

```lua
local sum = 0
for i = 1,10000000 do
    sum = sum + i
end
print(sum)
```

```
~/Documents/luster $ time lua ./test.lua
50000005000000
0.091 secs

~/Documents/luster $ time ./target/release/luster ./test.lua
50000005000000
0.566 secs
```

---

Thread is offensively complex and this probably negatively affects VM speed.  It
mimics PUC-Rio's design in that it uses a variable stack `top` in order to
handle variable arguments / returns, but this means that the stack slice for a
Lua frame is not predictable.

It is possible to instead have Lua stack frames always have, say, exactly 256
available registers, and this can eliminate bounds checking for registers when
executing VM instructions.

By using a separate "varargs" stack, this may make `Thread` have a much simpler
API, make the bytecode safer, and make it more sensible to eliminate register
bounds checking.  I'm not completely sure what this would look like opcode wise,
though?

---

Callbacks currently have `Vec<Value<'gc>>` arguments and returns, and this is an
utterly terrible, temporary state of affairs.  I've thought about this fairly in
depth, and there are a couple of possibilities.  One "easy" one would be to
simply pass callbacks a "value-buffer" type that contains the arguments and have
them return the same buffer, filled with the return values.  If the constructor
for this buffer is private, then the only way callbacks can return values is by
returning the buffer they were given, and in this way we can avoid having so
much `Vec` allocation.  This may dovetail into the idea of having a separate
"varargs" stack?

## API improvements ##

Currently large pieces of the API are pretty ugly to use.  The `Sequence` API is
designed after `futures` 0.3, and as far as a combinator API goes it's not
*terrible*, but it probably needs to be fleshed out.

Possible areas of improvement:
* Some possible way of closing over "contexts" that is less painful to use?
  Might require stable Fn traits to have custom `Fn` implementing types that are
  also `Collect`?
* Callbacks are *extremely* painful to write currently, there may need to be
  more convenient APIs for simple cases?

---

Is it possible in the near term to leverage Rust generators to avoid combinator
hell?  My suspicion is *no* since it is currently impossible for closures to
implement `Collect`, but maybe I'm not being creative enough?

## Missing Features ##

Nearly all of Lua's stdlib is unimplemented:

* coroutine - hard parts are implemented!, only needs convenience functions to be finished
* debug - a huge can of worms
* io - will require userdata support
* os - a small can of worms?
* package - `package.cpath` and `package.loadlib` are probably impossible or at
  least wildly inadvisable
* string - a good starting point, but contains a lot of complex functions
* table - a good starting point
* utf8 - probably after `string`
