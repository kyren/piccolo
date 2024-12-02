- ⚫️️ = unimplemented
- 🟡 = differing
- 🔵 = implemented
- ❗= will not implement
- 🤷‍♀️ = low importance

"Implemented" means "near 1:1 PUC-Lua behavior"[^0].

"Differing" means that there is an implementation, but it doesn't correspond to PUC-Lua behavior.

"Unimplemented" means there is no implementation (when used, `nil` is found) _or_
that calling the implementation with the corresponding arguments will error where in PUC-Lua it does not.

"Will Not Implement" is for functions that will not be implemented due to a fundamental difference between piccolo and PUC-Lua.

"Low Importance" is for things that, while technically implementable, will
likely not be implemented due to differences between piccolo and PUC-Lua.

**NOTE**: `(a[, b, c])` corresponds to the Lua docs' `(a[, b[, c]])` usage.

## Base

| Status | Function                                                       | Differences                                                                                                                            | Notes |
| ------ | -------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- | ----- |
| 🔵     | `assert(v[, message])`                                         |                                                                                                                                        |       |
| 🔵     | `collectgarbage("count")`                                      |                                                                                                                                        |       |
| ⚫️    | `collectgarbage("collect")`                                    |                                                                                                                                        |       |
| ⚫️    | `collectgarbage("stop")`                                       |                                                                                                                                        |       |
| ⚫️    | `collectgarbage("restart")`                                    |                                                                                                                                        |       |
| ⚫️    | `collectgarbage("step"[, memkb])`                              |                                                                                                                                        |       |
| ⚫️    | `collectgarbage("isrunning")`                                  |                                                                                                                                        |       |
| 🤷‍♀️     | `collectgarbage("incremental"[, gcpause, stepmult, stepsize])` |                                                                                                                                        |       |
| 🤷‍♀️     | `collectgarbage("generational"[, minormult, majormult])`       |                                                                                                                                        |       |
| ⚫️    | `dofile([filename])`                                           |                                                                                                                                        |       |
| 🟡     | `error(message)`                                               | Due to `level` not being implemented for, all calls here give the same result as PUC-Lua `error(message, 0)` (or any invalid `level`). |       |
| ⚫️    | `error(message, level)`                                        |                                                                                                                                        |       |
| ⚫️    | `_G` (value)                                                   |                                                                                                                                        |       |
| 🔵     | `getmetatable(object)`                                         |                                                                                                                                        |       |
| 🟡     | `ipairs(t)`                                                    | PUC-Lua returns `iter, table, 0`, where as piccolo returns `iter, table`.                                                              |       |
| ⚫️    | `load(chunk[, chunkname, mode, env])`                          |                                                                                                                                        |       |
| ⚫️    | `loadfile([filename, mode, env])`                              |                                                                                                                                        |       |
| 🔵     | `next(table [, index])`                                        |                                                                                                                                        |       |
| 🔵     | `pairs(t)`                                                     | By default, PUC-Lua return `iter, table, nil` where as piccolo returns `iter, table`.                                                  |       |
| 🔵     | `pcall(f, args...)`                                            |                                                                                                                                        |       |
| 🔵     | `print(args...)`                                               |                                                                                                                                        |       |
| ⚫️    | `rawequal(v1, v2)`                                             |                                                                                                                                        |       |
| 🔵     | `rawget(table, index)`                                         |                                                                                                                                        |       |
| 🔵    | `rawlen(v)`                                                    |                                                                                                                                        |       |
| 🔵     | `rawset(table, index, value)`                                  |                                                                                                                                        |       |
| 🔵     | `select(index, args...)`                                       |                                                                                                                                        |       |
| 🔵     | `setmetatable(table, metatable)`                               |                                                                                                                                        |       |
| 🔵    | `tonumber(e[, base])`                                          |                                                                                                                                        |       |
| 🟡     | `tostring(v)`                                                  | piccolo does not use the metatable field `__name` by default, while PUC-Lua does.                                                      |       |
| 🔵     | `type(v)`                                                      |                                                                                                                                        |       |
| 🔵    | `_VERSION` (value)                                             |                                                                                                                                        |       |
| ⚫️    | `warn(msg, args...)`                                           |                                                                                                                                        |       |
| ⚫️    | `xpcall(f, msgh, args...)`                                     |                                                                                                                                        |       |

[^0]: Hedging b/c I don't know PUC-Lua like my reverse palm, and there might be differing behaviors if you poke both implementations to death, but that's not what this document is for.

## Coroutine

| Status | Function                | Differences | Notes |
| ------ | ----------------------- | ----------- | ----- |
| ⚫️️   | `close(co)`             |             |       |
| 🔵     | `create(f)`             |             |       |
| ⚫️️   | `isyieldable([co])`     |             |       |
| 🔵     | `resume(co[, vals...])` |             |       |
| 🔵     | `running()`             |             |       |
| 🔵     | `status(co)`            |             |       |
| ⚫️️   | `wrap(f)`               |             |       |
| 🔵     | `yield(args...)`        |             |       |

## Package

| Status | Function                             | Differences                                                                                     | Notes |
| ------ | ------------------------------------ | ----------------------------------------------------------------------------------------------- | ----- |
| ⚫️️   | (global) `require(modname)`          |                                                                                                 |       |
| ⚫️️   | `config` (value)                     |                                                                                                 |       |
| ❗     | `cpath` (value)                      |                                                                                                 |       |
| ⚫️️   | `loaded` (value)                     |                                                                                                 |       |
| ❗     | `loadlib(libname, funcname)`         |                                                                                                 |       |
| ⚫️️   | `path` (value)                       |                                                                                                 |       |
| ⚫️️   | `preload` (value)                    |                                                                                                 |       |
| ⚫️️   | `searchers` (value)                  | This implementation will _definitely_ differ from PUC-Lua as piccolo does not support C loaders |       |
| ⚫️️   | `searchpath(name, path[, sep, rep])` |                                                                                                 |       |

## String

| Status | Function                          | Differences | Notes |
| ------ | --------------------------------- | ----------- | ----- |
| ⚫️️   | `byte(s[, i, j])`                 |             |       |
| ⚫️️   | `char(args...)`                   |             |       |
| ⚫️️   | `dump(function[, strip])`         |             |       |
| ⚫️️   | `find(s, pattern[, init, plain])` |             |       |
| ⚫️️   | `format(formatstring, args...)`   |             |       |
| ⚫️️   | `gmatch(s, pattern[, init])`      |             |       |
| ⚫️️   | `gsub(s, pattern, repl[, n])`     |             |       |
| 🔵     | `len(s)`                          |             |       |
| 🔵   | `lower(s)`                        |             |       |
| ⚫️️   | `match(s, pattern[, init])`       |             |       |
| ⚫️️   | `pack(fmt, values...)`            |             |       |
| ⚫️️   | `packsize(fmt)`                   |             |       |
| ⚫️️   | `rep(s, n[, sep])`                |             |       |
| 🔵   | `reverse(s)`                      |             |       |
| 🔵   | `sub(s, i[, j])`                  |             |       |
| ⚫️️   | `unpack(fmt, s[, pos])`           |             |       |
| 🔵   | `upper(s)`                        |             |       |

## UTF8

| Status | Function                     | Differences | Notes |
| ------ | ---------------------------- | ----------- | ----- |
| ⚫️️   | `char(args..)`               |             |       |
| ⚫️️   | `charpattern` (value)        |             |       |
| ⚫️️   | `codes(s[, lax])`            |             |       |
| ⚫️️   | `codepoints(s[, i, j, lax])` |             |       |
| ⚫️️   | `len(s[, i, j, lax])`        |             |       |
| ⚫️️   | `offset(s, n[, i])`          |             |       |

## Table

| Status | Function                     | Differences | Notes |
| ------ | ---------------------------- | ----------- | ----- |
| 🔵     | `concat(list[, sep, i, j])`  |             | Supports the `__concat` metamethod |
| 🔵     | `insert(list, [pos,] value)` |             |       |
| 🔵     | `move(a1, f, e, t[, a2])`    |             | Currently implemented with a Lua polyfill |
| 🔵     | `pack(args...)`              |             |       |
| 🔵     | `remove(list[, pos])`        |             |       |
| 🔵     | `sort(list[, comp])`         |             | Currently implemented with a Lua polyfill using a simple merge sort, rather than PUC-Rio Lua's quicksort impl |
| 🔵     | `unpack(list[, i, j])`       |             |       |

## Math

I'm not going over these with a fine-tooth comb, if it exists (and takes the specified number of arguments), it's considered implemented. (Except for "basic" identities like $\cos(0) = 1$ and stuff like that.)

| Status | Function             | Differences | Notes |
| ------ | -------------------- | ----------- | ----- |
| 🔵     | `abs(x)`             |             |       |
| 🔵     | `acos(x)`            |             |       |
| 🔵     | `asin(x)`            |             |       |
| 🔵     | `atan(y[, x])`       |             |       |
| 🔵     | `ceil(x)`            |             |       |
| 🔵     | `cos(x)`             |             |       |
| 🔵     | `deg(x)`             |             |       |
| 🔵     | `exp(x)`             |             |       |
| 🔵     | `floor(x)`           |             |       |
| 🔵     | `fmod(x, y)`         |             |       |
| 🔵     | `huge` (value)       |             |       |
| 🔵     | `log(x[, base])`     |             |       |
| 🔵     | `max(x, args...)`    |             |       |
| 🔵     | `maxinteger` (value) |             |       |
| 🔵     | `min(x, args...)`    |             |       |
| 🔵     | `mininteger` (value) |             |       |
| 🔵     | `modf(x)`            |             |       |
| 🔵     | `pi` (value)         |             |       |
| 🔵     | `rad(x)`             |             |       |
| 🔵     | `random([m, n])`     |             |       |
| 🔵     | `randomseed([x, y])` |             |       |
| 🔵     | `sin(x)`             |             |       |
| 🔵     | `sqrt(x)`            |             |       |
| 🔵     | `tan(x)`             |             |       |
| 🔵     | `tointeger(x)`       |             |       |
| 🔵     | `type(x)`            |             |       |
| 🔵     | `ult(m, n)`          |             |       |

## I/O

I see a module in the code repo that is labelled the IO library, but it only creates the `print` global, which is not the IO module (as understood from the Lua Manual).

| Status | Function                      | Differences                                                                                                                 | Notes |
| ------ | ----------------------------- | --------------------------------------------------------------------------------------------------------------------------- | ----- |
| ⚫️    | `close([file])`               |                                                                                                                             |       |
| ⚫️    | `flush()`                     |                                                                                                                             |       |
| ⚫️    | `input([file])`               |                                                                                                                             |       |
| ⚫️    | `lines([filename, args...])`  |                                                                                                                             |       |
| ⚫️    | `open(filename [, mode])`     |                                                                                                                             |       |
|        | `output([file])`              |                                                                                                                             |       |
| ⚫️/❗ | `popen(prog[, mode])`         | Might be classifiable as "C weirdness" or it's just creating another process which kinda feels as icky as the OS module imo |       |
| ⚫️    | `read(args...)`               |                                                                                                                             |       |
| ⚫️    | `tmpfile()`                   |                                                                                                                             |       |
| ⚫️    | `type(obj)`                   |                                                                                                                             |       |
| ⚫️    | `write(args...)`              |                                                                                                                             |       |
| ⚫️    | `file:close()`                |                                                                                                                             |       |
| ⚫️    | `file:flush()`                |                                                                                                                             |       |
| ⚫️    | `file:lines(args...)`         |                                                                                                                             |       |
| ⚫️    | `file:read(args...)`          |                                                                                                                             |       |
| ⚫️    | `file:seek([whence, offset])` |                                                                                                                             |       |
| ⚫️    | `file:setvbuf(mode[, size])`  |                                                                                                                             |       |
| ⚫️    | `file:write(args...)`         |                                                                                                                             |       |

## OS

IMO this module is best in its current state, but I cannot stop one from downloading the individual pixels of Henry Cavill's side profile, so...

| Status | Function                        | Differences                                                                                                                                                                                | Notes |
| ------ | ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----- |
| ⚫️    | `clock()`                       |                                                                                                                                                                                            |       |
| ⚫️    | `date([format, time])`          |                                                                                                                                                                                            |       |
| ⚫️    | `difftime(t2, t1)`              |                                                                                                                                                                                            |       |
| ❗     | `execute([command])`            | Because PUC-Lua requires this to be isomorphic to ISO C `system`, I can simply put this under C weirdness!                                                                                 |       |
| ⚫️    | `exit([code, close])`           | Probably a❗, but I cannae tell you want to do                                                                                                                                             |       |
| ⚫️    | `getenv(varname)`               | ...what is this a shell script?                                                                                                                                                            |       |
| ⚫️    | `remove(filename)`              |                                                                                                                                                                                            |       |
| ⚫️    | `rename(oldname, newname)`      |                                                                                                                                                                                            |       |
| ❗     | `setlocale(locale[, category])` | This is _explictly_ not going to be implemented according to the README, along with its C weirdness brethren, I just have problems with the rest of this module. _Personnel_ problems \\s. |       |
| ⚫️    | `time([table])`                 |                                                                                                                                                                                            |       |
| ⚫️    | `tmpname()`                     |                                                                                                                                                                                            |       |

## Debug

As stated on the repository main page, this library for the most part is not
implemented nor are there plans to implement due to differences between the implementations. This sections is mostly so that people might get an idea of what _is_ implemented, and what is theoretically _possible_ to implement.

| Status | Function                                  | Implementation Notes / Differences                                                                                                                                                                        | Notes |
| ------ | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----- |
| ⚫️    | `debug()`                                 |                                                                                                                                                                                                           |       |
| ❗     | `gethook([thread])`                       |                                                                                                                                                                                                           |       |
| ⚫️    | `getinfo([thread, ]f[, what])`            |                                                                                                                                                                                                           |       |
| ⚫️    | `getlocal([thread, ]f, local)`            |                                                                                                                                                                                                           |       |
| ⚫️    | `getmetatable(value)`                     |                                                                                                                                                                                                           |       |
| ⚫️    | `getregistry()`                           |                                                                                                                                                                                                           |       |
| ⚫️    | `getupvalue(f, up)`                       |                                                                                                                                                                                                           |       |
| ⚫️    | `getuservalue(u, n)`                      |                                                                                                                                                                                                           |       |
| ❗     | `sethook([thread, ] hook, mask[, count])` |                                                                                                                                                                                                           |       |
| ⚫️    | `setlocal([thread, ]level, local, value)` |                                                                                                                                                                                                           |       |
| ⚫️    | `setmetatable(value, table)`              | Interesting thing to note is that this is _not_ the base library `setmetatable`, as `debug.setmetatable`'s first argument accepts any Lua value, while `setmetatable`'s first argument _must_ be a table. |       |
| ⚫️    | `setupvalue(f, up, value)`                |                                                                                                                                                                                                           |       |
| ⚫️    | `setuservalue(udata, value, n)`           |                                                                                                                                                                                                           |       |
| ⚫️    | `traceback([thread,][message, level])`    |                                                                                                                                                                                                           |       |
| ⚫️    | `upvalueid(f, n)`                         |                                                                                                                                                                                                           |       |
| ⚫️    | `upvaluejoin(f1, n1, f2, n2)`             |                                                                                                                                                                                                           |       |
