--- pass
--- some message
--- stack traceback:
---   <callback: debug.traceback>
---   ./tests/goldenscripts/debug_traceback.lua:62 in <function 'test2' at line 61>
---     arguments:
---       msg: some message
---       level: 0
---   ./tests/goldenscripts/debug_traceback.lua:66 in <function 'test1' at line 65>
---     arguments:
---       msg: some message
---       level: 0
---   ./tests/goldenscripts/debug_traceback.lua:73 in <chunk>
---
--- some other message
--- stack traceback:
---   ./tests/goldenscripts/debug_traceback.lua:62 in <function 'test2' at line 61>
---     arguments:
---       msg: some other message
---       level: 1
---   ./tests/goldenscripts/debug_traceback.lua:66 in <function 'test1' at line 65>
---     arguments:
---       msg: some other message
---       level: 1
---   ./tests/goldenscripts/debug_traceback.lua:74 in <chunk>
---
--- some other message
--- stack traceback:
---   ./tests/goldenscripts/debug_traceback.lua:62 in <function 'test2' at line 61>
---     arguments:
---       msg: some other message
---       level: nil
---   ./tests/goldenscripts/debug_traceback.lua:66 in <function 'test1' at line 65>
---     arguments:
---       msg: some other message
---       level: nil
---   ./tests/goldenscripts/debug_traceback.lua:75 in <chunk>
---
--- skip too much
--- stack traceback: <no frames>
--- a pcall message
--- stack traceback:
---   ./tests/goldenscripts/debug_traceback.lua:62 in <function 'test2' at line 61>
---     arguments:
---       msg: a pcall message
---       level: nil
---   ./tests/goldenscripts/debug_traceback.lua:66 in <function 'test1' at line 65>
---     arguments:
---       msg: a pcall message
---       level: nil
---   <sequence>
---   ./tests/goldenscripts/debug_traceback.lua:70 in <function 'sequence_test' at line 69>
---     arguments:
---       msg: a pcall message
---       level: nil
---   ./tests/goldenscripts/debug_traceback.lua:77 in <chunk>
---


-- Line 60
function test2(msg, level)
    print(debug.traceback(nil, msg, level))
end

function test1(msg, level)
    test2(msg, level)
end

function sequence_test(msg, level)
    pcall(test1, msg, level)
end

test1("some message", 0)
test1("some other message", 1)
test1("some other message")
test1("skip too much", 100)
sequence_test("a pcall message")

trace1 = debug.traceback(nil, nil, 1) trace2 = debug.traceback()
assert(trace1 == trace2, "traceback with level 1 should be the same as without level")