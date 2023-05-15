t = {1, 2, 3, "a", "b", "c"}

local k, v = next(t, nil)
local k, v = next(t, k)
local k, v = next(t, k)
local k, v = next(t, k)
local k, v = next(t, k)
local k, v = next(t, k)
local k = next(t, k)
assert(k == nil, "next after last key is not nil")

assert(select(1, pcall(function() next(t, "d") end)) == false, "next with missing key did not error")
