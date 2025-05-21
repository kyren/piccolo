-- This is an odd one; it's a minimal reproduction of a bug
-- that ends up corrupting the stack when concat returns
-- an error from its async sequence.
-- (the locals' register allocations aren't optimized away,
-- which allows the interpreter to detect the corruption.)
local a, b, c
pcall(function() print(""..nil) end)

-- Additional reproduction, without using pcall:
--[[
local ts = setmetatable({}, { __concat = function() return "" end})
_ = (function() print(""..ts) end)()
]]
