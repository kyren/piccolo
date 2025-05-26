--- pass
--- abc	1	false
--- def	hello

print("abc", 1, false)

local mt = { __tostring = function() return "hello" end }
print("def", setmetatable({}, mt))
