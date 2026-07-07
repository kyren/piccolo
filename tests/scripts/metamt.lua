local meta = {__metatable = 6}

local test = {}
setmetatable(test, meta)
assert(getmetatable(test) == 6)


function is_err(f)
    return pcall(f) == false
end


-- Testing dynamic changes and support for all Lua types bar userdata:

-- notably, if __metatable field is absent, then getmetatable returns normally
meta.__metatable = nil                -- nil
assert(getmetatable(test) ~= nil)
assert(getmetatable(test) == meta)

meta.__metatable = true               -- boolean
assert(getmetatable(test) == true)

meta.__metatable = 7                  -- integer
assert(getmetatable(test) == 7)

meta.__metatable = 6.4                -- number
assert(getmetatable(test) == 6.4)

meta.__metatable = "mt"               -- string
assert(getmetatable(test) == "mt")

local test2 = {}
meta.__metatable = test2              -- table
assert(getmetatable(test) == test2)

local function test3() end
meta.__metatable = test3              -- function
assert(getmetatable(test) == test3)

local test4 = coroutine.create(test3)
meta.__metatable = test4              -- thread
assert(getmetatable(test) == test4)



-- Testing that `.__metatable` fields are not themselves used as metamethods.

test2.__index = { key = "value" }
meta.__metatable = test2
assert(test["key"] == nil)

meta.__index = { key = "direct" }
assert(test["key"] == "direct")


-- Testing that presence of __metatable override forbids updating the metatable.

assert(is_err(function() setmetatable(test, {}) end))

