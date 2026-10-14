-- Empty-table removal must work through both the fast path and the
-- metamethod fallback, including an omitted or explicitly nil position.
local metatables = {
    {},
    { __len = function(t) return rawlen(t) end },
    { __index = function() return nil end },
    { __newindex = function(t, k, v) rawset(t, k, v) end },
    { __index = {} },
    { __newindex = {} },
    {
        __len = function(t) return rawlen(t) end,
        __index = function() return nil end,
        __newindex = function(t, k, v) rawset(t, k, v) end,
    },
}

local function check(mt)
    local function empty()
        return setmetatable({}, mt)
    end

    -- Check the return arity as well as the value.
    local function check_nil(...)
        assert(select("#", ...) == 1)
        assert((...) == nil)
    end

    check_nil(table.remove(empty()))
    check_nil(table.remove(empty(), nil))
    check_nil(table.remove(empty(), 0))
    check_nil(table.remove(empty(), 1))

    for _, pos in ipairs({ -1, 2, math.mininteger, math.maxinteger }) do
        assert(not pcall(table.remove, empty(), pos))
    end

    -- Removing the last element must also allow another removal once empty.
    local t = setmetatable({ 42 }, mt)
    assert(not pcall(table.remove, t, 0))
    assert(rawget(t, 1) == 42)
    assert(table.remove(t) == 42)
    assert(rawget(t, 1) == nil)
    check_nil(table.remove(t))
end

check(nil)
for _, mt in ipairs(metatables) do
    check(mt)
end

-- Use the metamethod's length, not the raw array length.
do
    local calls = 0
    local t = setmetatable({ 42 }, {
        __len = function()
            calls = calls + 1
            return 0
        end,
    })
    assert(table.remove(t) == nil)
    assert(table.remove(t, 0) == nil)
    assert(rawget(t, 1) == 42)
    assert(calls == 2)
end
