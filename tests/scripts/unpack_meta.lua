
local function count_args(...)
    return select("#", ...)
end

do
    local val = {}
    local backing = { 3, 2, 1 }
    setmetatable(val, {
        __index = backing,
        __len = function()
            return #backing
        end,
    })

    local a, b, c = table.unpack(val)
    assert(a == 3, b == 2, c == 1)
    assert(count_args(table.unpack(val)) == 3)
end

do
    local val = { 1, 2, 3, 4 }
    setmetatable(val, {
        __len = function() return 0 end,
    })
    assert(nil == table.unpack(val))
    assert(count_args(table.unpack(val)) == 0)
end

do
    local val = { 1, 2, 3, 4 }
    setmetatable(val, {
        __len = function() return 8 end,
    })
    local a, b, c, d, e, f, g, h = table.unpack(val)

    assert(count_args(table.unpack(val)) == 8)

    assert(a == 1 and b == 2 and c == 3 and d == 4 and
        e == nil and f == nil and g == nil and h == nil)
end

do
    local val = { 1, nil, 3, 4 }
    setmetatable(val, {
        __len = function() return 4 end,
    })
    local a, b, c, d = table.unpack(val)

    assert(count_args(table.unpack(val)) == 4)
    assert(a == 1 and b == nil and c == 3 and d == 4)
end

do
    local val = { 1, nil, 3, nil }
    setmetatable(val, {
        __index = function() return 2 end,
    })

    local first, last = 1, 6
    local a, b, c, d, e, f = table.unpack(val, first, last)

    assert(count_args(table.unpack(val, first, last)) == 6)
    assert(a == 1 and b == 2 and c == 3 and d == 2 and e == 2 and f == 2)
end
