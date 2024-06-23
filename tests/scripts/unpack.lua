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

do
    local val = { [-1] = 1, [0] = 2, 3, 4 }

    local first = -1
    local a, b, c, d = table.unpack(val, first)
    assert(count_args(table.unpack(val, first)) == 4)
    assert(a == 1 and b == 2 and c == 3 and d == 4)
end

do
    local val = { 1, 2, 3, 4 }

    assert(table.unpack(val, 2, 2) == 2)
    assert(count_args(table.unpack(val, 2, 2)) == 1)

    assert(count_args(table.unpack(val, 3, 2)) == 0)
    assert(count_args(table.unpack(val, 1, -1)) == 0)
end

do
    local val = { 1, 2, 3, 4 }

    setmetatable(val, { __len = function() return 1 end })

    assert(table.unpack(val) == 1)
    assert(count_args(table.unpack(val)) == 1)

    setmetatable(val, { __len = function() return 0 end })

    assert(table.unpack(val) == nil)
    assert(count_args(table.unpack(val)) == 0)

    setmetatable(val, { __len = function() return -1 end })

    assert(table.unpack(val) == nil)
    assert(count_args(table.unpack(val)) == 0)
end

do
    local first = (1 << 63)     -- i64::MIN
    local last  = (1 << 63) - 1 -- i64::MAX
    -- computing length with `end - start + 1` will overflow

    -- check all cases for getting the length:
    local ok, status
    ok, status = pcall(function()
        local val = { 1, 2, 3, 4 }

        local _ = count_args(table.unpack(val, first, last))
    end)
    assert(not ok)

    ok, status = pcall(function()
        local val = setmetatable({ 1, 2, 3, 4 }, {
            __len = function() return (1 << 63) - 1 end
        })
        local _ = count_args(table.unpack(val, first))
    end)
    assert(not ok)

    ok, status = pcall(function()
        local val = setmetatable({ 1, 2, 3, 4 }, {
            __index = function() return 3 end
        })
        local _ = count_args(table.unpack(val, first, last))
    end)
    assert(not ok)
end

do
    local val = {}
    assert(count_args(table.unpack(val, 1, (1 << 16))) == (1 << 16))
end

do
    local val = setmetatable({ }, {
        __len = function() return (1 << 16) end
    })
    assert(count_args(table.unpack(val)) == (1 << 16))
end
