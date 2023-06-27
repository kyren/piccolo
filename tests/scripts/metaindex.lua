do
    local mt = {
        __index = function(table, key)
            return key
        end
    }

    local t = {}
    setmetatable(t, mt)

    assert(t.foo == "foo")
    assert(t["hello there"] == "hello there")
end

do
    local idx = {}

    local mt = {
        __index = idx,
    }

    local t = {}
    setmetatable(t, mt)

    assert(t.foo == nil)

    idx.foo = 3
    assert(t.foo == 3)
end

do
    local idx = {}

    local mt = {
        __index = idx,
        __newindex = idx,
    }

    local t = {}
    setmetatable(t, mt)

    assert(t.foo == nil)

    t.foo = 3
    assert(t.foo == 3 and idx.foo == 3)
    t.foo = 4
    assert(t.foo == 4 and idx.foo == 4)
end


do
    local idx = {}

    local mt = {
        __newindex = function(table, key, value) 
            idx[key] = value
        end,
    }

    local t = {}
    setmetatable(t, mt)

    t.foo = 3
    assert(idx.foo == 3)
    t.foo = 4
    assert(idx.foo == 4)
end
