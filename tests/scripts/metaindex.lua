function test1()
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

function test2()
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

test1()
test2()
