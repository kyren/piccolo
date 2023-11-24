do
    local function eq(a, b)
        return a.field == b.field
    end

    local a = { field = 2}
    local b = { field = 2}

    setmetatable(a, {__eq = eq})
    assert(a == b)
    a.field = 3
    assert(a ~= b)
    a.field = 2
    assert(a == b)

    setmetatable(a, nil)
    assert(a ~= b)

    setmetatable(b, {__eq = eq})
    assert(a == b)
    b.field = 3
    assert(a ~= b)
    b.field = 2
    assert(a == b)
end
