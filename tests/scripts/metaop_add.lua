do
    local function add(a, b)
        return { a.field, b.field }
    end

    local a = { field = 1 }
    local b = { field = 4 }

    setmetatable(a, { __add = add })

    local res

    res = a + b
    assert(#res == 2 and res[1] == a.field and res[2] == b.field)
    res = b + a
    assert(#res == 2 and res[1] == b.field and res[2] == a.field)

    setmetatable(a, nil)
    setmetatable(b, { __add = add })

    res = a + b
    assert(#res == 2 and res[1] == a.field and res[2] == b.field)
    res = b + a
    assert(#res == 2 and res[1] == b.field and res[2] == a.field)
end

do
    local function add1(a, b)
        return { 1, a.field, b.field }
    end
    local function add2(a, b)
        return { 2, a.field, b.field }
    end

    local a = { field = 1 }
    local b = { field = 4 }

    setmetatable(a, { __add = add1 })
    setmetatable(b, { __add = add2 })

    local res

    -- Prioritize __add metaop of first table
    res = a + b
    assert(#res == 3 and res[1] == 1 and res[2] == a.field and res[3] == b.field)
    res = b + a
    assert(#res == 3 and res[1] == 2 and res[2] == b.field and res[3] == a.field)

    -- If the first table has no __add metaop, try the second
    setmetatable(a, { })
    res = a + b
    assert(#res == 3 and res[1] == 2 and res[2] == a.field and res[3] == b.field)
    res = b + a
    assert(#res == 3 and res[1] == 2 and res[2] == b.field and res[3] == a.field)

    setmetatable(a, { __add = add1 })
    setmetatable(b, { })
    res = a + b
    assert(#res == 3 and res[1] == 1 and res[2] == a.field and res[3] == b.field)
    res = b + a
    assert(#res == 3 and res[1] == 1 and res[2] == b.field and res[3] == a.field)
end

do
    local function add(a, b)
        return { a, b }
    end

    local a = { field = 1 }
    setmetatable(a, { __add = add })

    local res
    res = a + 3
    assert(#res == 2 and res[1].field == 1 and res[2] == 3)
    res = 3 + a
    assert(#res == 2 and res[1] == 3 and res[2].field == 1)
end
