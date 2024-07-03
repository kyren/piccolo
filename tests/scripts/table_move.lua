
local function arrays_eq(a, b)
    for k, v in pairs(a) do
        if b[k] ~= v then return false end
    end
    for k, v in pairs(b) do
        if a[k] ~= v then return false end
    end
    return true
end

do
    local t = { 1, 2, 3 }
    table.move(t, 1, 3, 4)
    assert(arrays_eq(t, { 1, 2, 3, 1, 2, 3 }))

    t = { 1, 2, 3 }
    table.move(t, 1, 3, 1)
    assert(arrays_eq(t, { 1, 2, 3 }))

    table.move(t, 2, 3, 2)
    assert(arrays_eq(t, { 1, 2, 3 }))

    table.move(t, 2, 3, 1)
    assert(arrays_eq(t, { 2, 3, 3 }))

    table.move(t, 1, 1, 3)
    assert(arrays_eq(t, { 2, 3, 2 }))
end

do
    local t

    -- forward overlapping copy
    t = { 1, 2, 3, 4, 5 }
    table.move(t, 1, 5, 3)
    assert(arrays_eq(t, { 1, 2, 1, 2, 3, 4, 5 }))

    -- backward overlapping copy
    t = { 1, 2, 3, 4, 5 }
    table.move(t, 3, 5, 1)
    assert(arrays_eq(t, { 3, 4, 5, 4, 5 }))

    t = { 1, 2, 3, 4, 5 }
    table.move(t, 1, 5, 3, t) -- with explicit dest of self
    assert(arrays_eq(t, { 1, 2, 1, 2, 3, 4, 5 }))

    t = { 1, 2, 3, 4, 5 }
    table.move(t, 3, 5, 1, t) -- with explicit dest of self
    assert(arrays_eq(t, { 3, 4, 5, 4, 5 }))
end

do
    local a = { 1, 2, 3 }
    local b = {}

    table.move(a, 1, 3, 2, b)
    assert(arrays_eq(b, { nil, 1, 2, 3 }))
    assert(arrays_eq(a, { 1, 2, 3 }))

    b = {}
    table.move(a, 1, 3, -1, b)
    assert(arrays_eq(b, { [-1] = 1, [0] = 2, 3 }))
end
