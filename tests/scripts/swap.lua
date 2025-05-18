do
    local x, y = 1, 2
    x, y = y, x
    assert(x == 2 and y == 1)

    x, y = 1, 2
    y, x = x, y
    assert(x == 2 and y == 1)
end

do
    local t = { x = 1, y = 2 }
    t.x, t.y = t.y, t.x
    assert(t.x == 2 and t.y == 1)

    t = { x = 1, y = 2 }
    t.y, t.x = t.x, t.y
    assert(t.x == 2 and t.y == 1)
end

do
    local t = { "a", "b" }
    t[1], t[2] = t[2], t[1]
    assert(t[1] == "b" and t[2] == "a")

    t = { "a", "b" }
    t[2], t[1] = t[1], t[2]
    assert(t[1] == "b" and t[2] == "a")
end

do
    local i = 0
    i, i, i = 1, 2, 3
    assert(i == 1)

    local j = 0
    j, j, j = table.unpack({ 1, 2, 3 })
    assert(j == 1)
end
