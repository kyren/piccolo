do
    local t = {}
    t.i = 1
    t.j = 2
    assert(t.i == 1 and t.j == 2)
end

do
    local t = {}
    t[1] = 1
    t[2] = 2
    t[3] = 3
    assert(t[1] == 1 and t[2] == 2 and t[3] == 3)
end

do
    local t = {}
    assert(#t == 0)

    t[1] = 1
    assert(#t == 1)

    t[2] = 2
    t[3] = 3
    assert(#t == 3)

    t[4] = 4
    t[5] = 5
    assert(#t == 5)

    t[5] = nil
    assert(#t == 4)

    t[2] = nil
    assert(#t == 4 or #t == 1)

    t[4] = nil
    t[3] = nil
    t[1] = nil
    assert(#t == 0)
end

do
    local t = {}

    -- Make sure the map part of the table has space
    for i = 1,100 do
        t[i .. "str"] = i
    end
    for i = 1,20 do
        t[i .. "str"] = nil
    end

    assert(#t == 0)

    t[1] = 1
    assert(#t == 1)

    t[2] = 2
    t[3] = 3
    assert(#t == 3)

    t[4] = 4
    t[5] = 5
    assert(#t == 5)

    t[5] = nil
    assert(#t == 4)

    t[2] = nil
    assert(#t == 4 or #t == 1)

    t[4] = nil
    t[3] = nil
    t[1] = nil
    assert(#t == 0)
end

do
    local t = {
        1,
        2,
        [3] = 3,
        a = "a"
    }
    assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t.a == "a")
end

do
    local t = {
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
      22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
      58, 59, 60, 61, 62, 63, 64, 65, 66, 67
    }

    assert(#t == 67)
    assert(t[67] == 67)
    assert(t[33] == 33)
    assert(t[31] == 31)
    assert(t[1] == 1)
    assert(t[2] == 2)
end

do
    local t = table.pack(1, 2, 3)
    assert(t[1] == 1)
    assert(t[2] == 2)
    assert(t[3] == 3)

    local x, y, z = table.unpack(t)
    assert(x == 1)
    assert(y == 2)
    assert(z == 3)

    local u, v = table.unpack(t, 2, 3)
    assert(u == 2)
    assert(v == 3)

    assert(table.unpack(t, 1, 1) == 1)
    assert(table.unpack(t, 3, 3) == 3)
    assert(table.unpack(t, 4, 4) == nil)
    assert(table.unpack(t, 4, 2) == nil)
end

do
    local t = {1, 2, 3, a = "a", b = "b"}
    for k,_ in pairs(t) do
        t[k] = nil
    end
    assert(#t == 0 and t.a == nil and t.b == nil)

    local t2 = {[1] = 1, [4] = 4, [7] = 7, a = "a", b = "b"}
    for k,v in pairs(t2) do
        t2[k] = nil
    end
    assert(t2[1] == nil and t2[4] == nil and t2[7] == nil and t.a == nil and t.b == nil)
end
