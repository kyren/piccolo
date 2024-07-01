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
    assert(t.n == 3)

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

local function dump(obj)
    if type(obj) == "table" then
        local s = "{"
        local seq_len = rawlen(obj)
        local actual_len = 0
        for k, v in pairs(obj) do actual_len = actual_len + 1 end
        if seq_len == actual_len then
            local sep = " "
            -- would use ipairs here, but that breaks with the len and index overrides
            for i = 1, seq_len do
                s = s .. sep .. dump(rawget(obj, i))
                sep = ", "
            end
        else
            local sep = " "
            for k, v in pairs(obj) do
                if type(k) ~= "string" then
                    k = "[" .. k .. "]"
                end
                s = s .. sep .. k .. " = " .. dump(v)
                sep = ", "
            end
        end
        s = s .. " }"
        return s
    elseif type(obj) == "string" then
        return '"' .. obj .. '"' -- note: no proper escaping
    else
        return tostring(obj)
    end
end

local primitives = { ["nil"] = 0, number = 0, string = 0, boolean = 0, ["function"] = 0, thread = 0 }

local function cmp_array_recurse(a, b, top)
    if (top == nil or top == true) and debug_enabled then
        print(dump(a))
        print(dump(b))
    end
    local a_ty = type(a)
    local b_ty = type(b)
    if a_ty ~= b_ty then
        return false
    end
    if primitives[a_ty] ~= nil then
        return a == b
    end
    if rawlen(a) ~= rawlen(b) then
      return false
    end
    for i = 1, rawlen(a) do
        if not cmp_array_recurse(rawget(a, i), rawget(b, i), false) then
            return false
        end
    end
    return true
end

do
    local t = { }

    table.insert(t, 1)
    assert(cmp_array_recurse(t, { 1 }))

    table.insert(t, 3)
    assert(cmp_array_recurse(t, { 1, 3 }))

    table.insert(t, 2)
    assert(cmp_array_recurse(t, { 1, 3, 2 }))
end

do
    local t = { }

    table.insert(t, 1, 1)
    assert(cmp_array_recurse(t, { 1 }))

    table.insert(t, 1, 3)
    assert(cmp_array_recurse(t, { 3, 1 }))

    table.insert(t, 1, 2)
    assert(cmp_array_recurse(t, { 2, 3, 1 }))
end

do
    local t = { 1, 2, 3 }

    table.insert(t, 2, nil)
    assert(t[1] == 1 and t[2] == nil and t[3] == 2 and t[4] == 3)
end

do
    local t = { }

    for i = 1, 100 do
        table.insert(t, i)
    end
    assert(#t == 100)

    for i = 100, 1, -1 do
        assert(table.remove(t, i + 1) == nil)
        assert(not pcall(function() table.remove(t, i + 2) end))
        assert(table.remove(t) == i)
    end
    assert(#t == 0)
    assert(table.remove(t) == nil)
    assert(table.remove(t, 0) == nil)
end

do
    local t = { "1", "2", "3", "4", "5" }

    assert(table.remove(t, 3) == "3")
    assert(cmp_array_recurse(t, { "1", "2", "4", "5" }))

    assert(table.remove(t) == "5")
    assert(cmp_array_recurse(t, { "1", "2", "4" }))

    assert(not pcall(function() table.remove(t, 0) end))
    assert(cmp_array_recurse(t, { "1", "2", "4" }))

    assert(table.remove(t, 1) == "1")
    assert(cmp_array_recurse(t, { "2", "4" }))
end
