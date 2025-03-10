
do
    local i = 0
    local t = setmetatable({}, {
        __concat = function(a, b) i = i + 1 return i .. "(" .. tostring(a) .. tostring(b) .. ")" end,
        __tostring = function(this) i = i + 1 return i end
    })

    assert(t..t..t..t == "6(74(51(23)))")
end

do
    assert("a".."b".."c" == "abc")
end

do
    local t = { "a", "b", "c" }
    assert(table.concat(t) == "abc")
    assert(table.concat(t, ",") == "a,b,c")
end

do
    local t = { }
    assert(table.concat(t) == "")
    assert(table.concat(t, ",") == "")
end

do
    local t = { "a", 2, 3.14 }
    assert(table.concat(t) == "a23.14")
    assert(table.concat(t, ",") == "a,2,3.14")
end

do
    assert(not pcall(function() table.concat({ "a", true }) end))
    assert(not pcall(function() table.concat({ "b", {} }) end))

    local t = setmetatable({ "a" }, { __len = function() return 2 end })
    assert(not pcall(function() table.concat(t) end))
end

do
    local az = "abcdefghijklmnopqrstuvwxyz"
    local t = {}
    for i = 1, #az do
        table.insert(t, string.sub(az, i, i))
    end

    assert(table.concat(t, nil, 24) == "xyz")
    assert(table.concat(t, nil, 3, 5) == "cde")

    assert(table.concat(t, "", 1, #t) == "abcdefghijklmnopqrstuvwxyz")
    assert(table.concat(t, "!", 1, #t) == "a!b!c!d!e!f!g!h!i!j!k!l!m!n!o!p!q!r!s!t!u!v!w!x!y!z")
end

do
    assert(0 .. 1 .. -1 == "01-1")

    assert(-0.1 .. 0.1 == "-0.10.1")

    -- Formatting of floats differs slightly from PRLua
    -- assert(0.1 .. 0.0 .. -0.0 .. -0.1 == "0.10.0-0.0-0.1")
end
