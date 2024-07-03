
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
    assert(not pcall(function() table.concat({ true }) end))
    assert(not pcall(function() table.concat({ {} }) end))

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
