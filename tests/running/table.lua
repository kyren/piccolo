function test1()
    local t = {}
    t.i = 1
    t.j = 2
    return t.i == 1 and t.j == 2
end

function test2()
    local t = {}
    t[1] = 1
    t[2] = 2
    t[3] = 3
    return t[1] == 1 and t[2] == 2 and t[3] == 3
end

return
    test1() and
    test2()
