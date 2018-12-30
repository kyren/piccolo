local function test1()
    local t = {}
    t.i = 1
    t.j = 2
    return t.i == 1 and t.j == 2
end

return test1()
