local function test1()
    local function f()
        return 1, 2, 3
    end
    local i, j, k = f()
    return i + j + k == 6
end

local function test2()
    local function f1()
        return 2, 3
    end
    local function f2(a, b, c)
        return a + b + c
    end
    local i, j = f2(1, f1())

    return i == 6 and j == nil
end

return
    test1() and
    test2()
