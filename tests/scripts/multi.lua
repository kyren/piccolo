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

local function test3()
    local function inner(...)
        local a, b = ...
        return a + b
    end

    return inner(2, 3, 4) == 5
end

local function test4()
    local function inner(a, ...)
        return a + ...
    end

    return inner(2, 3) == 5
end

local function test5()
    local function inner(a, ...)
        return a, ...
    end

    local a, b, c, d
    a, b, c, d = 42, inner(1, 2, 3)
    return a == 42 and b == 1 and c == 2 and d == 3
end

local function test6()
    local function inner(p, ...)
        local a, b, c
        a, b, c = p, ...
        return a, b, c
    end

    local a, b, c
    a, b, c = inner(1, 2, 3)
    return a == 1 and b == 2 and c == 3
end

return
    test1() and
    test2() and
    test3() and
    test4() and
    test5() and
    test6()
