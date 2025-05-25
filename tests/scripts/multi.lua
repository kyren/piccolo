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

local function test7()
    local function two(a, b)
        return a, b
    end

    local a, b, c, d
    a, b, c = two(1, 2)
    assert(a == 1 and b == 2 and c == nil)

    a, b, c = 0, two(1, 2), 0
    assert(a == 0 and b == 1 and c == 0)

    a, b, c = (two(1, 2))
    assert(a == 1 and b == nil and c == nil)

    a, b, c, d = 0, (two(1, 2))
    assert(a == 0 and b == 1 and c == nil and d == nil)

    return true
end

local function test8()
    local function ret_all(...)
        return ...
    end
    local function two(a, b)
        return a, b
    end

    local a, b, c, d
    a, b, c = ret_all(two(1, 2))
    assert(a == 1 and b == 2 and c == nil)

    a, b, c = ret_all(0, two(1, 2), 0)
    assert(a == 0 and b == 1 and c == 0)

    a, b, c = ret_all((two(1, 2)))
    assert(a == 1 and b == nil and c == nil)

    a, b, c, d = ret_all(0, (two(1, 2)))
    assert(a == 0 and b == 1 and c == nil and d == nil)

    return true
end

assert(
    test1() and
    test2() and
    test3() and
    test4() and
    test5() and
    test6() and
    test7() and
    test8()
)
