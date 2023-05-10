local function test1()
    local function plus(a, b)
        return a + b
    end
    local function varargs(a, ...)
        return a + plus(...)
    end

    return varargs(1, 2, 3) == 6
end

local function test2()
    local function varargs(a, b, ...)
        local i, j, k, l = a, b, ...
        return i + j + k + l
    end

    return
        varargs(1, 2, 3, 4) == 10 and
        varargs(0, 1, 1, 2, 3, 5) == 4
end

assert(
    test1() and
    test2()
)
