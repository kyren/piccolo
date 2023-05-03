function test1()
    local function inner()
        return 1, 2, 3
    end

    local function outer()
        return inner()
    end

    local a, b, c = outer()
    return a == 1 and b == 2 and c == 3
end

function test2()
    local function inner(...)
        return ...
    end

    local function outer(...)
        return inner(...)
    end

    local a, b, c = outer(1, 2, 3)
    return a == 1 and b == 2 and c == 3
end

return
    test1() and
    test2()
