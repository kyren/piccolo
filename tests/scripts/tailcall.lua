do
    local function inner()
        return 1, 2, 3
    end

    local function outer()
        return inner()
    end

    local a, b, c = outer()
    assert(a == 1 and b == 2 and c == 3)
end

do
    local function inner(...)
        return ...
    end

    local function outer(...)
        return inner(...)
    end

    local a, b, c = outer(1, 2, 3)
    assert(a == 1 and b == 2 and c == 3)
end

do
    local function inner(a, b, c)
        return a, b, c
    end

    local function outer(...)
        local a = 1
        local b = 2
        local c = 3
        return inner(...)
    end

    local a, b, c = outer("a")
    assert(a == "a" and b == nil and c == nil)
end
