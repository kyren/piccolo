do
    local function plus(a, b)
        return a + b
    end
    local function varargs(a, ...)
        return a + plus(...)
    end

    assert(varargs(1, 2, 3) == 6)
end

do
    local function varargs(a, b, ...)
        local i, j, k, l = a, b, ...
        return i + j + k + l
    end

    assert(varargs(1, 2, 3, 4) == 10)
    assert(varargs(0, 1, 1, 2, 3, 5) == 4)
end

do
    local function varargs(...)
        local s = ""
        local t = {...}
        for _, v in ipairs(t) do
            s = s .. tostring(v)
        end
        return s
    end

    assert(varargs(1, 2, 3, 4) == "1234")
    assert(varargs() == "")
end

do
    local function varargs(...)
        local s = ""
        local t = {1, 2, ..., 4, ...}
        for _, v in ipairs(t) do
            s = s .. tostring(v)
        end
        return s
    end

    assert(varargs(3, "x") == "12343x")
    assert(varargs("x") == "12x4x")
end
