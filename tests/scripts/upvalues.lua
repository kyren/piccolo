local function test1()
    local i = 0
    local function inc(amt)
        i = i + amt
    end
    inc(1)
    inc(2)
    inc(3)
    return i == 6
end

local function test2()
    local i = 0
    local j = 0
    local k = 0
    local function inc(amt)
        i = i + amt
        j = j + 1
        k = 2 + k
    end
    inc(1)
    inc(2)
    inc(3)
    return i + j + k == 15
end

local function test3()
    local i = 0
    local inc
    local function make()
        local k = 4
        local function f()
            i = i + k
        end
        inc = f
    end

    make()
    inc()
    inc()
    inc()
    return i == 12
end

assert(
    test1() and
    test2() and
    test3()
)
