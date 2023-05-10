local function test1()
    local function test1a()
        test1var = 1
    end
    local function test1b()
        return test1var == 1
    end
    test1a()
    return test1b()
end

local function test2()
    _ENV.i = 3
    return _ENV.i == 3 and i == 3
end

assert(
    test1() and
    test2()
)