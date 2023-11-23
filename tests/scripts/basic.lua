local function test1()
    local i = 7
    local j = 30
    local i = 42
    return i == 42
end

local function test2()
    local i = 7
    local i = 30
    i = 35
    return i == 35
end

local function test3()
    local function test(a, b)
        return a + b
    end
    local i = test(1, 2)
    return i == 3
end

local function test4()
    local i = 2
    local j = 2
    return (i == j) == true
end

local function test5()
    return
        (1 == 1 and 2 == 2) and
        (1 == 1 or 1 == 2) and
        not (1 == 1 and 2 == 3) and
        not (false or false)
end

local function test6()
    local i = 0
    local function t(p)
        i = i + p
    end
    local _ = (false and t(1))
    local _ = (true and t(2))
    local _ = (false or t(3))
    local _ = (true or t(4))
    return i == 5
end

local function test_short_circuit_large()
    local a = 1
    local b = 2
    local c = 3
    local d = false
    return (a + 2 + (a == 1 and (d or (c + (b + 2))))) == 10
end

local function test_tonumber()
    return tonumber("1") == 1 and 
      tonumber("1.1") == 1.1 and 
      tonumber("0x1A") == 26 and 
      -- TODO
      -- tonumber("1010", 2) == 10 and  -- binary to decimal
      -- tonumber("12", 8) and  -- octal to decimal
      tonumber("foo") == nil
end

assert(
    test1() and
    test2() and
    test3() and
    test4() and
    test5() and
    test6() and
    test_short_circuit_large() and
    test_tonumber()
)
