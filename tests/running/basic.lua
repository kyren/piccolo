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

return
    test1() and
    test2() and
    test3() and
    test4() and
    test5() and
    test6()