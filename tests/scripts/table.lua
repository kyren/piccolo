function test1()
    local t = {}
    t.i = 1
    t.j = 2
    return t.i == 1 and t.j == 2
end

function test2()
    local t = {}
    t[1] = 1
    t[2] = 2
    t[3] = 3
    return t[1] == 1 and t[2] == 2 and t[3] == 3
end

function test3()
    local passed = true

    local t = {}
    passed = passed and #t == 0

    t[1] = 1
    passed = passed and #t == 1

    t[2] = 2
    t[3] = 3
    passed = passed and #t == 3

    t[4] = 4
    t[5] = 5
    passed = passed and #t == 5

    t[5] = nil
    passed = passed and #t == 4

    t[2] = nil
    passed = passed and (#t == 4 or #t == 1)

    t[4] = nil
    t[3] = nil
    t[1] = nil
    passed = passed and #t == 0

    return passed
end

function test4()
    local t = {}

    -- Make sure the map part of the table has space
    for i = 1,100 do
        t[i .. "str"] = i
    end
    for i = 1,20 do
        t[i .. "str"] = nil
    end

    local passed = true

    passed = passed and #t == 0

    t[1] = 1
    passed = passed and #t == 1

    t[2] = 2
    t[3] = 3
    passed = passed and #t == 3

    t[4] = 4
    t[5] = 5
    passed = passed and #t == 5

    t[5] = nil
    passed = passed and #t == 4

    t[2] = nil
    passed = passed and (#t == 4 or #t == 1)

    t[4] = nil
    t[3] = nil
    t[1] = nil
    passed = passed and #t == 0

    return passed
end

function test5()
    t = {
        1,
        2,
        [3] = 3,
        a = "a"
    }
    return t[1] == 1 and t[2] == 2 and t[3] == 3 and t.a == "a"
end

assert(
    test1() and
    test2() and
    test3() and
    test4() and
    test5()
)
