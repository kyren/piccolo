function test1()
    local function test(i, j, k)
        if i then
            return i
        elseif j then
            return j
        else
            return k
        end
    end

    return
        test(1, nil, nil) == 1 and
        test(nil, 2, nil) == 2 and
        test(nil, nil, 3) == 3
end

function test2()
    local function test(a)
        if not a then
            return 1
        else
            return 2
        end
    end

    return
        test(false) == 1 and
        test(true) == 2
end

assert(
    test1() and
    test2()
)
