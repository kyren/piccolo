function test_param_order()
    local i = 1

    local function sub1()
        i = i - 1
        return i
    end

    local function mult3()
        i = i * 3
        return i
    end

    local function eat(...)
    end

    eat(mult3(), 1 + sub1(), mult3(), eat(sub1(), 2 + mult3()), sub1())

    return i == 14
end

assert(
    test_param_order()
)
