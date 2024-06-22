function integers()
    local function inc(s, c)
        return c + 1
    end
    return inc, nil, 0
end

function test_generic()
    function test_iterator()
        local function inc(s, c)
            if c == 10 then
                return nil
            else
                return c + 1, c + 11
            end
        end
        return inc, nil, 0
    end

    local sum = 0
    for i in test_iterator() do
        sum = sum + i
    end

    return sum == 55
end

function test_numeric()
    local sum = 0
    for i = 1,10 do
        sum = sum + i
    end
    return sum == 55
end

function test_numeric_closure()
    local closure = {}
    for i = 1,10 do
        closure[i] = function() return i end
    end
    return closure[1]() == 1 and closure[4]() == 4 and closure[9]() == 9
end

function test_generic_closure()
    local closure = {}
    for i in integers() do
        closure[i] = function() return i end
        if i == 10 then
            break
        end
    end
    return closure[1]() == 1 and closure[4]() == 4 and closure[9]() == 9
end

function test_break_scope()
    function integers()
        local function inc(s, c)
            return c + 1
        end
        return inc, nil, 1
    end

    for i = 1,10 do
        if i == 3 then
            break
        end
    end

    for i in integers() do
        if i == 10 then
            break
        end
    end

    for i = 1,10 do
        if i == 3 then
            break
        end
    end

    return true
end

function test_overflow()
    local iters = 0
    for i = math.maxinteger - 32, math.maxinteger, 1 do
        iters = iters + 1
        assert(i > 0)
    end
    assert(iters == 33)

    iters = 0
    for i = 0, 10, math.maxinteger do
        iters = iters + 1
        assert(i == 0)
    end
    assert(iters == 1)

    return true
end

function test_mixed_floats()
    -- if initial and step are ints, the loop will use ints,
    -- even if limit is a float
    local iters = 0
    for i = math.maxinteger - 32, math.maxinteger - 0.5, 1 do
        iters = iters + 1
        assert(iters < 50)
    end
    assert(iters == 33)

    -- loops exit on integer overflow, even if the limit isn't reached
    iters = 0
    for i = math.maxinteger - 4, math.huge, 1 do
        iters = iters + 1
        assert(iters < 50)
    end
    assert(iters == 5)

    return true
end

assert(
    test_generic() and
    test_numeric() and
    test_numeric_closure() and
    test_generic_closure() and
    test_break_scope() and
    test_mixed_floats() and
    test_overflow()
)
