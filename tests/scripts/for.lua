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

assert(
    test_generic() and
    test_numeric() and
    test_numeric_closure() and
    test_generic_closure() and
    test_break_scope()
)
