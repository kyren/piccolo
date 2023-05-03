function test_sum()
    local sum = 0
    local i = 1
    while i ~= 10 do
        sum = sum + i
        i = i + 1
    end

    return sum == 45
end

function test_break()
    local i = 1
    while true do
        i = i + 1
        if i == 5 then
            break
        end
    end

    return i == 5
end

function test_endlabel()
    local i = 1
    while true do
        i = i + 1
        if i == 5 then
            goto testlabel
        end
        if i == 10 then
            break
        end
        ::testlabel::
    end

    return i == 10
end

function test_closure()
    local closure = {}
    local i = 1
    while i ~= 10 do
        local j = i
        closure[i] = function() return j end
        i = i + 1
    end

    return closure[1]() == 1 and closure[4]() == 4 and closure[9]() == 9
end

function test_break_scope()
    local i = 1
    while i ~= 10 do
        i = i + 1
    end

    while i ~= 20 do
        i = i + 1
    end

    return true
end

return
    test_sum() and
    test_break() and
    test_endlabel() and
    test_closure() and
    test_break_scope()
