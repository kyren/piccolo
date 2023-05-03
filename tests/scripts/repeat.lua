function test1()
    local sum = 0

    local i = 0
    repeat
        i = i + 1
        sum = sum + i
    until i == 10

    return sum == 55
end

function test2()
    local closure
    local i = 1
    repeat
        if i == 3 then
            break
        end
        local j = i
        closure = function()
            return j
        end
        i = i + 1
    until i == 5

    local bad_upvalue = "wrong"
    return closure() == 2
end

return
    test1() and
    test2()
