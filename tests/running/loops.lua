function test1()
    local sum = 0
    local i = 1
    while i ~= 10 do
        sum = sum + i
        i = i + 1
    end

    return sum == 45
end

function test2()
    local i = 1
    while true do
        i = i + 1
        if i == 5 then
            break
        end
    end

    return i == 5
end

return
    test1() and
    test2()