function test1()
    local i = 1
    do
        local i = 2
    end
    return i == 1
end

function test2()
    i = 1
    do
        local i = 2
    end
    return i == 1
end

return
    test1() and
    test2()