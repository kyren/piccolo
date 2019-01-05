function test1()
    local inner_function
    do
        local upval = 42
        inner_function = function()
            return upval
        end
    end
    local new_local = "wrong"
    return inner_function() == 42
end

function test2()
    local inner_function
    do
        local upval = 42
        inner_function = function()
            return upval
        end
        goto forward_label
    end
    ::forward_label::
    local new_local = "wrong"
    return inner_function() == 42
end

function test3()
    local inner_function
    do
        local upval = 42
        do
            inner_function = function()
                return upval
            end
            goto forward_label
        end
    end
    ::forward_label::
    local new_local = "wrong"
    return inner_function() == 42
end

function test4()
    local test_val = true
    local inner_function

    ::back_label::
    if test_val then
        do
            local upval = 42
            do
                inner_function = function()
                    return upval
                end
                test_val = false
                goto back_label
            end
        end
    end

    local new_local = "wrong"
    return inner_function() == 42
end

return
    test1() and
    test2() and
    test3() and
    test4()