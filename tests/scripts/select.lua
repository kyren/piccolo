function select_forward()
    local fail_on_0 = pcall(function()
        return select(0)
    end)

    local full_forward = (function()
        local a, b, c, d, e = select(1, 1, 2, 3, 4)
        return a == 1 and b == 2 and c == 3 and d == 4 and e == nil
    end)()

    local partial_forward = (function()
        local c, d, e = select(3, 1, 2, 3, 4)
        return c == 3 and d == 4 and e == nil
    end)()

    return full_forward and partial_forward and not fail_on_0
end

function select_backwards()
    local last_element = (function()
        local last, beyond = select(-1, 1, 2, 3, 4)
        return last == 4 and beyond == nil
    end)()

    local before_last_element = (function()
        local eeny, meeny, miney, moe = select(-3, 1, 2, 3, 4)
        return eeny == 2 and meeny == 3 and miney == 4 and moe == nil
    end)()
    return last_element and before_last_element
end

assert(select_forward() and select_backwards())
