do
    local succeed_on_0 = pcall(function()
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

    assert(full_forward and partial_forward and not succeed_on_0)
end

do
    local last_element = (function()
        local last, beyond = select(-1, 1, 2, 3, 4)
        return last == 4 and beyond == nil
    end)()

    local before_last_element = (function()
        local eeny, meeny, miney, moe = select(-3, 1, 2, 3, 4)
        return eeny == 2 and meeny == 3 and miney == 4 and moe == nil
    end)()

    local too_far = pcall(function()
        return select(-5, 1, 2, 3, 4)
    end)
    assert(last_element and before_last_element and not too_far)
end
