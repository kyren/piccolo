function test1()
    local function error_func(e)
        error(e)
    end
    local function good_func()
        return "good"
    end

    local r1, e1 = pcall(error_func, "test error")
    local r2, e2 = pcall(error_func, "test error 2")
    local r3, e3 = pcall(good_func)

    return
        r1 == false and e1 == "test error" and
        r2 == false and e2 == "test error 2" and
        r3 == true and e3 == "good"
end

return test1()
