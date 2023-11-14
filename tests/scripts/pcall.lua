do
    local function error_func(e)
        error(e)
    end
    local function good_func()
        return "good"
    end

    local r1, e1 = pcall(error_func, "test error")
    assert(r1 == false and e1 == "test error")

    local r2, e2 = pcall(error_func, "test error 2")
    assert(r2 == false and e2 == "test error 2")

    local r3, e3 = pcall(good_func)
    assert(r3 == true and e3 == "good")
end

do
    local function test_coroutine()
        coroutine.yield(1)
        coroutine.yield(2)
        coroutine.yield(3)
    end

    co = coroutine.create(function() pcall(test_coroutine) end)

    local e1, r1 = coroutine.resume(co)
    local s1 = coroutine.status(co)
    assert(e1 == true and r1 == 1 and s1 == "suspended")

    local e2, r2 = coroutine.resume(co)
    local s2 = coroutine.status(co)
    assert(e2 == true and r2 == 2 and s2 == "suspended")

    local e3, r3 = coroutine.resume(co)
    local s3 = coroutine.status(co)
    assert(e3 == true and r3 == 3 and s3 == "suspended")

    local e4, r4 = coroutine.resume(co)
    local s4 = coroutine.status(co)
    assert(e4 == true and r4 == nil and s4 == "dead")
end
