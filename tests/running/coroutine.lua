function test1()
    local function test_coroutine()
        coroutine.yield(1)
        coroutine.yield(2)
        coroutine.yield(3)
    end

    co = coroutine.create(test_coroutine)

    local e1, r1 = coroutine.resume(co)
    local s1 = coroutine.status(co)
    local e2, r2 = coroutine.resume(co)
    local s2 = coroutine.status(co)
    local e3, r3 = coroutine.resume(co)
    local s3 = coroutine.status(co)
    local e4, r4 = coroutine.resume(co)
    local s4 = coroutine.status(co)

    return
        e1 == true and r1 == 1 and s1 == "suspended" and
        e2 == true and r2 == 2 and s2 == "suspended" and
        e3 == true and r3 == 3 and s3 == "suspended" and
        e4 == true and r4 == nil and s4 == "dead"
end

function test2()
    local function test_coroutine()
        coroutine.yield(1)
        error('test error')
    end

    co = coroutine.create(test_coroutine)

    local e1, r1 = coroutine.resume(co)
    local s1 = coroutine.status(co)
    local e2, r2 = coroutine.resume(co)
    local s2 = coroutine.status(co)

    return
        e1 == true and r1 == 1 and s1 == "suspended" and
        e2 == false and r2 == 'test error' and s2 == "dead"
end

return
    test1() and
    test2()
