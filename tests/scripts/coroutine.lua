do
    local function test_coroutine()
        coroutine.yield(1)
        coroutine.yield(2)
        coroutine.yield(3)
    end

    local co = coroutine.create(test_coroutine)

    local e1, r1 = coroutine.resume(co)
    local s1 = coroutine.status(co)
    local e2, r2 = coroutine.resume(co)
    local s2 = coroutine.status(co)
    local e3, r3 = coroutine.resume(co)
    local s3 = coroutine.status(co)
    local e4, r4 = coroutine.resume(co)
    local s4 = coroutine.status(co)

    assert(
        e1 == true and r1 == 1 and s1 == "suspended" and
        e2 == true and r2 == 2 and s2 == "suspended" and
        e3 == true and r3 == 3 and s3 == "suspended" and
        e4 == true and r4 == nil and s4 == "dead")
end

do
    local function test_coroutine()
        coroutine.yield(1)
        error('test error')
    end

    local co = coroutine.create(test_coroutine)

    local e1, r1 = coroutine.resume(co)
    local s1 = coroutine.status(co)
    local e2, r2 = coroutine.resume(co)
    local s2 = coroutine.status(co)

    assert(
        e1 == true and r1 == 1 and s1 == "suspended" and
        e2 == false and r2 == 'test error' and s2 == "dead")
end

do
    local function test_coroutine(co)
        coroutine.yield(coroutine.status(co))
    end

    local co = coroutine.create(test_coroutine)

    local e, s = coroutine.resume(co, co)

    assert(e == true and s == "running")
end

do
    local function go1()
        coroutine.yield(4)
    end

    local function go2(co1)
        return coroutine.continue(co1)
    end

    local co1 = coroutine.create(go1)
    local co2 = coroutine.create(go2)
    local e, r = coroutine.resume(co2, co1)
    assert(e == true and r == 4)
end

do
    local function go1(co1, co2)
        local a = coroutine.yieldto(co2, co1) .. "1"
        local b = coroutine.yieldto(co2, a) .. "2"
        local c = coroutine.yieldto(co2, b) .. "3"
        return c
    end

    local function go2(co1)
        local a = coroutine.yieldto(co1, "a") .. "b"
        local b = coroutine.yieldto(co1, a) .. "c"
        return coroutine.yieldto(co1, b)
    end

    local co1 = coroutine.create(go1)
    local co2 = coroutine.create(go2)

    local s, r = coroutine.resume(co1, co1, co2)
    assert(s == true and r == "a1b2c3")
end

do
    local co = coroutine.create(function() end)
    coroutine.resume(co)
    assert(coroutine.status(co) == "dead")

    assert(pcall(function()
        coroutine.yieldto(co)
    end) == false)
end
