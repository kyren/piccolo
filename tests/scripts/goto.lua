function test1()
    local i = 1

    if i == 1 then
        goto last
    end

    i = 2

    ::last::

    return i == 1
end

function test2()
    local closure
    local done = false

    ::start::
    if done then
        local wrong_upval = "wrong"
        return closure() == 42
    end

    local upval = 42
    closure = function()
        return upval
    end
    done = true
    goto start
end

assert(
    test1() and
    test2()
)
