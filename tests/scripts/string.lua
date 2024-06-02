function is_err(f)
    return pcall(f) == false
end

do
    assert("a" .. "b" .. "c" == "abc")
    assert("a" .. 1 .. "c" == "a1c")
    assert(1 .. 2 .. 3 == "123")
end

do
    function test_coroutine_len()
        return nil
    end

    assert(is_err(function() return string.len(nil) end))
    assert(is_err(function() return string.len(true) end))
    assert(is_err(function() return string.len(false) end))
    assert(is_err(function() return string.len({}) end))
    assert(is_err(function() return string.len(is_err) end))
    assert(is_err(function() return string.len(coroutine.create(test_coroutine_len)) end))
    assert(string.len("") == 0)
    assert(string.len("x") == 1)
    assert(string.len("x\0") == 2)
    assert(string.len(1) == 1)
    assert(string.len(-1) == 2)
    assert(string.len(12) == 2)
    assert(string.len(123) == 3)
    assert(string.len(1.2) == 3)
    assert(string.len(-1.2) == 4)
    assert(string.len(1.23) == 4)
    assert(string.len(2147483647) == 10)
    assert(string.len(-2147483648) == 11)
end

do
    assert(is_err(function() return string.sub(nil) end))
    assert(is_err(function() return string.sub(true, 1) end))
    assert(is_err(function() return string.sub(false) end))
    assert(is_err(function() return string.sub({}) end))
    assert(is_err(function() return string.sub(is_err) end))
    assert(is_err(function() return string.sub(coroutine.create(test_coroutine_len)) end))
    assert(string.sub(48, 1) == "48")
    assert(string.sub(48, 2) == "8")
    assert(string.sub(48, -1) == "8")
    assert(string.sub("hilo", -1, -4) == "")
    assert(string.sub("hilo", -4, 4) == "hilo")
    assert(string.sub("hilo", -4, -3) == "hi")
    assert(string.sub("hilo", -4, -6) == "")
    assert(string.sub("hilo", 0, -4) == "h")
    assert(string.sub(3.4, 1, 2) == "3.")
end
