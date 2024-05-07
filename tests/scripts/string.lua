function is_err(f)
    return pcall(f) == false
end

do
    assert(
        "a" .. "b" .. "c" == "abc" and
        "a" .. 1 .. "c" == "a1c" and
        1 .. 2 .. 3 == "123")
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
    assert(
        is_err(function() return string.len(nil) end) and
        is_err(function() return string.len(true) end) and
        is_err(function() return string.len(false) end) and
        is_err(function() return string.len({}) end) and
        is_err(function() return string.len(is_err) end) and
        is_err(function() return string.len(coroutine.create(test_coroutine_len)) end) and
        string.len("") == 0 and
        string.len("x") == 1 and
        string.len("x\0") == 2 and
        string.len(1) == 1 and
        string.len(-1) == 2 and
        string.len(12) == 2 and
        string.len(123) == 3 and
        string.len(1.2) == 3 and
        string.len(-1.2) == 4 and
        string.len(1.23) == 4 and
        string.len(2147483647) == 10 and
        string.len(-2147483648) == 11)
end

do
    assert(is_err(function() return string.lower(nil) end) and
        is_err(function() return string.lower(true) end) and
        is_err(function() return string.lower(false) end) and
        is_err(function() return string.lower({}) end) and
        is_err(function() return string.lower(is_err) end) and
        is_err(function() return string.lower(coroutine.create(test_coroutine_len)) end) and
        string.lower("HelLo") == "hello" and
        string.lower("ABCDEFGHIJKLMNOP123QRSTUVWXYZ") == "abcdefghijklmnop123qrstuvwxyz")
    -- for some reason this is accepted in PUC Lua 5.4
    -- and string.lower(80) == "80"
end

do
    assert(string.rep("apple", -32) == "" and
        string.rep("bat", 0) == "" and
        string.rep("say", 3) == "saysaysay" and
        string.rep("say", 3, ", ") == "say, say, say, ")
end

do
    assert(is_err(function() return string.reverse(nil) end) and
        is_err(function() return string.reverse(true) end) and
        is_err(function() return string.reverse(false) end) and
        is_err(function() return string.reverse({}) end) and
        is_err(function() return string.reverse(is_err) end) and
        is_err(function() return string.reverse(coroutine.create(test_coroutine_len)) end) and
        string.reverse("HelLo") == "oLleH" and
        string.reverse("raCecar") == "raceCar")
end

do
    assert(is_err(function() return string.upper(nil) end) and
        is_err(function() return string.upper(true) end) and
        is_err(function() return string.upper(false) end) and
        is_err(function() return string.upper({}) end) and
        is_err(function() return string.upper(is_err) end) and
        is_err(function() return string.upper(coroutine.create(test_coroutine_len)) end) and
        string.upper("HelLo") == "HELLO" and
        string.upper("abcdefghijklmnop123qrstuvwxyz") == "ABCDEFGHIJKLMNOP123QRSTUVWXYZ")
    -- for some reason this is accepted in PUC Lua 5.4
    -- and string.upper(80) == "80"
end
