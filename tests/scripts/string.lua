function is_err(f)
    return pcall(f) == false
end

function test_concat()
    return
        "a" .. "b" .. "c" == "abc" and
        "a" .. 1 .. "c" == "a1c" and
        1 .. 2 .. 3 == "123"
end

function test_coroutine_len()
    return nil
end

function test_len()
    return
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
        string.len(-2147483648) == 11
end

function test_sub()
    return string.sub("foo", 1) == "foo" and
        string.sub("foo", 2) == "oo" and
        string.sub("hello world", 3, 5) == "llo"
end

assert(
    test_concat() and
    test_len() and
    test_sub()
)
