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

do
    assert([[
foo
bar
baz]] == "foo\nbar\nbaz")
end

do
    assert(is_err(function() return string.lower(nil) end))
    assert(is_err(function() return string.lower(true) end))
    assert(is_err(function() return string.lower(false) end))
    assert(is_err(function() return string.lower({}) end))
    assert(is_err(function() return string.lower(is_err) end))
    assert(is_err(function() return string.lower(coroutine.create(test_coroutine_len)) end))
    assert(string.lower("HelLo") == "hello")
    assert(string.lower("ABCDEFGHIJKLMNOP123QRSTUVWXYZ") == "abcdefghijklmnop123qrstuvwxyz")
    assert(string.lower(80) == "80")
    assert(string.lower(3.14) == "3.14")
end

do
    assert(is_err(function() return string.reverse(nil) end))
    assert(is_err(function() return string.reverse(true) end))
    assert(is_err(function() return string.reverse(false) end))
    assert(is_err(function() return string.reverse({}) end))
    assert(is_err(function() return string.reverse(is_err) end))
    assert(is_err(function() return string.reverse(coroutine.create(test_coroutine_len)) end))
    assert(string.reverse("HelLo") == "oLleH")
    assert(string.reverse("raCecar") == "raceCar")
    assert(string.reverse(84) == "48")
    assert(string.reverse(3.14) == "41.3")
end

do
    assert(is_err(function() return string.upper(nil) end))
    assert(is_err(function() return string.upper(true) end))
    assert(is_err(function() return string.upper(false) end))
    assert(is_err(function() return string.upper({}) end))
    assert(is_err(function() return string.upper(is_err) end))
    assert(is_err(function() return string.upper(coroutine.create(test_coroutine_len)) end))
    assert(string.upper("HelLo") == "HELLO")
    assert(string.upper("abcdefghijklmnop123qrstuvwxyz") == "ABCDEFGHIJKLMNOP123QRSTUVWXYZ")
    assert(string.upper(80) == "80")
    assert(string.upper(3.14) == "3.14")
end

do
    assert(is_err(function() return string.byte(nil) end))
    assert(is_err(function() return string.byte(true) end))
    assert(is_err(function() return string.byte(false) end))
    assert(is_err(function() return string.byte({}) end))
    assert(is_err(function() return string.byte(is_err) end))
    assert(is_err(function() return string.byte(coroutine.create(test_coroutine_len)) end))
    assert(string.byte("") == nil)
    assert(string.byte("abcd") == 97)
    assert(string.byte("\xef") == 0xef)
    assert(string.byte("\0") == 0)
    assert(string.byte(1) == string.byte("1"))
    assert(string.byte("abcd", 2) == string.byte("b"))
    assert(string.byte("abcd", -1) == string.byte("d"))

    local b, c, d, e = string.byte("abcd", 2, -1)
    assert(b == string.byte("b") and
        c == string.byte("c") and
        d == string.byte("d") and
        e == nil)
    b, c, d = string.byte("abcd", 2, 3)
    assert(b == string.byte("b") and
        c == string.byte("c") and
        d == nil)
    assert(string.byte("abcd", 2, -5) == nil)
    assert(string.byte("abcd", 3, 1) == nil)
end

do
    assert(is_err(function() return string.char(nil) end))
    assert(is_err(function() return string.char(true) end))
    assert(is_err(function() return string.char(false) end))
    assert(is_err(function() return string.char({}) end))
    assert(is_err(function() return string.char(is_err) end))
    assert(is_err(function() return string.char(coroutine.create(test_coroutine_len)) end))
    assert(string.char() == "")
    assert(string.char(0, "1", 97, 98, 255) == "\0\x01ab\xff")
    assert(is_err(function() return string.char(256) end))
    assert(is_err(function() return string.char(-1) end))
end
